module BlockChain.Parser
  ( parseContract,
    parseExpr,
    parseStmt,
    parseProgram,
    Parser,
  )
where

import BlockChain.Error (ParseError (..), Pos (..), advancePos, posStart)
import BlockChain.Syntax
import Control.Applicative (Alternative (..))
import Control.Monad (void, when)
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)

------------------------------------------------------------
-- Parser state + monad
------------------------------------------------------------

data PState = PState {psInput :: !String, psPos :: !Pos}

newtype Parser a = P {runP :: PState -> Either ParseError (a, PState)}

instance Functor Parser where
  fmap f (P p) = P $ \s -> fmap (\(a, s') -> (f a, s')) (p s)

instance Applicative Parser where
  pure a = P (\s -> Right (a, s))
  P pf <*> P px = P $ \s -> case pf s of
    Left e -> Left e
    Right (f, s') -> case px s' of
      Left e -> Left e
      Right (x, s'') -> Right (f x, s'')

instance Monad Parser where
  P p >>= f = P $ \s -> case p s of
    Left e -> Left e
    Right (a, s') -> runP (f a) s'

-- | Alternative: left-biased, always backtracks fully (restores input on failure).
instance Alternative Parser where
  empty = P (\s -> Left (ParseError (psPos s) "no parse"))
  P p <|> P q = P $ \s -> case p s of
    Right r -> Right r
    Left e -> case q s of
      Right r -> Right r
      Left _ -> Left e -- keep left's error

------------------------------------------------------------
-- Primitive combinators
------------------------------------------------------------

failAt :: String -> Parser a
failAt msg = P (\s -> Left (ParseError (psPos s) msg))

-- | Consume one char satisfying a predicate; does not advance on failure.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = P $ \s -> case psInput s of
  [] -> Left (ParseError (psPos s) "unexpected end of input")
  (c : cs) ->
    if p c
      then Right (c, PState cs (advancePos c (psPos s)))
      else
        Left
          ( ParseError
              (psPos s)
              ("unexpected character " ++ show c)
          )

char :: Char -> Parser Char
char c = satisfy (== c) <|> failAt ("expected " ++ show c)

string :: String -> Parser String
string [] = pure ""
string (c : cs) = (:) <$> char c <*> string cs

------------------------------------------------------------
-- Whitespace + comments
------------------------------------------------------------

whitespace :: Parser ()
whitespace = P $ \s -> Right ((), skipWS s)
  where
    skipWS st =
      case psInput st of
        [] -> st
        ('/' : '/' : rest) -> skipLine rest (advancePos '/' (advancePos '/' (psPos st)))
        ('/' : '*' : rest) -> skipBlock rest (advancePos '/' (advancePos '/' (psPos st)))
        (c : cs)
          | isSpace c -> skipWS (PState cs (advancePos c (psPos st)))
          | otherwise -> st

    skipLine [] p = PState [] p
    skipLine (c : cs) p
      | c == '\n' = skipWS (PState cs (advancePos c p))
      | otherwise = skipLine cs (advancePos c p)

    skipBlock [] p = PState [] p
    skipBlock ('*' : '/' : rest) p =
      skipWS (PState rest (advancePos '/' (advancePos '*' p)))
    skipBlock (c : cs) p = skipBlock cs (advancePos c p)

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

symbol :: String -> Parser String
symbol s = lexeme (string s)

sym :: String -> Parser ()
sym s = void (symbol s) <|> failAt ("expected " ++ show s)

parens :: Parser a -> Parser a
parens p = sym "(" *> p <* sym ")"

braces :: Parser a -> Parser a
braces p = sym "{" *> p <* sym "}"

brackets :: Parser a -> Parser a
brackets p = sym "[" *> p <* sym "]"

------------------------------------------------------------
-- Tokens
------------------------------------------------------------

reserved :: [String]
reserved =
  [ "contract",
    "state",
    "transaction",
    "map",
    "int",
    "bool",
    "address",
    "empty",
    "sender",
    "require",
    "if",
    "else",
    "true",
    "false"
  ]

isIdentStart :: Char -> Bool
isIdentStart c = isAlpha c || c == '_'

isIdentRest :: Char -> Bool
isIdentRest c = isAlphaNum c || c == '_'

identifier :: Parser String
identifier = lexeme $ do
  c <- satisfy isIdentStart
  cs <- many (satisfy isIdentRest)
  let name = c : cs
  when (name `elem` reserved) $ failAt ("reserved word: " ++ name)
  pure name

intLiteral :: Parser Int
intLiteral = lexeme $ do
  ds <- some (satisfy isDigit)
  pure (read ds)

keyword :: String -> Parser ()
keyword kw = lexeme $ do
  s <- some (satisfy isIdentRest)
  if s == kw then pure () else failAt ("expected keyword " ++ show kw)

------------------------------------------------------------
-- Types
------------------------------------------------------------

parseType :: Parser Type
parseType =
  (keyword "int" *> pure TInt)
    <|> (keyword "bool" *> pure TBool)
    <|> (keyword "address" *> pure TAddress)
    <|> ( do
            keyword "map"
            sym "<"
            k <- parseType
            sym ","
            v <- parseType
            sym ">"
            pure (TMap k v)
        )
    <|> failAt "expected a type"

------------------------------------------------------------
-- Expressions
------------------------------------------------------------

parseLiteral :: Parser Literal
parseLiteral =
  (keyword "true" *> pure (LBool True))
    <|> (keyword "false" *> pure (LBool False))
    <|> (keyword "empty" *> pure LEmpty)
    <|> (LInt <$> intLiteral)

parsePrimary :: Parser Expr
parsePrimary =
  (EVar <$> identifier)
    <|> (ELit <$> parseLiteral)
    <|> (keyword "sender" *> pure ESender)
    <|> (parens parseExpr)

parsePostfix :: Parser Expr
parsePostfix = do
  e <- parsePrimary
  rest e
  where
    rest e =
      ( do
          k <- brackets parseExpr
          rest (EIndex e k)
      )
        <|> pure e

parseUnary :: Parser Expr
parseUnary =
  (do sym "-"; e <- parseUnary; pure (EUnary UNeg e))
    <|> (do sym "!"; e <- parseUnary; pure (EUnary UNot e))
    <|> parsePostfix

mkBin :: Op -> Expr -> Expr -> Expr
mkBin = EBinOp

parseMul :: Parser Expr
parseMul =
  chainl1 parseUnary $
    (sym "*" *> pure (mkBin OMul))
      <|> (sym "/" *> pure (mkBin ODiv))

parseAdd :: Parser Expr
parseAdd =
  chainl1 parseMul $
    (sym "+" *> pure (mkBin OAdd))
      <|> (sym "-" *> pure (mkBin OSub))

parseCmp :: Parser Expr
parseCmp =
  chainl1 parseAdd $
    (sym "<=" *> pure (mkBin OLe))
      <|> (sym ">=" *> pure (mkBin OGe))
      <|> (sym "<" *> pure (mkBin OLt))
      <|> (sym ">" *> pure (mkBin OGt))

parseEq :: Parser Expr
parseEq =
  chainl1 parseCmp $
    (sym "==" *> pure (mkBin OEq))
      <|> (sym "!=" *> pure (mkBin ONe))

parseAnd :: Parser Expr
parseAnd = chainl1 parseEq (sym "&&" *> pure (mkBin OAnd))

parseOr :: Parser Expr
parseOr = chainl1 parseAnd (sym "||" *> pure (mkBin OOr))

parseExpr :: Parser Expr
parseExpr = parseOr

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= rest
  where
    rest x = (do f <- op; y <- p; rest (f x y)) <|> pure x

------------------------------------------------------------
-- Statements
------------------------------------------------------------

parseStmt :: Parser Stmt
parseStmt =
  ( do
      keyword "require"
      e <- parseExpr
      sym ";"
      pure (SRequire e)
  )
    <|> ( do
            keyword "if"
            c <- parseExpr
            ts <- braces (many parseStmt)
            ( do
                keyword "else"
                fs <- braces (many parseStmt)
                pure (SIf c ts fs)
              )
              <|> pure (SIf c ts [])
        )
    <|> ( do
            lhs <- parsePostfix -- allow map[k] := ...
            sym ":="
            rhs <- parseExpr
            sym ";"
            pure (SAssign lhs rhs)
        )

------------------------------------------------------------
-- Declarations
------------------------------------------------------------

parseParamList :: Parser [(Name, Type)]
parseParamList =
  ( do
      n <- identifier
      sym ":"
      t <- parseType
      rest [(n, t)]
  )
    <|> pure []
  where
    rest acc =
      ( do
          sym ","
          n <- identifier
          sym ":"
          t <- parseType
          rest (acc ++ [(n, t)])
      )
        <|> pure acc

parseStateVar :: Parser StateVar
parseStateVar = do
  n <- identifier
  sym ":"
  t <- parseType
  sym "="
  e <- parseExpr
  sym ";"
  pure (StateVar n t e)

parseTxDef :: Parser TxDef
parseTxDef = do
  keyword "transaction"
  n <- identifier
  ps <- parens parseParamList
  body <- braces (many parseStmt)
  pure (TxDef n ps body)

parseContract :: Parser Contract
parseContract = do
  whitespace
  keyword "contract"
  name <- identifier
  (svs, txs) <- braces $ do
    keyword "state"
    svs <- braces (many parseStateVar)
    txs <- many parseTxDef
    pure (svs, txs)
  pure (Contract name svs txs)

------------------------------------------------------------
-- Entry points
------------------------------------------------------------

eofP :: Parser ()
eofP = P $ \s -> case psInput s of
  [] -> Right ((), s)
  _ -> Left (ParseError (psPos s) "expected end of input")

-- | Parse a contract from source text. Returns 'Left' on syntax error.
parseProgram :: String -> Either ParseError Contract
parseProgram input =
  case runP (parseContract <* eofP) (PState input posStart) of
    Right (c, _) -> Right c
    Left e -> Left e
