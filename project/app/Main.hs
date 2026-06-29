module Main (main) where

import BlockChain.Error (VMError (..), renderChainError, renderParseError)
import BlockChain.Ledger
  ( Block (..),
    BlockResult (..),
    Chain (..),
    SubmittedTx (..),
    TxLog (..),
    initChain,
    submitBlock,
  )
import BlockChain.Parser (parseProgram)
import BlockChain.Syntax (Contract (..))
import BlockChain.Value (Address, Value (..))
import Control.Exception (IOException, try)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (intercalate, stripPrefix)
import Data.Map.Strict qualified as M
import System.Environment (getArgs)
import System.Exit (exitFailure)

------------------------------------------------------------
-- Entry
------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case args of
    [contractPath, scenarioPath] -> runWith contractPath scenarioPath
    _ -> do
      putStrLn "usage: project-exe <contract.bcl> <scenario.txt>"
      exitFailure

runWith :: FilePath -> FilePath -> IO ()
runWith contractPath scenarioPath = do
  -- 1) Read + parse the contract.
  src <- readFileSafe contractPath
  contract <- case parseProgram src of
    Left e -> do putStrLn (renderParseError e); exitFailure
    Right c -> pure c

  -- 2) Initialise the chain (deploys with the first scenario sender as deployer).
  scenario <- readFileSafe scenarioPath
  let deployer = firstSender scenario
  chain0 <- case initChain contract deployer of
    Left e -> do putStrLn (renderChainError e); exitFailure
    Right c -> pure c

  putStrLn $ "contract: " ++ cName contract
  putStrLn $ "deployer: " ++ deployer
  putStrLn ""
  putStrLn "initial state:"
  printStore (cStore chain0)
  putStrLn ""

  -- 3) Walk the scenario, accumulating the chain.
  chainRef <- newIORef chain0
  processScenario chainRef (lines scenario)

  -- 4) Print final state.
  finalChain <- readIORef chainRef
  putStrLn ""
  putStrLn "final state:"
  printStore (cStore finalChain)

------------------------------------------------------------
-- Scenario processing
------------------------------------------------------------

processScenario :: IORef Chain -> [String] -> IO ()
processScenario _ [] = pure ()
processScenario chainRef (l0 : rest0)
  | isComment l0 || null (words l0) = processScenario chainRef rest0
  | Just rest' <- stripPrefix "block " (dropWhile (== ' ') l0) =
      runBlock chainRef (read (takeWhile isDigit rest')) [] rest0
  | otherwise = do
      putStrLn $ "ignoring unknown scenario line: " ++ l0
      processScenario chainRef rest0
  where
    isDigit c = c >= '0' && c <= '9'

    -- Collect txs until the closing "end", then submit.
    runBlock :: IORef Chain -> Int -> [SubmittedTx] -> [String] -> IO ()
    runBlock ref parent txs [] = build ref parent (reverse txs)
    runBlock ref parent txs (ln : rest)
      | isComment ln || null (words ln) = runBlock ref parent txs rest
      | (cmd : _) <- words ln,
        cmd == "end" = do
          build ref parent (reverse txs)
          processScenario ref rest
      | Just tx <- parseTxLine ln = runBlock ref parent (tx : txs) rest
      | otherwise = do
          putStrLn $ "bad tx line: " ++ ln
          runBlock ref parent txs rest

    -- Submit a block to the chain referenced by the IORef.
    build :: IORef Chain -> Int -> [SubmittedTx] -> IO ()
    build ref parent txs = do
      ch <- readIORef ref
      let block = Block (Just parent) txs
      case submitBlock ch block of
        Left e -> do
          putStrLn $
            "BLOCK REJECTED (parent="
              ++ show parent
              ++ "): "
              ++ renderChainError e
        Right (ch', br) -> do
          putStrLn $
            "block id="
              ++ show (brBlockId br)
              ++ " parent="
              ++ show parent
              ++ " txs="
              ++ show (length txs)
          mapM_ printLog (brLog br)
          modifyIORef' ref (const ch')

printLog :: TxLog -> IO ()
printLog (TxLog name sender res) =
  putStrLn $
    "  "
      ++ name
      ++ " from="
      ++ sender
      ++ " -> "
      ++ resultStr res
  where
    resultStr (Right ()) = "COMMITTED"
    resultStr (Left VMRevert {}) = "REVERTED (require failed)"
    resultStr (Left e) = "REVERTED (" ++ show e ++ ")"

isComment :: String -> Bool
isComment s = case dropWhile (== ' ') s of
  ('#' : _) -> True
  _ -> False

------------------------------------------------------------
-- Line parsing
------------------------------------------------------------

-- | Parse a single "tx name sender k=v ..." line.
parseTxLine :: String -> Maybe SubmittedTx
parseTxLine l = case words l of
  ("tx" : name : sender : argParts) ->
    Just (SubmittedTx name sender (parseArgs argParts))
  _ -> Nothing

parseArgs :: [String] -> [(String, Value)]
parseArgs = foldr step []
  where
    step kv acc = case break (== '=') kv of
      (k, '=' : v) -> (k, parseVal v) : acc
      _ -> acc

parseVal :: String -> Value
parseVal s = case s of
  "true" -> VBool True
  "false" -> VBool False
  _
    | all (\c -> c >= '0' && c <= '9') s -> VInt (read s)
    | otherwise -> VAddr s

------------------------------------------------------------
-- Misc
------------------------------------------------------------

-- | First 'sender' encountered in the scenario (used as contract deployer).
firstSender :: String -> Address
firstSender src =
  case [sender | l <- lines src, not (isComment l), ("tx" : _ : sender : _) <- [words l]] of
    (s : _) -> s
    [] -> "deployer"

readFileSafe :: FilePath -> IO String
readFileSafe p = do
  e <- try (readFile p)
  case e of
    Right s -> pure s
    Left (_ :: IOException) -> do
      putStrLn $ "cannot read file: " ++ p
      exitFailure

-- | Pretty-print a store for the demo.
printStore :: M.Map String Value -> IO ()
printStore st = do
  let kvs = M.toList st
  mapM_ (\(k, v) -> putStrLn $ "  " ++ k ++ " = " ++ renderVal v) kvs
  where
    renderVal (VInt i) = show i
    renderVal (VBool b) = if b then "true" else "false"
    renderVal (VAddr a) = a
    renderVal (VMap _ m) =
      "{" ++ intercalate ", " [renderVal k ++ " -> " ++ renderVal v | (k, v) <- M.toList m] ++ "}"
