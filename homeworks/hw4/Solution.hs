module Solution where

newtype Reader r a = Reader {runReader :: r -> a}

instance Functor (Reader r) where
  fmap f ra = Reader (f . runReader ra)

instance Applicative (Reader r) where
  pure x = Reader (const x)
  liftA2 f ra rb = Reader (\env -> f (runReader ra env) (runReader rb env))

instance Monad (Reader r) where
  ra >>= f = Reader (\env -> runReader (f (runReader ra env)) env)

ask :: Reader r r
ask = Reader id

asks :: (r -> a) -> Reader r a
asks = Reader

local :: (r -> r) -> Reader r a -> Reader r a
local f ra = Reader (runReader ra . f)

data BankConfig = BankConfig
  { interestRate :: Double,
    transactionFee :: Int,
    minimumBalance :: Int
  }
  deriving (Show)

data Account = Account
  { accountId :: String,
    balance :: Int
  }
  deriving (Show)

calculateInterest :: Account -> Reader BankConfig Int
calculateInterest acc = do
  rate <- asks interestRate
  return (floor (fromIntegral (balance acc) * rate))

applyTransactionFee :: Account -> Reader BankConfig Account
applyTransactionFee acc = do
  fee <- asks transactionFee
  return acc {balance = balance acc - fee}

checkMinimumBalance :: Account -> Reader BankConfig Bool
checkMinimumBalance acc = do
  minB <- asks minimumBalance
  return (balance acc >= minB)

processAccount :: Account -> Reader BankConfig (Account, Int, Bool)
processAccount acc = do
  acc' <- applyTransactionFee acc
  i <- calculateInterest acc
  ok <- checkMinimumBalance acc
  return (acc', i, ok)
