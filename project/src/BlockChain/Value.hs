module BlockChain.Value
  ( Address,
    Value (..),
    Store,
    TxEnv (..),
    mkVMap,
    mapDefault,
    lookupKey,
    insertKey,
    renderValue,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

-- | Addresses are opaque strings (no cryptography in base project).
type Address = String

-- | Runtime values.
-- A 'VMap' carries its own default value (used for missing-key reads),
-- which lets e.g. @balances[unknown]@ reduce to @VInt 0@ for an int-valued map.
data Value
  = VInt !Int
  | VBool !Bool
  | VAddr !Address
  | VMap
      -- | default value returned for missing keys
      !Value
      !(Map Value Value)
  deriving stock (Eq, Show, Ord)

-- | Contract state: a name -> value map.
type Store = Map String Value

-- | Per-transaction environment (immutable reader-ish data, threaded explicitly).
data TxEnv = TxEnv
  { txeSender :: Maybe Address,
    txeParams :: Map String Value
  }

-- | Smart constructor for a map value from an association list.
mkVMap :: Value -> [(Value, Value)] -> Value
mkVMap def kvs = VMap def (Map.fromList kvs)

-- | The default value associated with a map (used for missing-key reads).
mapDefault :: Value -> Value
mapDefault (VMap def _) = def
mapDefault _ = VInt 0 -- defensive; should not be called on non-maps

-- | Look up a key in a value, falling back to the map's default.
lookupKey :: Value -> Value -> Maybe Value
lookupKey (VMap def m) k = Just (Map.findWithDefault def k m)
lookupKey _ _ = Nothing

-- | Insert a key into a map value; returns the original value untouched if not a map.
insertKey :: Value -> Value -> Value -> Value
insertKey (VMap def m) k v = VMap def (Map.insert k v m)
insertKey other _ _ = other

-- | Render a value for display / logs.
renderValue :: Value -> String
renderValue = \case
  VInt i -> show i
  VBool b -> if b then "true" else "false"
  VAddr a -> a
  VMap _ m -> "{" ++ show m ++ "}"
