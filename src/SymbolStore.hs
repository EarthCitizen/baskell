module SymbolStore where

import qualified Data.Map.Strict as M

import Error (LanguageError(..))

data SymbolTable k v = MutableSymbolTable   { getTable :: M.Map k v }
                     | ImmutableSymbolTable { getTable :: M.Map k v }
                     deriving (Show, Eq)

data SymbolStack k v = SymbolStack { getStack :: [SymbolTable k v] }
                     deriving (Show, Eq)

class SymbolStore t where
    getSymbolValue :: Ord k => k -> t k v -> Maybe v
    setSymbolValue :: Ord k => k -> v -> t k v -> Either LanguageError (t k v)

instance SymbolStore SymbolTable where
    getSymbolValue k (MutableSymbolTable   { getTable = st }) = M.lookup k st
    getSymbolValue k (ImmutableSymbolTable { getTable = st }) = M.lookup k st

    setSymbolValue k v (MutableSymbolTable   { getTable = st }) = Right $ MutableSymbolTable $ M.insert k v st
    setSymbolValue k v (ImmutableSymbolTable { getTable = st }) =
        case M.member k st of
            True  -> Left $ BuiltInSymbolError $ "Symbol not found"
            False -> Right $ ImmutableSymbolTable $ M.insert k v st
