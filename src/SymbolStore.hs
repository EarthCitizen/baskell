{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SymbolStore where

import qualified Data.Map.Strict as M

import Data.Either (isLeft)

import AST (Expression(..))
import Error (LanguageError(..))

type Symbol = String
type VarTable = SymbolTable Symbol Expression

data SymbolTable k v = MutableSymbolTable   { getTable :: M.Map k v }
                     | ImmutableSymbolTable { getTable :: M.Map k v }
                     | SymbolStack { getStack :: [SymbolTable k v] }
                     deriving (Show, Eq)


symbolNotFound :: String -> Either LanguageError Expression
symbolNotFound s = Left $ SymbolNotFoundError $ "Symbol not found: " ++ s

getSymbolValue :: Symbol -> SymbolTable Symbol Expression -> Either LanguageError Expression
getSymbolValue k stc =
    case stc of
        (MutableSymbolTable   { getTable = st }) -> getValue st
        (ImmutableSymbolTable { getTable = st }) -> getValue st
        (SymbolStack { getStack = stk}) ->
            let l = dropWhile isLeft $ fmap (getSymbolValue k) stk
             in case l of
                 [] -> symbolNotFound k
                 (x:xs) -> x
    where getValue st' = case M.lookup k st' of
              Just v  -> return v
              Nothing -> symbolNotFound k

setSymbolValue :: Symbol -> Expression -> SymbolTable Symbol Expression -> Either LanguageError (SymbolTable Symbol Expression)
setSymbolValue k v (MutableSymbolTable   { getTable = st }) =
    return $ MutableSymbolTable $ M.insert k v st
setSymbolValue k v (ImmutableSymbolTable { getTable = st }) =
    case M.member k st of
        True  -> Left $ BuiltInSymbolError $ "Symbol is read-only"
        False -> return $ ImmutableSymbolTable $ M.insert k v st


-- data SymbolStack k v = SymbolStack { getStack :: [SymbolTable k v] }
--                      deriving (Show, Eq)

-- class SymbolStore t e | t -> e where
--     getSymbolValue :: k -> t k v -> Either e v
--     setSymbolValue :: k -> v -> t k v -> Either e (t k v)

-- instance SymbolStore SymbolTable LanguageError where
--     -- getSymbolValue :: Symbol -> SymbolTable Symbol Expression -> Either LanguageError Expression
--     getSymbolValue k stc =
--         case stc of
--             (MutableSymbolTable   { getTable = st }) -> getValue k st
--             (ImmutableSymbolTable { getTable = st }) -> getValue k st
--         where getValue k' st' = case M.lookup k' st' of
--                   Just v  -> return v
--                   Nothing -> Left $ SymbolNotFoundError "Symbol not found"
--
--     setSymbolValue :: Symbol -> Expression -> SymbolTable Symbol Expression -> Either LanguageError (SymbolTable Symbol Expression)
--     setSymbolValue k v (MutableSymbolTable   { getTable = st }) = return $ MutableSymbolTable $ M.insert k v st
--     setSymbolValue k v (ImmutableSymbolTable { getTable = st }) =
--         case M.member k st of
--             True  -> Left $ BuiltInSymbolError $ "Symbol is read-only"
--             False -> return $ ImmutableSymbolTable $ M.insert k v st
