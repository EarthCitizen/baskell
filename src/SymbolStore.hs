{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module SymbolStore (SymbolStore(..), MutableSymbolTable(..), ImmutableSymbolTable(..), SymbolStack(..), Symbol) where

import qualified Data.Map.Strict as M

import Data.Either (isLeft)

import AST (Expression(..))
import Error (Error(..))

type Symbol = String

data MutableSymbolTable k v = MutableSymbolTable   { getMutMap :: M.Map k v } deriving (Show, Eq)
data ImmutableSymbolTable k v = ImmutableSymbolTable { getImmutMap :: M.Map k v } deriving (Show, Eq)
data SymbolStack k v = SymbolStack { getStack :: [MutableSymbolTable k v] } deriving (Show, Eq)

class SymbolStore t e | t -> e where
    hasSymbol :: Symbol -> t Symbol v -> Bool
    getSymbolValue :: Symbol -> t Symbol v -> Either e v
    setSymbolValue :: Symbol -> v -> t Symbol v -> Either e (t Symbol v)

instance SymbolStore MutableSymbolTable Error where
    hasSymbol k t = M.member k (getMutMap t)
    getSymbolValue k t = let m = getMutMap t
                          in case M.lookup k m of
                              Nothing -> Left $ SymbolNotFoundError k
                              Just v  -> Right $ v
    setSymbolValue k v t = Right $ MutableSymbolTable $ M.insert k v (getMutMap t)

instance SymbolStore ImmutableSymbolTable Error where
    hasSymbol k t = M.member k (getImmutMap t)
    getSymbolValue k t = let m = getImmutMap t
                          in case M.lookup k m of
                              Nothing -> Left $ SymbolNotFoundError k
                              Just v  -> Right $ v
    setSymbolValue k v t = Left $ ReadOnlySymbolError k

-- type VarTable = SymbolTable Symbol Expression

-- data SymbolTable k v = MutableSymbolTable   { getTable :: M.Map k v }
--                      | ImmutableSymbolTable { getTable :: M.Map k v }
--                      | SymbolStack { getStack :: [SymbolTable k v] }
--                      | RuntimeSymbols {
--                      deriving (Show, Eq)

-- data RuntimeSymbols k v = { builtIns :: ImmutableSymbolTable k v,

-- symbolNotFound :: String -> Either LanguageError Expression
-- symbolNotFound s = Left $ SymbolNotFoundError $ "Symbol not found: " ++ s

-- getSymbolValue :: Symbol -> SymbolTable Symbol Expression -> Either LanguageError Expression
-- getSymbolValue k stc =
--     case stc of
--         (MutableSymbolTable   { getTable = st }) -> getValue st
--         (ImmutableSymbolTable { getTable = st }) -> getValue st
--         (SymbolStack { getStack = stk}) ->
--             let l = dropWhile isLeft $ fmap (getSymbolValue k) stk
--              in case l of
--                  [] -> symbolNotFound k
--                  (x:xs) -> x
--     where getValue st' = case M.lookup k st' of
--               Just v  -> return v
--               Nothing -> symbolNotFound k
--
-- setSymbolValue :: Symbol -> Expression -> SymbolTable Symbol Expression -> Either LanguageError (SymbolTable Symbol Expression)
-- setSymbolValue k v (MutableSymbolTable   { getTable = st }) =
--     return $ MutableSymbolTable $ M.insert k v st
-- setSymbolValue k v (ImmutableSymbolTable { getTable = st }) =
--     case M.member k st of
--         True  -> Left $ ReadOnlySymbolError $ "Symbol is read-only"
--         False -> Left $ mkReadOnlyTableError -- ImmutableSymbolTable $ M.insert k v st


-- data SymbolStack k v = SymbolStack { getStack :: [SymbolTable k v] }
--                      deriving (Show, Eq)


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
--             True  -> Left $ ReadOnlySymbolError $ "Symbol is read-only"
--             False -> return $ ImmutableSymbolTable $ M.insert k v st
