-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE GADTs #-}
-- {-# LANGUAGE StandaloneDeriving #-}

module Element (Element(..), Expression(..)) where

import Error

data Expression = Add          Expression Expression |
                  Subtract     Expression Expression |
                  Multiply     Expression Expression |
                  Divide       Expression Expression |
                  BooleanValue Bool    |
                  DoubleValue  Double  |
                  IntegerValue Integer |
                  StringValue  String  |
                  VarValue     String  |
                  Invalid      LangageError deriving (Eq, Ord, Show)

data IfBranch = ElseIf Expression Element IfBranch |
                Else Element |
                EndIf
                deriving (Eq, Ord, Show)

data Element = Print Expression |
               Block [Element]  |
               Main Element     |
               Var String Expression  |
               If Expression Element IfBranch
               deriving (Eq, Ord, Show)
