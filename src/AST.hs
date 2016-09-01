-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE GADTs #-}
-- {-# LANGUAGE StandaloneDeriving #-}

module AST (AST(..), Main(..),  Block(..), Statement(..), If(..), Else(..), Expression(..)) where

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

data ExprResult = ExprResBoolean Bool    |
                  ExprResDouble  Double  |
                  ExprResInteger Integer |
                  ExprResInvalid LangageError  deriving (Eq, Ord, Show)


data Else = ElseIf Expression Block Else |
            Else Block |
            EndIf
            deriving (Eq, Ord, Show)

data If = If Expression Block Else deriving (Eq, Ord, Show)

data Statement = PrintStatement Expression      |
                 VarStatement String Expression |
                 IfStatement If
                 deriving (Eq, Ord, Show)

data Block = Block [Statement] deriving (Eq, Ord, Show)

data Main = Main Block deriving (Eq, Ord, Show)

data AST = AST Main deriving (Eq, Ord, Show)
