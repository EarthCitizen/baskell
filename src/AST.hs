-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE GADTs #-}
-- {-# LANGUAGE StandaloneDeriving #-}

module AST (AST(..), Main(..),  Block(..), Statement(..), Expression(..), BigDecimal, BigFloat, Prec50) where

import Data.Number.BigFloat (BigFloat, Prec50)
import Error

type BigDecimal = BigFloat Prec50

data Expression = Add           Expression Expression |
                  Subtract      Expression Expression |
                  Multiply      Expression Expression |
                  Divide        Expression Expression |
                  BooleanValue  Bool       |
                  FloatingValue BigDecimal |
                  IntegerValue  Integer    |
                  StringValue   String     |
                  VarValue      String     deriving (Eq, Ord, Show)

-- data Else = ElseIf Expression Block Else |
--             Else Block |
--             EndIf
--             deriving (Eq, Ord, Show)
--
-- data If = If Expression Block Else deriving (Eq, Ord, Show)

data ExprResult = ExprResBoolean Bool       |
                  ExprResDouble  BigDecimal |
                  ExprResInteger Integer    |
                  ExprResInvalid Error  deriving (Eq, Ord, Show)

data Statement = PrintStatement  Expression        |
                 VarStatement    String Expression |
                 IfStatement     Expression Block Statement |
                 NoOp
                 deriving (Eq, Ord, Show)

data Block = Block [Statement] deriving (Eq, Ord, Show)

data Main = Main Block deriving (Eq, Ord, Show)

data AST = AST Main deriving (Eq, Ord, Show)
