-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE GADTs #-}
-- {-# LANGUAGE StandaloneDeriving #-}

module Element (Element(..), Expression(..), eval, expressionToString) where

data Expression = Add          Expression Expression |
                  Subtract     Expression Expression |
                  Multiply     Expression Expression |
                  Divide       Expression Expression |
                  BooleanValue Bool    |
                  DoubleValue  Double  |
                  IntegerValue Integer |
                  StringValue  String  |
                  VarValue     String  |
                  Invalid      String  deriving (Eq, Ord, Show)

expressionToString :: Expression -> String
expressionToString (BooleanValue True) = "true"
expressionToString (BooleanValue False) = "false"
expressionToString (IntegerValue v) = show v
expressionToString (DoubleValue v) = show v
expressionToString (StringValue v) = v
expressionToString (Invalid v) = v
expressionToString e = expressionToString $ eval e


data LangageError = RuntimeError String deriving (Eq, Ord, Show)

lookupVarValue :: String -> Expression
lookupVarValue "x" = StringValue "Hello World!"
lookupVarValue _   = DoubleValue 9.0

noStringInAdd = "String cannot be used in add"
noBooleanInAdd = "Boolean cannot be used in add"

evalM_ :: Expression -> Either LangageError Expression
evalM_ e = let ev = eval e
          in case ev of
               Invalid err -> Left $ RuntimeError err
               _ -> Right ev

eval :: Expression -> Expression
eval v@(BooleanValue _) = v
eval v@(DoubleValue _)  = v
eval v@(IntegerValue _) = v
eval v@(StringValue _)  = v
eval (VarValue n)       = eval $ lookupVarValue n
eval v@(Invalid _)      = v
eval v@(Add _ _) = evalAdd v
eval v@(Subtract _ _) = evalSubtract v
eval v@(Multiply _ _) = evalMultiply v
eval v@(Divide _ _) = evalDivide v

type DoubleBinOp = Double -> Double -> Double
type IntegerBinOp = Integer -> Integer -> Integer

doubleAdd = (+) :: DoubleBinOp
doubleSub = (-) :: DoubleBinOp
doubleMul = (*) :: DoubleBinOp
doubleDiv = (/) :: DoubleBinOp

integerAdd = (+) :: IntegerBinOp
integerSub = (-) :: IntegerBinOp
integerMul = (*) :: IntegerBinOp
integerDiv = div :: IntegerBinOp


evalNumbersWith :: Expression -> Expression -> DoubleBinOp -> IntegerBinOp -> Expression
evalNumbersWith a b fndOp fniOp = evalNumbers a b
                   where evalNumbers i@(Invalid _) _ = i
                         evalNumbers _ i@(Invalid _) = i
                         evalNumbers (StringValue _) _ = Invalid noStringInAdd
                         evalNumbers _ (StringValue _) = Invalid noStringInAdd
                         evalNumbers (BooleanValue _) _ = Invalid noBooleanInAdd
                         evalNumbers _ (BooleanValue _) = Invalid noBooleanInAdd
                         evalNumbers (DoubleValue x)  (DoubleValue y)   = DoubleValue (fndOp x y)
                         evalNumbers (DoubleValue x)  (IntegerValue y)  = DoubleValue (fndOp x (realToFrac y))
                         evalNumbers (IntegerValue x)  (DoubleValue y)  = DoubleValue (fndOp (realToFrac x) y)
                         evalNumbers (IntegerValue x)  (IntegerValue y) = IntegerValue (fniOp x y)
                         evalNumbers x y = evalNumbers (eval x) (eval y)

evalAdd :: Expression -> Expression
evalAdd (Add a b) = evalNumbersWith a b doubleAdd integerAdd

evalSubtract :: Expression -> Expression
evalSubtract (Subtract a b) = evalNumbersWith a b doubleSub integerSub

evalMultiply :: Expression -> Expression
evalMultiply (Multiply a b) = evalNumbersWith a b doubleMul integerMul

evalDivide :: Expression -> Expression
evalDivide (Divide a b) = evalDivideCheckZero a b

evalDivideCheckZero :: Expression -> Expression -> Expression
evalDivideCheckZero (_) (IntegerValue 0) = Invalid "Divide by 0 error"
evalDivideCheckZero (_) (DoubleValue 0.0) = Invalid "Divide by 0 error"
evalDivideCheckZero a b = evalNumbersWith a b doubleDiv integerDiv

data Element = Print Expression    |
               Block [Element] |
               Main Element    |
               Var String Expression
               deriving (Eq, Ord, Show)
