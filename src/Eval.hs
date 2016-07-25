module Eval where

import AST
import Error

expressionToString :: Expression -> String
expressionToString (BooleanValue True) = "true"
expressionToString (BooleanValue False) = "false"
expressionToString (IntegerValue v) = show v
expressionToString (DoubleValue v) = show v
expressionToString (StringValue v) = v
expressionToString (Invalid (DivideByZeroError v)) = v
expressionToString (Invalid (TypeMismatchError v)) = v
expressionToString e = expressionToString $ eval e

lookupVarValue :: String -> Expression
lookupVarValue "x" = StringValue "Hello World!"
lookupVarValue _   = DoubleValue 9.0

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

evalM_ :: Expression -> Either LangageError Expression
evalM_ e = let ev = eval e
          in case ev of
               Invalid err -> Left err
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


evalNumbersWith :: Expression -> Expression -> DoubleBinOp -> IntegerBinOp -> Expression
evalNumbersWith a b fndOp fniOp = evalNumbers a b
    where evalNumbers i@(Invalid _) _ = i
          evalNumbers _ i@(Invalid _) = i
          evalNumbers (StringValue _) _ = Invalid $ TypeMismatchError errMsgNoStringInNum
          evalNumbers _ (StringValue _) = Invalid $ TypeMismatchError errMsgNoStringInNum
          evalNumbers (BooleanValue _) _ = Invalid $ TypeMismatchError errMsgNoBooleanInNum
          evalNumbers _ (BooleanValue _) = Invalid $ TypeMismatchError errMsgNoBooleanInNum
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
evalDivideCheckZero (_) (IntegerValue 0) = Invalid $ DivideByZeroError errMsgDivideByZero
evalDivideCheckZero (_) (DoubleValue 0.0) = Invalid $ DivideByZeroError errMsgDivideByZero
evalDivideCheckZero a b = evalNumbersWith a b doubleDiv integerDiv
