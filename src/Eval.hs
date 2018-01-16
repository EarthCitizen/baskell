module Eval where

import Control.Monad (join)

import AST
import Debug.Trace
import Error

expressionToString :: Expression -> Either Error String
expressionToString (BooleanValue True)  = Right "true"
expressionToString (BooleanValue False) = Right "false"
expressionToString (IntegerValue v)     = Right $ show v
expressionToString (FloatingValue v)    = Right $ show v
expressionToString (StringValue v)      = Right v
expressionToString e = eval e >>= expressionToString

lookupVarValue :: String -> Expression
lookupVarValue "x" = StringValue "Hello World!"
lookupVarValue _   = FloatingValue 9.0

type DoubleBinOp = BigDecimal -> BigDecimal -> BigDecimal
type IntegerBinOp = Integer -> Integer -> Integer

doubleAdd = (+) :: DoubleBinOp
doubleSub = (-) :: DoubleBinOp
doubleMul = (*) :: DoubleBinOp
doubleDiv = (/) :: DoubleBinOp

integerAdd = (+) :: IntegerBinOp
integerSub = (-) :: IntegerBinOp
integerMul = (*) :: IntegerBinOp
integerDiv = div :: IntegerBinOp

type EvalResult = Either Error Expression

eval :: Expression -> EvalResult
eval v@(BooleanValue _)  = Right $ v
eval v@(FloatingValue _) = Right $ v
eval v@(IntegerValue _)  = Right $ v
eval v@(StringValue _)   = Right $ v
eval (VarValue n)        = eval $ lookupVarValue n
eval v@(Add _ _)         = evalAdd v
eval v@(Subtract _ _)    = evalSubtract v
eval v@(Multiply _ _)    = evalMultiply v
eval v@(Divide _ _)      = evalDivide v

leftDivZeroError = Left  . DivideByZeroError :: String     -> EvalResult
leftTypeMisError = Left  . TypeMismatchError :: String     -> EvalResult
rightFValue      = Right . FloatingValue     :: BigDecimal -> EvalResult
rightIValue      = Right . IntegerValue      :: Integer    -> EvalResult

evalNumbersWith :: Expression -> Expression -> DoubleBinOp -> IntegerBinOp -> EvalResult
-- evalNumbersWith a b fndOp fniOp = join $ evalNumbers <$> (eval a) <*> (eval b)
evalNumbersWith a b fndOp fniOp =
    do
        ea <- eval a
        eb <- eval b
        evalNumbers ea eb
    where evalNumbers :: Expression -> Expression -> EvalResult
          evalNumbers (StringValue _) _  = leftTypeMisError errMsgNoStringInNum
          evalNumbers _ (StringValue _)  = leftTypeMisError errMsgNoStringInNum
          evalNumbers (BooleanValue _) _ = leftTypeMisError errMsgNoBooleanInNum
          evalNumbers _ (BooleanValue _) = leftTypeMisError errMsgNoBooleanInNum
          evalNumbers (FloatingValue x) (FloatingValue y) = rightFValue (fndOp x y)
          evalNumbers (FloatingValue x) (IntegerValue  y) = rightFValue (fndOp x (fromInteger y))
          evalNumbers (IntegerValue  x) (FloatingValue y) = rightFValue (fndOp (fromInteger x) y)
          evalNumbers (IntegerValue  x) (IntegerValue  y) = rightIValue (fniOp x y)

evalAdd :: Expression -> EvalResult
evalAdd (Add a b) = evalNumbersWith a b doubleAdd integerAdd

evalSubtract :: Expression -> EvalResult
evalSubtract (Subtract a b) = evalNumbersWith a b doubleSub integerSub

evalMultiply :: Expression -> EvalResult
evalMultiply (Multiply a b) = evalNumbersWith a b doubleMul integerMul

evalDivide :: Expression -> EvalResult
evalDivide (Divide a b) = evalDivideCheckZero a b

evalDivideCheckZero :: Expression -> Expression -> EvalResult
evalDivideCheckZero (_) (IntegerValue 0)    = leftDivZeroError errMsgDivideByZero
evalDivideCheckZero (_) (FloatingValue 0.0) = leftDivZeroError errMsgDivideByZero
evalDivideCheckZero a b = evalNumbersWith a b doubleDiv integerDiv
