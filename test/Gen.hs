import Test.QuickCheck
import Control.Monad (liftM)
import AST

newtype MathExpr = MathExpr Expression

makeGenIntegerValue :: Integer -> Gen Expression
makeGenIntegerValue x = return $ IntegerValue x

makeAddGen :: Integer -> Gen Expression
makeAddGen 0 = return $ Add (IntegerValue 0) (IntegerValue 0)
makeAddGen total = do
    subtrahend <- choose (0, total)
    Add <$> (genMathExpr $ total - subtrahend) <*> (genMathExpr subtrahend)

makeSubtractGen :: Integer -> Gen Expression
makeSubtractGen 0 = return $ Subtract (IntegerValue 0) (IntegerValue 0)
makeSubtractGen total = do
    addend <- choose (0, total)
    Subtract <$> (genMathExpr $ total + addend) <*> (genMathExpr addend)

-- makeSubtractGen :: Integer -> Gen Expression
-- makeSubtractGen 0 = return $ Subtract (IntegerValue 0) (IntegerValue 0)
-- makeSubtractGen total = do
--     addend <- choose (0, total)
--     Subtract <$> (genMathExpr $ total + addend) <*> (genMathExpr addend)

genSubtract :: Integer -> Gen Expression
genSubtract total = undefined

genMultiply :: Integer -> Gen Expression
genMultiply total = undefined

genDivide :: Integer -> Gen Expression
genDivide total = undefined

genMathExpr :: Integer -> Gen Expression
genMathExpr total = oneof [makeGenIntegerValue total, makeAddGen total, makeSubtractGen total]

exprToString :: Expression -> String
exprToString (IntegerValue a) = show a
exprToString (Subtract a b) = "(" ++ exprToString a ++ "-" ++ exprToString b ++ ")"
exprToString (Add a b) = "(" ++ exprToString a ++ "+" ++ exprToString b ++ ")"

main = (generate $ makeSubtractGen (999)) >>= return . exprToString >>= print

-- makeIntegerOr :: Integer -> (Integer -> Gen Expression) -> Gen Expression
-- makeIntegerOr nextTotal otherGen = oneof [makeGenIntegerValue nextTotal, otherGen nextTotal]

-- genAdd :: Integer -> Gen Expression
-- genAdd = makeAddGen
--     where makeAddGen :: Integer -> Gen Expression
--           makeAddGen 0 = return $ Add (IntegerValue 0) (IntegerValue 0)
--           makeAddGen nextTotal = do
--               subtrahend <- choose (0, nextTotal)
--               Add <$> (makeIntegerOrAdd $ nextTotal - subtrahend) <*> (makeIntegerOrAdd subtrahend)
--
--           makeIntegerOrAdd :: Integer -> Gen Expression
--           makeIntegerOrAdd nextTotal = makeIntegerOr nextTotal $ makeAddGen
--
--           genAdd :: Integer -> Gen Expression
--           genAdd = makeAddGen
