{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

import System.Environment (getArgs)
import Test.QuickCheck
import Control.Monad (liftM)
import AST
import Text.Read (readMaybe)

import Debug.Trace

data TotalInput = IntegerTotal Integer | DoubleTotal Double deriving (Eq, Show)

-- Strict, otherwise the list will be rebuilt each reference
tenkPrimes :: [Integer]
!tenkPrimes = [1, 2, 3, 5, 7, 9, 11, 13] ++ [ x | x <- [14..10000], (not.any ((0 ==).(rem x))) [2..(x-1)]]


newtype MathExpr = MathExpr Expression

makeGenIntegerValue :: Integer -> Gen Expression
makeGenIntegerValue x = return $ IntegerValue x

makeGenDoubleValue :: Double -> Gen Expression
makeGenDoubleValue x = return $ DoubleValue x

data OpConfig = OpConfig {
    getTerms :: Integer -> Gen (Integer, Integer),
    getCons  :: Expression -> Expression -> Expression
}

addConfig = OpConfig {
    getTerms =
        \case t | t == 0    -> return (0, 0)
                | otherwise -> do
                    t2 <- arbitrary
                    return $ (t - t2, t2),
    getCons = Add
}

subtractConfig = OpConfig {
    getTerms =
        \case t | t == 0    -> return (0, 0)
                | otherwise -> do
                    t2 <- arbitrary
                    return $ (t + t2, t2),
    getCons = Subtract
}

factorize :: Integer -> [Integer]
factorize term = let s = signum term
                     a = abs term
                     k = takeWhile (\x -> x <= a) tenkPrimes
                     c = if a < 100000 then [1..a] else k
                  in [ s * x | x <- c, rem a x == 0 ]

multiplyConfig = OpConfig {
    getTerms =
        \case t | t == 0    -> return (0, 0)
                | otherwise -> do
                    divisor <- elements $ factorize t
                    return $ (div t divisor, divisor),
    getCons = Multiply
}

divideConfig = OpConfig {
    getTerms =
        \case t | t == 0    -> return (0, 1)
                | otherwise -> do
                    divisor <- suchThat arbitrary (/=0)
                    return $ (t * divisor, divisor),
    getCons = Divide
}

type Total = Integer
type Depth = Integer

makeOpGen :: OpConfig -> Depth -> Total -> Gen Expression
makeOpGen tc _ 0 = do
    (t1, t2) <- getTerms tc 0
    return $ getCons tc (IntegerValue t1) (IntegerValue t2)
makeOpGen tc depth total
    | depth <= 0 = makeGenIntegerValue total
    | otherwise  = do
        let nextDepth = depth - 1
        (t1, t2) <- getTerms tc total
        getCons tc <$> (genMathExpr nextDepth t1) <*> (genMathExpr nextDepth t2)

makeAddGen :: Depth -> Total -> Gen Expression
makeAddGen = makeOpGen addConfig

makeSubtractGen :: Depth -> Total -> Gen Expression
makeSubtractGen = makeOpGen subtractConfig

makeMultiplyGen :: Depth -> Total -> Gen Expression
makeMultiplyGen = makeOpGen multiplyConfig

makeDivideGen :: Depth -> Total -> Gen Expression
makeDivideGen = makeOpGen divideConfig

genMathExpr :: Depth -> Total -> Gen Expression
genMathExpr depth total = oneof [
        makeGenIntegerValue total,
        makeAddGen depth total,
        makeSubtractGen depth total,
        makeMultiplyGen depth total,
        makeDivideGen depth total
    ]

exprToString :: Expression -> String
exprToString (IntegerValue a) = integerValueToString a
exprToString (Subtract a b)   = parensWithOp a b "-"
exprToString (Add a b)        = parensWithOp a b "+"
exprToString (Multiply a b)   = parensWithOp a b "*"
exprToString (Divide a b)     = parensWithOp a b "/"

integerValueToString value
    | value < 0 = "(" ++ show value ++ ")"
    | otherwise = show value

parensWithOp a b op = "(" ++ exprToString a ++ op ++ exprToString b ++ ")"

readTotal :: String -> Maybe TotalInput
readTotal a =
    case (readMaybe a :: Maybe Integer) of
        Nothing -> case (readMaybe a :: Maybe Double) of
            Nothing -> Nothing
            v -> fmap DoubleTotal v
        d -> fmap IntegerTotal d

main = do
    args <- getArgs
    let depth = read (args !! 0) :: Integer
        total = readTotal (args !! 1)
        -- total = read (args !! 1) :: Integer
    -- expr <- generate $ genMathExpr depth total
    -- putStrLn $ exprToString expr
    print depth
    print total
