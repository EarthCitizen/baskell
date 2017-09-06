{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternGuards #-}

import Data.Tuple (swap)
import System.Environment (getArgs)
import Test.QuickCheck
import Control.Applicative ((<|>))
import Control.Monad (liftM)
import AST
import Text.Read (readEither, readMaybe)

import Debug.Trace

data Params = Params { getDepth :: Integer
                     , getTotal :: Total
                     } deriving (Eq, Show)

data Total = IntegerTotal Integer
           | DoubleTotal Double
           deriving (Eq, Show)

-- Strict, otherwise the list will be rebuilt each reference
tenkPrimes :: [Integer]
!tenkPrimes = initialPrimes ++ [ x | x <- [14..10000], isPrime x]
    where initialPrimes = [1, 2, 3, 5, 7, 9, 11, 13]
          isPrime x = (not . any ((0 ==) . (rem x))) [2..(x-1)]

newtype MathExpr = MathExpr Expression

makeGenIntegerValue :: Integer -> Gen Expression
makeGenIntegerValue x = return $ IntegerValue x

makeGenDoubleValue :: Double -> Gen Expression
makeGenDoubleValue x = return $ DoubleValue x

data OpConfig = OpConfig
              { getTerms :: Total -> Gen (Expression, Expression)
              , getCons  :: Expression -> Expression -> Expression
              }

getTermsAddInteger :: Integer -> Gen (Expression, Expression)
getTermsAddInteger i = do
    i2 <- arbitrary
    let opt1 = (IntegerValue (i - i2), IntegerValue (i2))
        opt2 = swap opt1
    oneof [ return opt1, return opt2 ]

getTermsAddDouble :: Double -> Gen (Expression, Expression)
getTermsAddDouble d = oneof [doubleDouble, doubleInteger]
    where doubleDouble = do
              d2 <- arbitrary
              let opt1 = (DoubleValue (d - d2), DoubleValue d2)
                  opt2 = swap opt1
              oneof [ return opt1, return opt2 ]
          doubleInteger = do
              i <- arbitrary :: Gen Integer
              let opt1 = (DoubleValue (d - (fromIntegral i)), IntegerValue i)
                  opt2 = swap opt1
              oneof [ return opt1, return opt2 ]

getTermsAdd :: Total -> Gen (Expression, Expression)
getTermsAdd (IntegerTotal i) = getTermsAddInteger i
getTermsAdd (DoubleTotal  d) = getTermsAddDouble  d

addConfig = OpConfig
          { getTerms = getTermsAdd
          , getCons = Add
          }

--------------------------------------------------------------------------------

getTermsSubtractInteger :: Integer -> Gen (Expression, Expression)
getTermsSubtractInteger i = do
    i2 <- arbitrary
    return (IntegerValue (i + i2), IntegerValue i2)

getTermsSubtractDouble :: Double -> Gen (Expression, Expression)
getTermsSubtractDouble d = oneof [doubleDouble, doubleInteger1, doubleInteger2]
    where doubleDouble = do
              d2 <- arbitrary
              return (DoubleValue (d + d2), DoubleValue d2)
          doubleInteger1 = do
              i <- arbitrary :: Gen Integer
              return (DoubleValue (d + (fromIntegral i)), IntegerValue i)
          doubleInteger2 = do
              i <- arbitrary
              return (IntegerValue i, DoubleValue ((fromIntegral i) - d))

getTermsSubtract :: Total -> Gen (Expression, Expression)
getTermsSubtract (IntegerTotal i) = getTermsSubtractInteger i
getTermsSubtract (DoubleTotal  d) = getTermsSubtractDouble  d

subtractConfig = OpConfig
               { getTerms = getTermsSubtract
               , getCons = Subtract
               }

--------------------------------------------------------------------------------

-- This will find at least a factor of 1
factorize :: Integer -> [Integer]
factorize term = let s = signum term
                     a = abs term
                     k = takeWhile (\x -> x <= a) tenkPrimes
                     c = if a < 100000 then [1..a] else k
                  in [ s * x | x <- c, rem a x == 0 ]

getTermsMultiplyInteger :: Integer -> Gen (Expression, Expression)
getTermsMultiplyInteger 0 = do
    i2 <- arbitrary
    let opt1 = (IntegerValue 0, IntegerValue i2)
        opt2 = swap opt1
    oneof [return opt1, return opt2]
getTermsMultiplyInteger i = do
    factor <- elements $ factorize i
    let opt1 = (IntegerValue (div i factor), IntegerValue factor)
        opt2 = swap opt1
    oneof [ return opt1, return opt2 ]

getTermsMultiplyDouble :: Double -> Gen (Expression, Expression)
getTermsMultiplyDouble 0 = do
    d2 <- arbitrary
    i2 <- arbitrary
    let opt1 = (DoubleValue 0, DoubleValue d2)
        opt2 = swap opt1
        opt3 = (DoubleValue 0, IntegerValue i2)
        opt4 = swap opt2
        opt5 = (IntegerValue 0, DoubleValue d2)
        opt6 = swap opt5
    oneof $ return <$> [opt1, opt2, opt3, opt4, opt5, opt6]
getTermsMultiplyDouble d = oneof [doubleDouble, doubleInteger]
    where notIsInfinite = not . isInfinite
          doubleDouble = do
              let termFn   = (d/)
                  termFltr = notIsInfinite . termFn
              d2 <- suchThat arbitrary termFltr
              let opt1 = (DoubleValue (termFn d2), DoubleValue d2)
                  opt2 = swap opt1
              oneof [ return opt1, return opt2 ]
          doubleInteger = do
              let termFn   = (\x -> (d / (fromIntegral x)))
                  termFltr = notIsInfinite . termFn
              i <- suchThat arbitrary termFltr :: Gen Integer
              let opt1 = (DoubleValue (termFn i), IntegerValue i)
                  opt2 = swap opt1
              oneof [ return opt1, return opt2 ]

getTermsMultiply :: Total -> Gen (Expression, Expression)
getTermsMultiply (IntegerTotal i) = getTermsMultiplyInteger i
getTermsMultiply (DoubleTotal d)  = getTermsMultiplyDouble d

multiplyConfig = OpConfig
               { getTerms = getTermsMultiply
               , getCons = Multiply
               }

--------------------------------------------------------------------------------

getTermsDivideInteger :: Integer -> Gen (Expression, Expression)
getTermsDivideInteger i = do
    divisor <- suchThat arbitrary (/=0)
    return (IntegerValue (i * divisor), IntegerValue divisor)

getTermsDivideDouble :: Double -> Gen (Expression, Expression)
getTermsDivideDouble d = oneof [ doubleDouble, doubleInteger ]
    where doubleDouble = do
              divisorDbl <- suchThat arbitrary (/=0)
              return (DoubleValue (d * divisorDbl), DoubleValue divisorDbl)
          doubleInteger = do
              divisorInt <- suchThat arbitrary (/=0) :: Gen Integer
              return (DoubleValue (d * (fromIntegral divisorInt)), IntegerValue divisorInt)

getTermsDivide :: Total -> Gen (Expression, Expression)
getTermsDivide (IntegerTotal i) = getTermsDivideInteger i
getTermsDivide (DoubleTotal d)  = getTermsDivideDouble d

divideConfig = OpConfig
             { getTerms = getTermsDivide
             , getCons = Divide
             }

type Depth = Integer

makeOpGen :: OpConfig -> Depth -> Total -> Gen Expression
makeOpGen tc depth tot
    | (DoubleTotal  d) <- tot, depth <= 0 = makeGenDoubleValue  d
    | (IntegerTotal i) <- tot, depth <= 0 = makeGenIntegerValue i
    | (DoubleTotal  0) <- tot = makeGenDoubleValue  0
    | (IntegerTotal 0) <- tot = makeGenIntegerValue 0
    | otherwise = do
        let nextDepth = depth - 1
        (t1, t2) <- getTerms tc tot
        getCons tc <$> (genMathExpr nextDepth $ termToTotal t1) <*> (genMathExpr nextDepth $ termToTotal t2)
    where termToTotal (DoubleValue  d) = DoubleTotal  d
          termToTotal (IntegerValue i) = IntegerTotal i

makeAddGen :: Depth -> Total -> Gen Expression
makeAddGen = makeOpGen addConfig

makeSubtractGen :: Depth -> Total -> Gen Expression
makeSubtractGen = makeOpGen subtractConfig

makeMultiplyGen :: Depth -> Total -> Gen Expression
makeMultiplyGen = makeOpGen multiplyConfig

makeDivideGen :: Depth -> Total -> Gen Expression
makeDivideGen = makeOpGen divideConfig

makeNumberValueGen :: Total -> Gen Expression
makeNumberValueGen (IntegerTotal i) = return $ IntegerValue i
makeNumberValueGen (DoubleTotal  d) = return $ DoubleValue  d

genMathExpr :: Depth -> Total -> Gen Expression
genMathExpr depth total = oneof [ makeNumberValueGen total
                                , makeAddGen      depth total
                                , makeSubtractGen depth total
                                , makeMultiplyGen depth total
                                , makeDivideGen   depth total
                                ]

exprToString :: Expression -> String
exprToString (DoubleValue d)  = doubleValueToString d
exprToString (IntegerValue i) = integerValueToString i
exprToString (Subtract a b)   = parensWithOp a b "-"
exprToString (Add a b)        = parensWithOp a b "+"
exprToString (Multiply a b)   = parensWithOp a b "*"
exprToString (Divide a b)     = parensWithOp a b "/"

doubleValueToString = numValueToString :: Double -> String
integerValueToString = numValueToString :: Integer -> String

numValueToString :: (Num a, Ord a, Show a) => a -> String
numValueToString value
    | value < 0 = "(" ++ show value ++ ")"
    | otherwise = show value

parensWithOp a b op = "(" ++ exprToString a ++ op ++ exprToString b ++ ")"

readDepth :: String -> Either String Integer
readDepth a = readDepth' >>= validateDepth
    where readDepth' = (readEither a :: Either String Integer) <|> Left ("Not a valid depth: " ++ a)
          validateDepth d
              | d >= 0    = Right d
              | otherwise = Left "Depth must be >= 0"

readTotal :: String -> Either String Total
readTotal a =
    (fmap IntegerTotal (readEither a :: Either String Integer)) <|>
    (fmap DoubleTotal  (readEither a :: Either String Double))  <|>
    Left ("Not a valid total: " ++ a)

showValue (DoubleValue  d) = show d
showValue (IntegerValue i) = show i

main = do
    args <- getArgs
    let depthArg = (args !! 0)
        totalArg = (args !! 1)
        paramsEither = Params <$> readDepth depthArg <*> readTotal totalArg
    case paramsEither of
        Left err     -> putStrLn err
        Right params -> do
            let depth = getDepth params
                total = getTotal params
            expr <- generate $ genMathExpr depth total
            putStrLn $ exprToString expr
