{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleInstances #-}

module Gen where

import Data.Tuple (swap)
import System.Environment (getArgs)
import Test.QuickCheck
import Control.Applicative ((<|>))
import Control.Monad (liftM)
import AST
import Text.Read (readEither, readMaybe)
import qualified Data.AEq as D
import Debug.Trace

data Params = Params { getDepth :: Depth
                     , getTotal :: Total
                     } deriving (Eq, Show)

data Total = IntegerTotal Integer
           | FloatingTotal BigDecimal
           deriving (Eq, Show)

instance Arbitrary BigDecimal where
    arbitrary = do
        d <- arbitrary :: Gen Double
        return $ realToFrac d

-- Strict, otherwise the list will be rebuilt each reference
tenkPrimes :: [Integer]
!tenkPrimes = initialPrimes ++ [ x | x <- [14..10000], isPrime x]
    where initialPrimes = [1, 2, 3, 5, 7, 9, 11, 13]
          isPrime x = (not . any ((0 ==) . (rem x))) [2..(x-1)]

newtype MathExpr = MathExpr Expression

genBigDecimal :: Gen BigDecimal
genBigDecimal = do
    s <- oneof $ return <$> [0, 3, 6, 12, 24, 1200, 48000, 960000, 1920000000, 3840000000]
    resize s arbitrary

makeGenIntegerValue :: Integer -> Gen Expression
makeGenIntegerValue x = return $ IntegerValue x

makeGenDoubleValue :: BigDecimal -> Gen Expression
makeGenDoubleValue x = return $ FloatingValue x

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

getTermsAddDouble :: BigDecimal -> Gen (Expression, Expression)
getTermsAddDouble d = frequency [(2, floatingFloating), (1, floatingInteger)]
    where floatingFloating = do
              d2 <- genBigDecimal
              let opt1 = (FloatingValue (d - d2), FloatingValue d2)
                  opt2 = swap opt1
              oneof [ return opt1, return opt2 ]
          floatingInteger = do
              i <- arbitrary :: Gen Integer
              let opt1 = (FloatingValue (d - (fromIntegral i)), IntegerValue i)
                  opt2 = swap opt1
              oneof [ return opt1, return opt2 ]

getTermsAdd :: Total -> Gen (Expression, Expression)
getTermsAdd (IntegerTotal i) = getTermsAddInteger i
getTermsAdd (FloatingTotal  d) = getTermsAddDouble  d

addConfig = OpConfig
          { getTerms = getTermsAdd
          , getCons = Add
          }

--------------------------------------------------------------------------------

getTermsSubtractInteger :: Integer -> Gen (Expression, Expression)
getTermsSubtractInteger i = do
    i2 <- arbitrary
    return (IntegerValue (i + i2), IntegerValue i2)

getTermsSubtractDouble :: BigDecimal -> Gen (Expression, Expression)
getTermsSubtractDouble d = frequency [(4, floatingFloating), (1, floatingInteger1), (1, floatingInteger2)]
    where floatingFloating = do
              d2 <- genBigDecimal
              return (FloatingValue (d + d2), FloatingValue d2)
          floatingInteger1 = do
              i <- arbitrary :: Gen Integer
              return (FloatingValue (d + (fromIntegral i)), IntegerValue i)
          floatingInteger2 = do
              i <- arbitrary
              return (IntegerValue i, FloatingValue ((fromIntegral i) - d))

getTermsSubtract :: Total -> Gen (Expression, Expression)
getTermsSubtract (IntegerTotal i) = getTermsSubtractInteger i
getTermsSubtract (FloatingTotal  d) = getTermsSubtractDouble  d

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

getTermsMultiplyDouble :: BigDecimal -> Gen (Expression, Expression)
getTermsMultiplyDouble 0 = do
    d2 <- genBigDecimal
    i2 <- arbitrary
    let opt1 = (FloatingValue 0, FloatingValue d2)
        opt2 = swap opt1
        opt3 = (FloatingValue 0, IntegerValue i2)
        opt4 = swap opt2
        opt5 = (IntegerValue 0, FloatingValue d2)
        opt6 = swap opt5
    oneof $ return <$> [opt1, opt2, opt3, opt4, opt5, opt6]
getTermsMultiplyDouble d = frequency [(2, floatingFloating), (1, floatingInteger)]
    where isBadNumber = \x -> isInfinite x || isNaN x || isDenormalized x || isNegativeZero x || x == 0
          floatingFloating = do
              let termFn   = (d/)
                  termFltr = not . isBadNumber
              d2 <- suchThat genBigDecimal termFltr
              let opt1 = (FloatingValue (termFn d2), FloatingValue d2)
                  opt2 = swap opt1
              oneof [ return opt1, return opt2 ]
          floatingInteger = do
              let termFn   = (d/) . fromIntegral
                  termFltr = (/=0)
              i <- suchThat arbitrary termFltr :: Gen Integer
              let opt1 = (FloatingValue (termFn i), IntegerValue i)
                  opt2 = swap opt1
              oneof [ return opt1, return opt2 ]

getTermsMultiply :: Total -> Gen (Expression, Expression)
getTermsMultiply (IntegerTotal i)  = getTermsMultiplyInteger i
getTermsMultiply (FloatingTotal d) = getTermsMultiplyDouble d

multiplyConfig = OpConfig
               { getTerms = getTermsMultiply
               , getCons = Multiply
               }

--------------------------------------------------------------------------------

getTermsDivideInteger :: Integer -> Gen (Expression, Expression)
getTermsDivideInteger i = do
    divisor <- suchThat arbitrary (/=0)
    return (IntegerValue (i * divisor), IntegerValue divisor)

getTermsDivideDouble :: BigDecimal -> Gen (Expression, Expression)
getTermsDivideDouble d = frequency [(2, floatingFloating), (1, floatingInteger)]
    where floatingFloating = do
              divisorDbl <- suchThat genBigDecimal (/=0)
              return (FloatingValue (d * divisorDbl), FloatingValue divisorDbl)
          floatingInteger = do
              divisorInt <- suchThat arbitrary (/=0) :: Gen Integer
              return (FloatingValue (d * (fromIntegral divisorInt)), IntegerValue divisorInt)

getTermsDivide :: Total -> Gen (Expression, Expression)
getTermsDivide (IntegerTotal i)  = getTermsDivideInteger i
getTermsDivide (FloatingTotal d) = getTermsDivideDouble d

divideConfig = OpConfig
             { getTerms = getTermsDivide
             , getCons = Divide
             }

type Depth = Int

makeOpGen :: OpConfig -> Depth -> Total -> Gen Expression
makeOpGen tc depth tot
    | (FloatingTotal  d) <- tot, depth <= 0 = makeGenDoubleValue  d
    | (IntegerTotal i)   <- tot, depth <= 0 = makeGenIntegerValue i
    | (FloatingTotal  0) <- tot = makeGenDoubleValue  0
    | (IntegerTotal 0)   <- tot = makeGenIntegerValue 0
    | otherwise = do
        let nextDepth = depth - 1
        (t1, t2) <- getTerms tc tot
        getCons tc <$> (genMathExpr nextDepth $ termToTotal t1) <*> (genMathExpr nextDepth $ termToTotal t2)
    where termToTotal (FloatingValue  d) = FloatingTotal  d
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
makeNumberValueGen (FloatingTotal  d) = return $ FloatingValue  d

genMathExpr :: Depth -> Total -> Gen Expression
genMathExpr depth total = oneof [ makeAddGen      depth total
                                , makeSubtractGen depth total
                                , makeMultiplyGen depth total
                                , makeDivideGen   depth total
                                ]

instance Arbitrary Total where
    arbitrary = oneof [FloatingTotal <$> arbitrary, IntegerTotal <$> arbitrary]

data TestNumExpr = TestNumExpr Total Expression deriving (Show)

totalMatchesResult :: Total -> Expression -> Bool
totalMatchesResult (FloatingTotal total) (FloatingValue testVal) =
    let d1 = realToFrac total :: Double
        d2 = realToFrac testVal :: Double
     in d1 D.~== d2
totalMatchesResult (IntegerTotal total) (IntegerValue testVal) =
    total == testVal
totalMatchesResult _ _ = False

instance Arbitrary TestNumExpr where
    arbitrary = do
        sz    <- getSize
        depth <- choose (0, sz)
        total <- arbitrary
        expr  <- genMathExpr depth total
        return $ TestNumExpr total expr

exprToString :: Expression -> String
exprToString (FloatingValue d) = floatingValueToString d
exprToString (IntegerValue i)  = integerValueToString i
exprToString (Subtract a b)    = parensWithOp a b "-"
exprToString (Add a b)         = parensWithOp a b "+"
exprToString (Multiply a b)    = parensWithOp a b "*"
exprToString (Divide a b)      = parensWithOp a b "/"

floatingValueToString = numValueToString :: BigDecimal -> String
integerValueToString = numValueToString :: Integer -> String

numValueToString :: (Num a, Ord a, Show a) => a -> String
numValueToString value
    | value < 0 = "(" ++ show value ++ ")"
    | otherwise = show value

parensWithOp a b op = "(" ++ exprToString a ++ op ++ exprToString b ++ ")"

readDepth :: String -> Either String Depth
readDepth a = readDepth' >>= validateDepth
    where readDepth' = (readEither a :: Either String Depth) <|> Left ("Not a valid depth: " ++ a)
          validateDepth d
              | d >= 0    = Right d
              | otherwise = Left "Depth must be >= 0"

readTotal :: String -> Either String Total
readTotal a =
    (fmap IntegerTotal  (readEither a :: Either String Integer))     <|>
    (fmap bigFloatTotal (readEither a :: Either String Double))  <|>
    Left ("Not a valid total: " ++ a)
    where bigFloatTotal = FloatingTotal . realToFrac

showValue (FloatingValue  d) = show d
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
            rndDepth <- generate $ suchThat arbitrary (\a -> a >= 0 && a <= depth)
            expr <- generate $ genMathExpr rndDepth total
            putStrLn $ exprToString expr
