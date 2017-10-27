module SymbolStoreSpecLib where

import Debug.Trace

import qualified Data.Map.Strict as M
import Data.List (intersect)
import Control.Applicative ((<*>))
import Test.QuickCheck
import SymbolStore (SymbolTable(..), Symbol, VarTable)
import AST (Expression(..))

genEmptyVarTable :: Gen VarTable
genEmptyVarTable = oneof [ return $ MutableSymbolTable M.empty
                         , return $ ImmutableSymbolTable M.empty
                         , SymbolStack <$> (resize 5 $ listOf genEmptyVarTable)
                         ]

genVarTableWithout :: Symbol -> Gen VarTable
genVarTableWithout s = oneof [ fmap MutableSymbolTable   $ suchThat genMap (s `M.notMember`)
                             , fmap ImmutableSymbolTable $ suchThat genMap (s `M.notMember`)
                             , fmap SymbolStack $ resize 5 $ listOf $ genVarTableWithout s
                             ]

splitToGroupsOf :: (Show a) => Int -> [a] -> [[a]]
splitToGroupsOf sz xs | sz <= 0 = [xs]
splitToGroupsOf _ [] = []
splitToGroupsOf sz xs = let (hs, ts) = splitAt sz xs
                         in [hs] ++ splitToGroupsOf sz ts

insertSEs :: [(Symbol, Expression)] -> VarTable -> VarTable
insertSEs []  t = t
insertSEs ses (MutableSymbolTable mp)   = MutableSymbolTable   $ M.fromList ses
insertSEs ses (ImmutableSymbolTable mp) = ImmutableSymbolTable $ M.fromList ses
insertSEs ses (SymbolStack []) = SymbolStack [ImmutableSymbolTable $ M.fromList ses]
insertSEs ses (SymbolStack ss) =
    let sesToDisperse = length ses
        ssDestCount   = length ss
        a = fromIntegral sesToDisperse :: Double
        b = fromIntegral ssDestCount :: Double
        sesGroupSize  = ceiling $ a / b
        sesGroupings  = splitToGroupsOf sesGroupSize ses
        sesPadding    = replicate (ssDestCount - (length sesGroupings)) []
        nextSes       = (sesGroupings ++ sesPadding)
     in SymbolStack $ zipWith insertSEs nextSes ss

genVarTableWithValues :: [(Symbol, Expression)] -> Gen VarTable
genVarTableWithValues []  = genEmptyVarTable
genVarTableWithValues sel = do
    vt <- genEmptyVarTable
    genSEL <- genSEListWithValues sel
    return $ insertSEs genSEL vt



genSymbol :: Gen Symbol
genSymbol = do
    prefix <- oneof [ return ""
                    , resize 30 $ listOf $ return $ '_'
                    ]
    body   <- resize 60 $ listOf $ elements $ (['A'..'Z'] ++ ['a'..'z'])
    suffix <- oneof [ return ""
                    , resize 60 $ listOf $ elements ['0'..'9']
                    ]
    return $ prefix ++ body ++ suffix

genExpression :: Gen Expression
genExpression = IntegerValue <$> arbitrary

genMap :: Gen (M.Map Symbol Expression)
genMap = do
    sz <- suchThat arbitrary (\x -> x >= 0 && x <= 60) :: Gen Int
    symbols <- vectorOf sz genSymbol
    exprs   <- vectorOf sz $ oneof [ IntegerValue <$> arbitrary ]
    return $ M.fromList $ zip symbols exprs



genSymbolExprList :: Int -> Gen [(Symbol, Expression)]
genSymbolExprList len = vectorOf len $ (,) <$> genSymbol <*> genExpression

insertAt :: Int -> a -> [a] -> [a]
insertAt pos _ xs | pos < 0 = xs
insertAt pos x [] = [x]
insertAt pos x xs = let (h, t) = splitAt pos xs
                     in h ++ (x:t)

insertFromPosList :: [Int] -> [a] -> [a] -> [a]
insertFromPosList [] _ xs2 = xs2
insertFromPosList _ [] xs2 = xs2
insertFromPosList _ xs1 [] = xs1
insertFromPosList (p:ps) (x1:xs1) xs2 =
    insertFromPosList ps xs1 $ insertAt p x1 xs2

genSEListWithValues :: [(Symbol, Expression)] -> Gen [(Symbol, Expression)]
genSEListWithValues statics = do
    ln  <- suchThat arbitrary (>=0)
    svl <- suchThat (genSymbolExprList ln) (`noSymbolOverlapWith` statics)
    pl  <- vectorOf (length statics) $ suchThat arbitrary (>=0)
    return $ insertFromPosList pl statics svl
  where
    noSymbolOverlapWith = \genSvl staSvl ->
        let genSyms  = (fst <$> genSvl)
            statSyms = (fst <$> staSvl)
         in (length $ genSyms `intersect` statSyms) <= 0
