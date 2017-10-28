module SymbolStoreSpecLib where

import Debug.Trace

import qualified Data.Map.Strict as M
import Data.List (all, intersect)
import Control.Applicative ((<*>))
import Test.QuickCheck
import SymbolStore (SymbolTable(..), Symbol, VarTable)
import AST (Expression(..))

shouldAllBe :: (Eq a) => [a] -> a -> Bool
shouldAllBe xs x = all (\e -> e == x) xs

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
    vt   <- genEmptyVarTable
    gsel <- genSEListWithValues sel
    return $ insertSEs gsel vt

getVarTableWithKnownValues :: Gen ([(Symbol, Expression)], VarTable)
getVarTableWithKnownValues = do
    ln   <- suchThat arbitrary (>0)
    ksel <- genUniqueSymbolExprList ln
    gsel <- genSEListWithValues ksel
    vt   <- genVarTableWithValues gsel
    return $ (ksel, vt)

genSymbol :: Gen Symbol
genSymbol = do
    let leadChrs = ['A'..'Z'] ++ ['a'..'z'] ++ ['_']
    let bodyChrs = leadChrs ++ ['0'..'9']
    prefix <- oneof [ return ""
                    , resize 30 $ listOf $ return $ '_'
                    ]
    lead   <- elements leadChrs
    body   <- resize 60 $ listOf $ elements bodyChrs
    return $ prefix ++ [lead] ++ body

genExpression :: Gen Expression
genExpression = IntegerValue <$> arbitrary

genMap :: Gen (M.Map Symbol Expression)
genMap = do
    sz <- suchThat arbitrary (\x -> x >= 0 && x <= 60) :: Gen Int
    symbols <- vectorOf sz genSymbol
    exprs   <- vectorOf sz $ oneof [ IntegerValue <$> arbitrary ]
    return $ M.fromList $ zip symbols exprs

genUniqueSymbolList :: Int -> Gen [Symbol]
genUniqueSymbolList ln | ln <= 0 = return []
genUniqueSymbolList ln = do
    genSymbolsNotIn ln []
  where
    genSymbolsNotIn 0 ss = return []
    genSymbolsNotIn l ss = do
        sym   <- suchThat genSymbol $ \s -> s `notElem` ss
        usyms <- genSymbolsNotIn (l - 1) ([sym] ++ ss)
        return $ [sym] ++ usyms

genUniqueSymbolExprList :: Int -> Gen [(Symbol, Expression)]
genUniqueSymbolExprList ln = do
    usyms <- genUniqueSymbolList ln
    exprs <- vectorOf ln $ genExpression
    return $ zipWith (,) usyms exprs

-- genSymbolExprList :: Int -> Gen [(Symbol, Expression)]
-- genSymbolExprList len = vectorOf len $ (,) <$> genSymbol <*> genExpression

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
    svl <- suchThat (genUniqueSymbolExprList ln) (`noSymbolOverlapWith` statics)
    shuffle $ svl ++ statics
  where
    noSymbolOverlapWith = \genSvl staSvl ->
        let genSyms  = (fst <$> genSvl)
            statSyms = (fst <$> staSvl)
         in (length $ genSyms `intersect` statSyms) <= 0
