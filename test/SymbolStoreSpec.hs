module SymbolStoreSpec (spec) where

import qualified Data.Map.Strict as M
import Data.Monoid (All(..))
import Test.QuickCheck
import Test.QuickCheck.Property (forAll)
import Test.Hspec
import Test.Hspec.Core.QuickCheck (modifyMaxSize, modifyMaxSuccess)

import SymbolStoreSpecLib

import AST(Expression(..))
import Error
import SymbolStore

spec :: Spec
spec = do
    describe "SymbolTable" $ do
        context "when empty" $ do
            describe "getSymbolValue" $ do
                it "returns SymbolNotFoundError" $ forAll genEmptyVarTable $ \st ->
                    (getSymbolValue "x" st) `shouldBe` (Left $ SymbolNotFoundError "Symbol not found: x")
        context "when not empty, but without expected symbol" $ do
            describe "getSymbolValue" $ do
                it "returns SymbolNotFoundError" $ forAll (genVarTableWithout "x") $ \st ->
                    (getSymbolValue "x" st) `shouldBe` (Left $ SymbolNotFoundError "Symbol not found: x")
        context "when not empty, with expected symbol" $ do
            describe "getSymbolValue" $ do
                modifyMaxSuccess (const 10000) $ it "returns values for symbol" $ forAll getVarTableWithKnownValues $ \(kvs, st) ->
                    let results = (\(s, e) -> (getSymbolValue s st) == Right e) <$> kvs
                     in results `shouldAllBe` True
