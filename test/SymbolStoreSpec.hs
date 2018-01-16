module SymbolStoreSpec (spec) where

import Data.Either (isRight)
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
    describe "MutableSymbolTable" $ do
        describe "SymbolStore" $ do
            describe "hasSymbol" $ do
                context "when symbol not present" $ do
                    it "returns False" $ forAll mutableSymTablesWithout $ \(s, t) ->
                        hasSymbol s t `shouldBe` False
                context "when symbol present" $ do
                    it "returns True" $ forAll mutableSymTablesWith $ \(s, _, t) ->
                        hasSymbol s t `shouldBe` True
            describe "getSymbolValue" $ do
                context "when symbol not present" $ do
                    it "returns SymbolNotFoundError" $ forAll mutableSymTablesWithout $ \(s, t) ->
                        getSymbolValue s t `shouldBe` (Left $ SymbolNotFoundError s)
                context "when symbol present" $ do
                    it "returns value" $ forAll mutableSymTablesWith $ \(s, e, t) ->
                        getSymbolValue s t `shouldBe` (Right e)
            describe "setSymbolValue" $ do
                context "when symbol not present" $ do
                    it "inserts the symbol with expected value" $ forAll mutableSymTablesWithout $ \(s, t) ->
                        let e = IntegerValue 5
                            x = setSymbolValue s e t >>= getSymbolValue s
                         in x `shouldBe` (Right e)
                context "when symbol present" $ do
                    it "overwrites the symbol with expected value" $ forAll mutableSymTablesWithout $ \(s, t) ->
                        let e1 = IntegerValue 5
                            e2 = IntegerValue 10
                            x = setSymbolValue s e1 t >>= setSymbolValue s e2 >>= getSymbolValue s
                         in x `shouldBe` (Right e2)
    describe "ImmutableSymbolTable" $ do
        describe "SymbolStore" $ do
            describe "hasSymbol" $ do
                context "when symbol not present" $ do
                    it "returns False" $ forAll immutableSymTablesWithout $ \(s, t) ->
                        hasSymbol s t `shouldBe` False
                context "when symbol present" $ do
                    it "returns True" $ forAll immutableSymTablesWith $ \(s, _, t) ->
                        hasSymbol s t `shouldBe` True
            describe "getSymbolValue" $ do
                context "when symbol not present" $ do
                    it "returns SymbolNotFoundError" $ forAll immutableSymTablesWithout $ \(s, t) ->
                        getSymbolValue s t `shouldBe` (Left $ SymbolNotFoundError s)
                context "when symbol present" $ do
                    it "returns value" $ forAll immutableSymTablesWith $ \(s, e, t) ->
                        getSymbolValue s t `shouldBe` (Right e)
            describe "setSymbolValue" $ do
                context "when symbol not present" $ do
                    it "returns ReadOnlySymbolError" $ forAll immutableSymTablesWithout $ \(s, t) ->
                        let e = IntegerValue 5
                         in setSymbolValue s e t `shouldBe` (Left $ ReadOnlySymbolError s)
                context "when symbol present" $ do
                    it "returns ReadOnlySymbolError" $ forAll immutableSymTablesWith $ \(s, e, t) ->
                        let e = IntegerValue 5
                         in setSymbolValue s e t `shouldBe` (Left $ ReadOnlySymbolError s)
-- spec :: Spec
-- spec = do
    -- describe "SymbolTable" $ do
    --     describe "getSymbolValue" $ do
    --         context "when table empty" $ do
    --             it "returns SymbolNotFoundError" $ forAll genEmptyVarTable $ \st ->
    --                 (getSymbolValue "x" st) `shouldBe` (Left $ mkSymbolNotFoundError "x")
    --         context "when table not empty, but without expected symbol" $ do
    --             it "returns SymbolNotFoundError" $ forAll (genVarTableWithout "x") $ \st ->
    --                 (getSymbolValue "x" st) `shouldBe` (Left $ mkSymbolNotFoundError "x")
    --         context "when table not empty, with expected symbol" $ do
    --             modifyMaxSuccess (const 10000) $ do
    --                 it "returns values for symbol" $ forAll getVarTableWithKnownValues $ \(kvs, st) ->
    --                     let results = (\(s, e) -> (getSymbolValue s st) == Right e) <$> kvs
    --                      in results `shouldAllBe` True
    --     describe "setSymbolValue" $ do
    --         context "when symbol is not present" $ do
    --             context "for MutableSymbolTable" $ do
    --                 it "sets the symbol to the value" $ do
    --                     let emptyTable = MutableSymbolTable $ M.empty
    --                         expectedSetResult = Right $ MutableSymbolTable $ M.fromList [("x" :: Symbol, IntegerValue 10)]
    --                         actualSetResult = setSymbolValue "x" (IntegerValue 10) emptyTable
    --                      in actualSetResult `shouldBe` expectedSetResult
    --             context "for ImmutableSymbolTable" $ do
    --                 it "returns ReadOnlyTableError" $ do
    --                     let emptyTable = ImmutableSymbolTable $ M.empty
    --                         expectedSetResult = Left $ mkReadOnlyTableError
    --                         actualSetResult = setSymbolValue "x" (IntegerValue 10) emptyTable
    --                      in actualSetResult `shouldBe` expectedSetResult
    --             context "for SymbolStack" $ do
    --                 it "???" $ do
    --                     True
    --         context "when symbol is present" $ do
    --             context "for MutableSymbolTable" $ do
    --                 it "replaces the symbol value" $ do
    --                     True
    --             context "for ImmutableSymbolTable" $ do
    --                 it "returns ReadOnlySymbolError" $ do
    --                     True
    --             context "for SymbolStack" $ do
    --                 it "???" $ do
    --                     True
