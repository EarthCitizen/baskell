module SymbolStoreSpec (spec) where

import qualified Data.Map.Strict as M

import Test.QuickCheck
import Test.QuickCheck.Property (forAll)
import Test.Hspec

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
        context "from MutableSymbolTable" $ do
            context "when contains symbol" $ do
                it "returns value" $ do
                    let st = MutableSymbolTable $ M.fromList [("y", BooleanValue True), ("z", IntegerValue 5)]
                    (getSymbolValue "y" st) `shouldBe` (Right $ BooleanValue True)
        context "from ImmutableSymbolTable" $ do
            context "when contains symbol" $ do
                it "returns value" $ do
                    let st = ImmutableSymbolTable $ M.fromList [("y", BooleanValue True), ("z", IntegerValue 5)]
                    (getSymbolValue "y" st) `shouldBe` (Right $ BooleanValue True)
        context "from SymbolStack" $ do
            context "when contains symbol" $ do
                it "returns value of first symbol found" $ do
                    let st = SymbolStack [ MutableSymbolTable $ M.fromList [("y", BooleanValue True), ("z", IntegerValue 5)]
                                         , MutableSymbolTable $ M.fromList [("y", BooleanValue False), ("z", IntegerValue 50)]
                                         ]
                    (getSymbolValue "y" st) `shouldBe` (Right $ BooleanValue True)
                it "returns value" $ do
                    let st = SymbolStack [MutableSymbolTable $ M.fromList [("y", BooleanValue True), ("z", IntegerValue 5)]]
                    (getSymbolValue "y" st) `shouldBe` (Right $ BooleanValue True)
