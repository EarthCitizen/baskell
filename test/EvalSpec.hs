module EvalSpec (spec) where

import Test.QuickCheck
import Test.Hspec
import Test.Hspec.Core.QuickCheck (modifyMaxSize)
import AST
import Eval (eval)
import Error
import Gen (TestNumExpr (..), totalMatchesResult)
import Debug.Trace

spec :: Spec
spec = do
    describe "eval" $ do
        context "when passed BooleanValue True" $ do
            it "returns BooleanValue True" $ do
                eval (BooleanValue True) `shouldBe` (BooleanValue True)
        context "when passed BooleanValue False" $ do
            it "returns BooleanValue False" $ do
                eval (BooleanValue False) `shouldBe` (BooleanValue False)
        context "when passed IntegerValue with any contained integer" $ do
            it "returns IntegerValue with the same contained integer" $ property $
                \x -> (eval $ IntegerValue x) == IntegerValue x
        context "when passed DoubleValue with any contained double" $ do
            it "returns DoubleValue with the same contained double" $ property $
                \x -> (eval $ DoubleValue x) == DoubleValue x
        context "when passed StringValue with any contained string" $ do
            it "returns StringValue with the same contained string" $ property $
                \x -> (eval $ StringValue x) == StringValue x
        context "when passed Invalid with any contained error" $ do
            it "returns Invalid with the same contained error" $ do
                let invalid1 = Invalid $ DivideByZeroError "div0"
                    invalid2 = Invalid $ TypeMismatchError "mismatch"
                [eval invalid1, eval invalid2] `shouldBe` [invalid1, invalid2]
        context "when passed Add with two of IntegerValue" $ do
            it "returns IntegerValue containing the other integers added" $ property $ property $
                \x y -> eval (Add (IntegerValue x) (IntegerValue y)) == IntegerValue (x + y)
        context "when passed Add with two of DoubleValue" $ do
            it "returns DoubleValue containing the other integers added" $ property $ property $
                \x y -> eval (Add (DoubleValue x) (DoubleValue y)) == DoubleValue (x + y)
        context "when passed Add with one IntegerValue and one DoubleValue" $ do
            it "returns DoubleValue containing the integer and double added" $ property $ property $
                \x y -> eval (Add (IntegerValue x) (DoubleValue y)) == DoubleValue (realToFrac x + y)
        context "when passed a valid numeric expression" $ do
            modifyMaxSize (const 2000000) $ it "returns expected total" $ property $
                \(TestNumExpr total expr) -> totalMatchesResult total $ eval expr
