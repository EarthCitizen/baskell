module ExecSpec (spec) where

import Test.QuickCheck
import Test.Hspec
import AST
import Exec
import Error
import Debug.Trace
import System.IO.Silently

spec :: Spec
spec = do
    describe "execStatement" $ do
        context "when passed a PrintStatement" $ do
            it "prints the value of the expression parameter with a newline" $ do
                (out, _) <- capture $ execStatement $ PrintStatement $ StringValue "some test value"
                out `shouldBe` "some test value\n"
        context "when passed an IfStatement" $ do
            context "with one branch" $ do
                context "and the expression is true" $ do
                    it "executes the block" $ do
                        let printStatement1 = PrintStatement $ StringValue "test output 1"
                            printStatement2 = PrintStatement $ StringValue "test output 2"
                            block = Block [printStatement1, printStatement2]
                            ifStatement = IfStatement $ If (BooleanValue True) block EndIf
                        (out, _) <- capture $ execStatement ifStatement
                        out `shouldBe` "test output 1\ntest output 2\n"
                context "and the expression is false" $ do
                    it "does not execute the block" $ do
                        let printStatement1 = PrintStatement $ StringValue "test output 1"
                            printStatement2 = PrintStatement $ StringValue "test output 2"
                            block = Block [printStatement1, printStatement2]
                            ifStatement = IfStatement $ If (BooleanValue False) block EndIf
                        (out, _) <- capture $ execStatement ifStatement
                        out `shouldBe` ""
                -- context "and the expression not a BooleanValue" $ do
                --    it "returns"
