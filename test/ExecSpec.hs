module ExecSpec (spec) where

import Data.Typeable
import Test.QuickCheck
import Test.Hspec
import AST
import Exec
import Error
import Debug.Trace
import System.IO.Silently

printStatement1_1 = PrintStatement $ StringValue "test output 1-1"
printStatement1_2 = PrintStatement $ StringValue "test output 1-2"
printStatementsBlock1 = Block [printStatement1_1, printStatement1_2]

printStatement2_1 = PrintStatement $ StringValue "test output 2-1"
printStatement2_2 = PrintStatement $ StringValue "test output 2-2"
printStatementsBlock2 = Block [printStatement2_1, printStatement2_2]

printStatement3_1 = PrintStatement $ StringValue "test output 3-1"
printStatement3_2 = PrintStatement $ StringValue "test output 3-2"
printStatementsBlock3 = Block [printStatement3_1, printStatement3_2]

printStatementWithoutError = PrintStatement $ StringValue "no error here"
printStatementWithError = PrintStatement $ Add (BooleanValue True) (BooleanValue True)
blockWithError = Block $ [printStatementWithoutError, printStatementWithError, printStatementWithoutError]

nonBooleanExpression = IntegerValue 123

spec :: Spec
spec = do
    describe "execStatement" $ do
        context "when passed a PrintStatement" $ do
            context "when the expression does not contain an error" $ do
                it "prints the value of the expression with a newline and returns OK" $ do
                    (out, result) <- capture $ execStatement $ printStatement1_1
                    out `shouldBe` "test output 1-1\n"
                    result `shouldBe` Right ()
            context "when the expression contains an error" $ do
                it "does not print anything and returns the error" $ do
                    (out, result) <- capture $ execStatement $ printStatementWithError
                    out `shouldBe` ""
                    result `shouldBe` (Left $ TypeMismatchError errMsgNoBooleanInNum)

        context "when passed an IfStatement" $ do
            context "with one branch" $ do
                context "when the expression is true" $ do
                    context "when the block does not contain errors" $ do
                        it "executes the block and returns OK" $ do
                            let ifStatement = IfStatement $ If (BooleanValue True) printStatementsBlock1 EndIf
                            (out, result) <- capture $ execStatement ifStatement
                            out `shouldBe` "test output 1-1\ntest output 1-2\n"
                            result `shouldBe` Right ()
                    context "when the block contains errors" $ do
                        it "executes the block and returns the error from the errored statement" $ do
                            let ifStatement = IfStatement $ If (BooleanValue True) blockWithError EndIf
                            (out, result) <- capture $ execStatement ifStatement
                            out `shouldBe` "no error here\n"
                            result `shouldBe` (Left $ TypeMismatchError errMsgNoBooleanInNum)
                context "when the expression is false" $ do
                    it "does not execute the block and returns OK" $ do
                        let ifStatement = IfStatement $ If (BooleanValue False) printStatementsBlock1 EndIf
                        (out, result) <- capture $ execStatement ifStatement
                        out `shouldBe` ""
                        result `shouldBe` Right ()
                context "when the expression is not a boolean value" $ do
                    it "does not execute the block and returns an error" $ do
                        let ifStatement = IfStatement $ If nonBooleanExpression printStatementsBlock1 EndIf
                        (out, result) <- capture $ execStatement ifStatement
                        out `shouldBe` ""
                        result `shouldBe` (Left $ TypeMismatchError errMsgNoNumberInBool)

    describe "execIf" $ do
        context "when passed an If" $ do
            context "with one branch" $ do
                context "when the expression is true" $ do
                    context "when the block does not contain errors" $ do
                        it "executes the block and returns OK" $ do
                            let ifBranch = If (BooleanValue True) printStatementsBlock1 EndIf
                            (out, result) <- capture $ execIf ifBranch
                            out `shouldBe` "test output 1-1\ntest output 1-2\n"
                            result `shouldBe` Right ()
                    context "when the block contains errors" $ do
                        it "executes the block and returns the error from the errored statement" $ do
                            let ifBranch = If (BooleanValue True) blockWithError EndIf
                            (out, result) <- capture $ execIf ifBranch
                            out `shouldBe` "no error here\n"
                            result `shouldBe` (Left $ TypeMismatchError errMsgNoBooleanInNum)
                context "when the expression is false" $ do
                    it "does not execute the block and returns OK" $ do
                        let ifBranch = If (BooleanValue False) printStatementsBlock1 EndIf
                        (out, result) <- capture $ execIf ifBranch
                        out `shouldBe` ""
                        result `shouldBe` Right ()
            context "with an else if branch" $ do
                context "when the if expression is true" $ do
                    it "executes the if block and returns OK" $ do
                        let elseBranch = ElseIf (BooleanValue False) printStatementsBlock2 EndIf
                            ifBranch = If (BooleanValue True) printStatementsBlock1 elseBranch
                        (out, result) <- capture $ execIf ifBranch
                        out `shouldBe` "test output 1-1\ntest output 1-2\n"
                        result `shouldBe` Right ()
                context "when the else if expression is true" $ do
                    it "executes the else if block and returns OK" $ do
                        let elseBranch = ElseIf (BooleanValue True) printStatementsBlock2 EndIf
                            ifBranch = If (BooleanValue False) printStatementsBlock1 elseBranch
                        (out, result) <- capture $ execIf ifBranch
                        out `shouldBe` "test output 2-1\ntest output 2-2\n"
                        result `shouldBe` Right ()
                it "does something" $ do
                    (shouldBe True True) >> (shouldBe True False) >> (shouldBe True True)

    describe "execElse" $ do
        context "when passed an ElseIf" $ do
            context "with one branch" $ do
                context "when the expression is true" $ do
                    context "when the block does not contain errors" $ do
                        it "executes the block and returns OK" $ do
                            let elseBranch = ElseIf (BooleanValue True) printStatementsBlock1 EndIf
                            (out, result) <- capture $ execElse elseBranch
                            out `shouldBe` "test output 1-1\ntest output 1-2\n"
                            result `shouldBe` Right ()
                    context "when the block contains errors" $ do
                        it "executes the block and returns the error from the errored statement" $ do
                            let elseBranch = ElseIf (BooleanValue True) blockWithError EndIf
                            (out, result) <- capture $ execElse elseBranch
                            out `shouldBe` "no error here\n"
                            result `shouldBe` (Left $ TypeMismatchError errMsgNoBooleanInNum)
                context "when the expression is false" $ do
                    it "does not execute the block and returns OK" $ do
                        let elseBranch = ElseIf (BooleanValue False) printStatementsBlock1 EndIf
                        (out, result) <- capture $ execElse elseBranch
                        out `shouldBe` ""
                        result `shouldBe` Right ()
