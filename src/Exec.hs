module Exec (execAST, execMain, execBlock, execStatement) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

import AST
import Error
import Eval

execAST :: AST -> IO (Either LanguageError ())
execAST (AST m) = execMain m

execMain :: Main -> IO (Either LanguageError ())
execMain (Main b) = execBlock b

execBlock :: Block -> IO (Either LanguageError ())
execBlock (Block xs) = execStatementList xs

execStatementList :: [Statement] -> IO (Either LanguageError ())
execStatementList ([]) = return $ Right ()
execStatementList (x:[]) = execStatement x
execStatementList (x:xs) = do
    ei <- execStatement x
    case ei of
        l@(Left _) -> return l
        r@(Right _) -> execStatementList xs

execStatement :: Statement -> IO (Either LanguageError ())
execStatement ps@(PrintStatement _) = execPrintStatement ps
execStatement (IfStatement ex bl els) = execIfStatement ex bl els
execStatement NoOp = return $ Right ()

execPrintStatement :: Statement -> IO (Either LanguageError ())
execPrintStatement (PrintStatement e) = runEitherT $ do
    case eval e of
        Invalid err -> left err
        ev -> lift $ putStrLn $ expressionToString $ ev
    right ()

-- execIf :: If -> IO (Either LanguageError ())
-- execIf (If expr block els) = execBranch expr block els
--
-- execElse :: Else -> IO (Either LanguageError ())
-- execElse (ElseIf expr block els) = execBranch expr block els

execIfStatement :: Expression -> Block -> Statement -> IO (Either LanguageError ())
execIfStatement expr block els = case eval expr of
    BooleanValue True -> execBlock block
    BooleanValue False -> execStatement els
        -- ElseIf nextExpr nextBlock nextEls -> execBranch nextExpr nextBlock nextEls
        -- EndIf -> return $ Right ()
    _ -> return $ Left $ TypeMismatchError errMsgNoNumberInBool
