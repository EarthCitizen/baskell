module Exec (execAST, execMain, execBlock, execStatement) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

import AST
import Error
import Eval

execAST :: AST -> IO (Either Error ())
execAST (AST m) = execMain m

execMain :: Main -> IO (Either Error ())
execMain (Main b) = execBlock b

execBlock :: Block -> IO (Either Error ())
execBlock (Block xs) = execStatementList xs

execStatementList :: [Statement] -> IO (Either Error ())
execStatementList ([]) = return $ Right ()
execStatementList (x:[]) = execStatement x
execStatementList (x:xs) = do
    ei <- execStatement x
    case ei of
        l@(Left _) -> return l
        r@(Right _) -> execStatementList xs

execStatement :: Statement -> IO (Either Error ())
execStatement ps@(PrintStatement _) = execPrintStatement ps
execStatement (IfStatement ex bl els) = execIfStatement ex bl els
execStatement NoOp = return $ Right ()

execPrintStatement :: Statement -> IO (Either Error ())
execPrintStatement (PrintStatement e) = runEitherT $ do
    case expressionToString e of
        Left err -> left err
        Right ev -> lift $ putStrLn ev
    right ()

-- execIf :: If -> IO (Either Error ())
-- execIf (If expr block els) = execBranch expr block els
--
-- execElse :: Else -> IO (Either Error ())
-- execElse (ElseIf expr block els) = execBranch expr block els

execIfStatement :: Expression -> Block -> Statement -> IO (Either Error ())
execIfStatement expr block els = case eval expr of
    Right (BooleanValue True)  -> execBlock block
    Right (BooleanValue False) -> execStatement els
        -- ElseIf nextExpr nextBlock nextEls -> execBranch nextExpr nextBlock nextEls
        -- EndIf -> return $ Right ()
    _ -> return $ Left $ TypeMismatchError errMsgNoNumberInBool
