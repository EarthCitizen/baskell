module Exec (execAST, execMain, execBlock, execStatement) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

import AST
import Error
import Eval

execAST :: AST -> IO (Either LangageError ())
execAST (AST m) = execMain m

execMain :: Main -> IO (Either LangageError ())
execMain (Main b) = execBlock b

execBlock :: Block -> IO (Either LangageError ())
execBlock (Block xs) = execStatementList xs

execStatementList :: [Statement] -> IO (Either LangageError ())
execStatementList ([]) = return $ Right ()
execStatementList (x:[]) = execStatement x
execStatementList (x:xs) = do
    ei <- execStatement x
    case ei of
        l@(Left _) -> return l
        r@(Right _) -> execStatementList xs

execStatement :: Statement -> IO (Either LangageError ())
execStatement ps@(PrintStatement _) = execPrintStatement ps
execStatement (IfStatement i) = execIf i

execPrintStatement :: Statement -> IO (Either LangageError ())
execPrintStatement (PrintStatement e) = runEitherT $ do
    case eval e of
        Invalid err -> left err
        ev -> lift $ putStrLn $ expressionToString $ ev
    right ()

execIf :: If -> IO (Either LangageError ())
execIf (If expr block els) = case eval expr of
    BooleanValue True -> execBlock block
    BooleanValue False -> return $ Right ()
    _ -> return $ Left $ TypeMismatchError errMsgNoNumberInBool
