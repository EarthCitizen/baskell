module Exec (execAST) where

import AST
import Eval

-- exec :: Element -> IO ()
-- exec (Block []) = return ()
-- exec (Block xs) = mapM_ exec xs
-- exec (Print sv) = putStrLn $ expressionToString sv
-- exec (Main b)   = exec b

execAST :: AST -> IO ()
execAST (AST m) = execMain m

execMain :: Main -> IO ()
execMain (Main b) = execBlock b

execBlock :: Block -> IO ()
execBlock (Block s) = mapM_ execStatement s

execStatement :: Statement -> IO ()
execStatement (PrintStatement e) = putStrLn $ expressionToString e
