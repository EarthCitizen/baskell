module Exec (exec) where

import Element
import Eval

exec :: Element -> IO ()
exec (Block []) = return ()
exec (Block xs) = mapM_ exec xs
exec (Print sv) = putStrLn $ expressionToString sv
exec (Main b)   = exec b
