module Exec (exec) where

import Element

exec :: Element -> IO ()
exec (Block []) = return ()
exec (Block xs) = mapM_ exec xs
exec (Print sv) = putStrLn sv
