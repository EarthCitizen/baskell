import System.Environment

import Exec
import Parse
import Compile

main :: IO ()
main = do
    f <- getArgs
    o <- parseFile $ head f
    case o of
        (Left err)  -> print err
        (Right (AST main)) -> do
            exec main
            compileToC "out.c" main
    return ()
