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
            compileToFileWith main "out.c"  compileToC
            compileToFileWith main "out.js" compileToJS
            compileToFileWith main "out.sh" compileToBASH
    return ()
