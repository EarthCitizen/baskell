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
        (Right ast) -> do
            execAST ast
            compileToFileWith ast "out.c"  compileToC
            compileToFileWith ast "out.js" compileToJS
            compileToFileWith ast "out.sh" compileToBASH
    return ()
