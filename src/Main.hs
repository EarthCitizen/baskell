import System.Environment

import Exec
import Parse

main :: IO ()
main = do
    f <- getArgs
    o <- parseFile $ head f
    case o of
        (Left err)  -> print err
        (Right (AST block)) -> exec block
    return ()
