module Compile (compileToC) where

import System.IO
import Element


compileToC :: String -> Element -> IO ()
compileToC outFile (Main b) = do
    writeFile  outFile "#include <stdio.h>\n"
    appendFile outFile "\n"
    appendFile outFile "int main(int argc, char **argv)\n"
    appendFile outFile "{\n"
    compileToC outFile b
    appendFile outFile "return 0;\n"
    appendFile outFile "}\n"
    appendFile outFile "\n"

compileToC outFile (Block xs) = do
    mapM_ (compileToC outFile) xs
    return ()

compileToC outFile (Print s) = appendFile outFile $ "printf(\"" ++ s ++ "\\n\");\n"
