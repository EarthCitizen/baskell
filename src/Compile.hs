module Compile (compileToBASH, compileToC, compileToJS, compileToFileWith) where

import System.IO
import Element

compileToFileWith :: Element -> FilePath -> (Element -> [String]) -> IO ()
compileToFileWith element fileName compiler =
  writeFile fileName $ unlines $ compiler element

compileToC :: Element -> [String]
compileToC (Main b) =
    [ "#include <stdio.h>"
    , ""
    , "int main(int argc, char **argv)"
    , "{" ] ++
    compileToC b ++
    [ "return 0;"
    , "}"
    , "" ]
compileToC (Block xs) = concat $ map (compileToC) xs
compileToC (Print s) = [ "printf(\"" ++ (expressionToString s) ++ "\\n\");" ]

compileToJS :: Element -> [String]
compileToJS (Main b) = compileToJS b
compileToJS (Block xs) = concat $ map (compileToJS) xs
compileToJS (Print s) = ["console.log(\"" ++ (expressionToString s) ++ "\");"]

compileToBASH :: Element -> [String]
compileToBASH (Main b) = compileToBASH b
compileToBASH (Block xs) = concat $ map (compileToBASH) xs
compileToBASH (Print s) = ["printf \"" ++ (expressionToString s) ++ "\\n\""]
