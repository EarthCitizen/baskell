module Compile (compileToBASH, compileToC, compileToJS, compileToFileWith) where

import System.IO
import AST
import Eval

compileToFileWith :: AST -> FilePath -> (AST -> [String]) -> IO ()
compileToFileWith ast fileName compiler =
  writeFile fileName $ unlines $ compiler ast

-- C

compileToC :: AST -> [String]
compileToC (AST m) = compileMainToC m

compileMainToC :: Main -> [String]
compileMainToC (Main b) =
    [ "#include <stdio.h>"
    , ""
    , "int main(int argc, char **argv)"
    , "{" ] ++
    compileBlockToC b ++
    [ "return 0;"
    , "}"
    , "" ]

compileBlockToC :: Block -> [String]
compileBlockToC (Block xs) = concat $ map (compileStatementToC) xs

compileStatementToC :: Statement -> [String]
compileStatementToC (PrintStatement s) = [ "printf(\"" ++ (expressionToString s) ++ "\\n\");" ]

-- JS

compileToJS :: AST -> [String]
compileToJS (AST m) = compileMainToJS m

compileMainToJS :: Main -> [String]
compileMainToJS (Main b) = compileBlockToJS b

compileBlockToJS :: Block -> [String]
compileBlockToJS (Block xs) = concat $ map (compileStatementToJS) xs

compileStatementToJS :: Statement -> [String]
compileStatementToJS (PrintStatement s) = ["console.log(\"" ++ (expressionToString s) ++ "\");"]

-- BASH

compileToBASH :: AST -> [String]
compileToBASH (AST m) = compileMainToBASH m

compileMainToBASH :: Main -> [String]
compileMainToBASH (Main b) = compileBlockToBASH b

compileBlockToBASH :: Block -> [String]
compileBlockToBASH (Block xs) = concat $ map (compileStatementToBASH) xs

compileStatementToBASH :: Statement -> [String]
compileStatementToBASH (PrintStatement s) = ["printf \"" ++ (expressionToString s) ++ "\\n\""]
