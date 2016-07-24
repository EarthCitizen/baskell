{-# LANGUAGE FlexibleContexts #-}

module Parse (parseFile, ParseError) where

import AST

import qualified Text.Parsec as Parsec hiding (ParseError)
import Text.Parsec (ParseError, (<?>), (<|>))
import Text.Parsec.String (parseFromFile)

spaceChar :: Parsec.Parsec String () Char
spaceChar = Parsec.char ' ' <?> "space"

doubleQuoteChar :: Parsec.Parsec String () Char
doubleQuoteChar = Parsec.char '"' <?> "double-quote"

whiteSpace :: Parsec.Parsec String () Char
whiteSpace = Parsec.choice [spaceChar, Parsec.endOfLine]


escapedCharToActual :: Char -> Char
escapedCharToActual 'b' = '\b'
escapedCharToActual 'n' = '\n'
escapedCharToActual 't' = '\t'
escapedCharToActual 'r' = '\r'

escapedChar :: Parsec.Parsec String () Char
escapedChar = do
    Parsec.char '\\'
    ec <- Parsec.oneOf "bntr"
    return $ escapedCharToActual ec

quotedString :: Parsec.Parsec String () Expression
quotedString = do
    doubleQuoteChar
    qs <- Parsec.many $ Parsec.choice [escapedChar, Parsec.noneOf "\\\""]
    doubleQuoteChar
    return $ StringValue qs

booleanLiteral :: Parsec.Parsec String () Expression
booleanLiteral = do
    bs <- Parsec.choice [Parsec.string "true", Parsec.string "false"]
    return $ BooleanValue $ convBool bs
    where convBool "true" = True
          convBool "false" = False

integerLiteral :: Parsec.Parsec String () Expression
integerLiteral = do
    is <- Parsec.many1 Parsec.digit
    return $ IntegerValue (read is :: Integer)

expression :: Parsec.Parsec String () Expression
expression = Parsec.choice [quotedString, booleanLiteral, integerLiteral]

printStatement :: Parsec.Parsec String () Statement
printStatement = do
    Parsec.string "print"
    Parsec.many1 Parsec.space
    ev <- expression
    return (PrintStatement ev)

endOfStatement :: Parsec.Parsec String () ()
endOfStatement = do
    Parsec.skipMany spaceChar
    Parsec.skipMany1 Parsec.endOfLine <|> Parsec.eof
    return ()

endOfFile :: Parsec.Parsec String () ()
endOfFile = do
    Parsec.skipMany whiteSpace
    Parsec.eof
    return ()

statement :: Parsec.Parsec String () Statement
statement = do
    Parsec.skipMany whiteSpace
    ast <- printStatement <?> "print command"
    endOfStatement
    return ast

programFile :: Parsec.Parsec String () AST
programFile = do
    cmds <- Parsec.manyTill statement (Parsec.try endOfFile)
    Parsec.eof
    return $ AST $ Main (Block cmds)

parseFile :: String -> IO (Either ParseError AST)
parseFile = parseFromFile programFile
