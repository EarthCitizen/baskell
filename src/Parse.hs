{-# LANGUAGE FlexibleContexts #-}

module Parse (AST(..), parseFile, ParseError) where

import Element

import qualified Text.Parsec as Parsec hiding (ParseError)
import Text.Parsec (ParseError, (<?>), (<|>))
import Text.Parsec.String (parseFromFile)

data AST = AST Element

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

quotedString :: Parsec.Parsec String () String
quotedString = do
    doubleQuoteChar
    qs <- Parsec.many $ Parsec.choice [escapedChar, Parsec.noneOf "\\\""]
    doubleQuoteChar
    return qs

integerLiteral :: Parsec.Parsec String () String
integerLiteral = Parsec.many1 Parsec.digit

printCommand :: Parsec.Parsec String () Element
printCommand = do
    Parsec.string "print"
    Parsec.many1 Parsec.space
    sv <- Parsec.choice [quotedString, integerLiteral]
    return (Print sv)

endOfCommand :: Parsec.Parsec String () ()
endOfCommand = do
    Parsec.skipMany spaceChar
    Parsec.skipMany1 Parsec.endOfLine <|> Parsec.eof
    return ()

endOfFile :: Parsec.Parsec String () ()
endOfFile = do
    Parsec.skipMany whiteSpace
    Parsec.eof
    return ()

command :: Parsec.Parsec String () Element
command = do
    Parsec.skipMany whiteSpace
    ast <- printCommand <?> "print command"
    endOfCommand
    return ast

commandFile :: Parsec.Parsec String () AST
commandFile = do
    cmds <- Parsec.manyTill command (Parsec.try endOfFile)
    Parsec.eof
    return $ AST (Block cmds)

parseFile :: String -> IO (Either ParseError AST)
parseFile = parseFromFile commandFile
