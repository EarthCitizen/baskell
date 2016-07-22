module Error where

data LangageError = DivideByZeroError String | TypeMismatchError String deriving (Eq, Ord, Show)
