module Error where

data LanguageError = BuiltInSymbolError  String
                   | DivideByZeroError   String
                   | SymbolNotFoundError String
                   | TypeMismatchError   String
                   deriving (Eq, Ord, Show)

errMsgDivideByZero = "Divide by 0 error"

errMsgNoStringInNum = "String cannot be used in a numeric expression"
errMsgNoBooleanInNum = "Boolean cannot be used in a numeric expression"

errMsgNoStringInBool = "String cannot be used in a boolean expression"
errMsgNoNumberInBool = "Number cannot be used in a boolean expression"
