module Error where

data Error = ReadOnlySymbolError String
           | ReadOnlyTableError  String
           | DivideByZeroError   String
           | InternalError       String
           | SymbolNotFoundError String
           | TypeMismatchError   String
           deriving (Eq, Ord, Show)

errMsgDivideByZero = "Divide by 0 error"

errMsgNoStringInNum = "String cannot be used in a numeric expression"
errMsgNoBooleanInNum = "Boolean cannot be used in a numeric expression"

errMsgNoStringInBool = "String cannot be used in a boolean expression"
errMsgNoNumberInBool = "Number cannot be used in a boolean expression"

errMsgSymbolNotFoundError x = "Symbol not found: " ++ x

errMsgSymbolReadOnly x = "Cannot modify read-only symbol: " ++ x
errMsgTableReadOnly = "Cannot modify an immutable symbol table"

mkSymbolNotFoundError x = SymbolNotFoundError $ errMsgSymbolNotFoundError x
mkReadOnlySymbolError x = ReadOnlySymbolError $ errMsgSymbolReadOnly x
mkReadOnlyTableError = ReadOnlyTableError errMsgTableReadOnly
