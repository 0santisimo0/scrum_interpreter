module ErrorHandlers.ErrorHandler (
    identifierError,
    integerError,
    floatError,
    booleanError,
    stringLiteralError,
    literalError,
    binaryOperatorError,
    binaryExpressionError,
    elementError,
    listExpressionError,
    iterableError,
    forLoopError,
    userStoryTypeError,
    userStoryFormatBlockError,
    userStoryError
) where

import Text.Parsec (ParseError)

identifierError :: ParseError -> String
identifierError err = "Error parsing identifier: " ++ show err

integerError :: ParseError -> String
integerError err = "Error parsing integer: " ++ show err

floatError :: ParseError -> String
floatError err = "Error parsing float: " ++ show err

booleanError :: ParseError -> String
booleanError err = "Error parsing boolean: " ++ show err

stringLiteralError :: ParseError -> String
stringLiteralError err = "Error parsing string literal: " ++ show err

literalError :: ParseError -> String
literalError err = "Error parsing literal: " ++ show err

binaryOperatorError :: ParseError -> String
binaryOperatorError err = "Error parsing binary operator: " ++ show err

binaryExpressionError :: ParseError -> String
binaryExpressionError err = "Error parsing binary expression: " ++ show err

elementError :: ParseError -> String
elementError err = "Error parsing element: " ++ show err

listExpressionError :: ParseError -> String
listExpressionError err = "Error parsing list expression: " ++ show err

iterableError :: ParseError -> String
iterableError err = "Error parsing iterable: " ++ show err

forLoopError :: ParseError -> String
forLoopError err = "Error parsing for loop: " ++ show err

userStoryTypeError :: ParseError -> String
userStoryTypeError err = "Error parsing user story type: " ++ show err

userStoryFormatBlockError :: ParseError -> String
userStoryFormatBlockError err = "Error parsing user story format block: " ++ show err

userStoryError :: ParseError -> String
userStoryError err = "Error parsing user story: " ++ show err
