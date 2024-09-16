module Error (
    LexicalError(..),
    SyntaxError(..),
    TypeError(..),
    handleErrors
) where

import Control.Exception (Exception)

import File

-- LexicalError type
data LexicalError
    = TokenError String
    | IdentifierError String
    | LitteralError String
    | CommentError String
    deriving (Show, Eq)

-- SyntaxError type
data SyntaxError
    = UnexpectedToken String
    | MissingToken String
    deriving (Show, Eq)

-- TypeError type
data TypeError
    = MismatchedTypes String
    | UndefinedVariable String
    deriving (Show, Eq)

-- Instances of Exception for each error type
instance Exception LexicalError
instance Exception SyntaxError
instance Exception TypeError

-- Unified error handler that only works with Exception types
handleErrors :: (Exception e) => e -> Line -> [a]
handleErrors err line = error ("At line " ++ show line ++ " || " ++ show err)
