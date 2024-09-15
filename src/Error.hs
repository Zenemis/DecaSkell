module Error (
    LexicalError(..),
    handleLexicalErrors
) where

import Control.Exception (Exception)

import File

data LexicalError
    = TokenError String
    | IdentifierError String
    | LitteralError String
    | CommentError String
    deriving (Show, Eq)

instance Exception LexicalError

handleLexicalErrors :: LexicalError -> Line -> [a]
handleLexicalErrors err line = error ("At line " ++ show line ++ " || " ++ show err)