module Error where

data LexicalError
    = TokenError String
    | IdentifierError String
    | LitteralError String
    | CommentError String
    deriving (Show, Eq)