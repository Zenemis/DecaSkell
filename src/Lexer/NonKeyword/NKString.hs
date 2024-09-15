module Lexer.NonKeyword.NKString where

import Lexer.Header (Token(..))

import Error (LexicalError(..))

-- Fonction pour construire un token chaîne de caractères (strlitt)
buildString :: String -> Either LexicalError (Token, Int)
buildString []          = Left (LitteralError "String opened but not closed, empty lexem")
buildString (_:xs)
    | '\n' `elem` lexem = Left (LitteralError ("String cannot be split onto 2 lines, lexem read : '" ++ lexem ++ "'"))
    | rest == []        = Left (LitteralError ("String opened but not closed, lexem read : '" ++ lexem ++ "'"))
    | otherwise         = Right (STRLITT, length lexem + 2)
    where (lexem, rest) = span (/='"') xs