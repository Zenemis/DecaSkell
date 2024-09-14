module Lexer.NonKeyword.NKString where

import Lexer.Header (Token(..))

-- Fonction pour construire un token chaîne de caractères (strlitt)
buildString :: String -> (Token, Int)
buildString []          = error "String litteral open but not closed, empty lexem"
buildString (_:xs)
    | '\n' `elem` lexem = error ("String litteral cannot be split onto 2 lines, lexem read : '" ++ lexem ++ "'")
    | rest == []        = error ("String litteral open but not closed, lexem read : '" ++ lexem ++ "'")
    | otherwise         = (STRLITT, length lexem + 2)
    where (lexem, rest) = span (/='"') xs