module Lexer.NonKeyword.NKString where

import Lexer.Header (Token(..))

-- Fonction pour construire un token chaîne de caractères (strlitt)
buildString :: String -> (Token, Int)
buildString s@(x:xs) = 
                    if '\n' `elem` lexem
                    then error "String litteral cannot be split onto 2 lines"
                    else (STRLITT, length lexem) 
                    where (lexem, rest) = span (/='"') s