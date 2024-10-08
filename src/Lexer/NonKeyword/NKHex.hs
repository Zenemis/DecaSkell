module Lexer.NonKeyword.NKHex where

import Lexer.Header (Token(..))

import Error (LexicalError(..))

import Data.Char (toLower, isDigit)

-- Fonction pour vérifier qu'un caractère est bien un chiffre hexa
isHexDigit :: Char -> Bool
isHexDigit c = isDigit c || (toLower c >= 'a' && toLower c <= 'f')

-- Fonction pour construire un token hexadécimal (intlitt)
buildHex :: String -> Either LexicalError (Token, Int)
buildHex s = Right (INTLITT, length lexem)
  where (lexem, rest) = span isHexDigit s