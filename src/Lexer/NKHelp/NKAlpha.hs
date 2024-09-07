module Lexer.NonKeyword.NKAlpha where

import Lexer.Header (Token(..))

import Data.Char (isAlpha, isAlphaNum)

-- Fonction pour vérifier si un caractère est valide dans un identifiant
isValidChar :: Char -> Bool
isValidChar c = isAlphaNum c || c == '_'

-- Fonction pour vérifier si une chaîne est un identifiant valide
isID :: String -> Bool
isID str = case str of
  [] -> False
  (x:xs) -> isAlpha x && all isValidChar xs && length str <= 31

-- Fonction qui construit un token alphanumérique (id / boolitt)
buildAlpha :: String -> (Token, Int)
buildAlpha s@(x:xs) 
  | lexem == "true"     = (TRUE, 4)
  | lexem == "false"    = (FALSE, 5)
  | isID lexem          = (ID, length lexem)
  where (lexem, rest) = span isValidChar s