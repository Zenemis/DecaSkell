module Lexer.NonKeyword.NKAlpha where

import Lexer.Header (Token(..))

import Error (LexicalError(..))

import Data.Char (isAlpha, isAlphaNum)

-- Fonction pour vérifier si un caractère est valide dans un identifiant
isValidChar :: Char -> Bool
isValidChar c = isAlphaNum c || c == '_'

-- Fonction pour vérifier si une chaîne est un identifiant valide
isID :: String -> Bool
isID str = case str of
  [] -> False
  (x:xs) -> isAlpha x && all isValidChar xs && length str <= 31

-- Fonction qui construit un token alphanumérique (id)
buildAlpha :: String -> Either LexicalError (Token, Int)
buildAlpha []           = Left (TokenError ("Empty lexem"))
buildAlpha s
  | isID lexem          = Right (ID, length lexem)
  | otherwise           = Left (IdentifierError ("Malformed identifier : '" ++ lexem ++ "'"))
  where (lexem, _) = span isValidChar s