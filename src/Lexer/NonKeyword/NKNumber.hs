module Lexer.NonKeyword.NKNumber where

import Lexer.NonKeyword.NKHex
import Lexer.NonKeyword.NKFloat

import Lexer.Header (Token(..))

import Error (LexicalError(..))

import Data.Char (isDigit, toLower)

-- Fonction pour vérifier qu'un caractère construit bien un nombre
isNumDigit :: Char -> Bool
isNumDigit c = isDigit c || toLower c == 'e' || c == '.'

-- Fonction pour construire un token numérique à partir du préfixe (intlitt / doublelitt)
buildNumber :: String -> Either LexicalError (Token, Int)
buildNumber [_] = Right (INTLITT, 1)
buildNumber ('0':'x':xs) = handleHexResult (buildHex xs) 2
buildNumber ('0':'X':xs) = handleHexResult (buildHex xs) 2
buildNumber s            = buildNumberCut s

-- Fonction pour traiter le résultat de buildHex et ajuster la taille
handleHexResult :: Either LexicalError (Token, Int) -> Int -> Either LexicalError (Token, Int)
handleHexResult (Right (token, size)) offset = Right (token, size + offset)
handleHexResult (Left err) _ = Left err

-- Fonction pour construire un token numérique à partir de la suite (intlitt / doublelitt)
buildNumberCut :: String -> Either LexicalError (Token, Int)
buildNumberCut s
  | all isDigit lexem  = Right (INTLITT, length lexem)
  | isDoubleLitt lexem = Right (DOUBLELITT, length lexem)
  | otherwise       = Left (LitteralError "Invalid number litteral")
  where (lexem, _) = span isNumDigit s