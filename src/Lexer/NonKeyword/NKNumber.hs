module Lexer.NonKeyword.NKNumber where

import Lexer.NonKeyword.NKHex
import Lexer.NonKeyword.NKFloat

import Lexer.Header (Token(..))

import Data.Char (isDigit, toLower)

-- Fonction pour vérifier qu'un caractère construit bien un nombre
isNumDigit :: Char -> Bool
isNumDigit c = isDigit c || toLower c == 'e' || c == '.'

-- Fonction pour construire un token numérique à partir du préfix (intlitt / doublelitt)
buildNumber :: String -> (Token, Int)
buildNumber [_] = (INTLITT, 1)
buildNumber ('0':'x':xs) = let (token, size) = buildHex xs in (token, size+2)
buildNumber ('0':'X':xs) = let (token, size) = buildHex xs in (token, size+2)
buildNumber s            = buildNumberCut s

-- Fonction pour construire un token numérique à partir de la suite (intlitt / doublelitt)
buildNumberCut :: String -> (Token, Int)
buildNumberCut s
  | all isDigit lexem  = (INTLITT, length lexem)
  | isDoubleLitt lexem = (DOUBLELITT, length lexem)
  | otherwise       = error "Invalid number litteral"
  where (lexem, _) = span isNumDigit s