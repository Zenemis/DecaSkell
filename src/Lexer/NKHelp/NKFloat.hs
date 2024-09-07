module Lexer.NonKeyword.NKNumber.NKFloat where

import Data.List (isInfixOf)
import Data.Char (isDigit)

-- Fonction pour vérifier si une chaîne est un littéral flottant
isDoubleLitt :: String -> Bool
isDoubleLitt s
  | "E" `isInfixOf` s || "e" `isInfixOf` s = isValidScientific s
  | otherwise = isValidDouble s

-- Fonction pour vérifier si une chaîne est un flottant SANS exposants
isValidDouble :: String -> Bool
isValidDouble s
  | '.' `elem` s && all isDigit (filter (/= '.') s) = True
  | otherwise = False

-- Fonction pour vérifier si une chaîne est un flottant AVEC exposants
isValidScientific :: String -> Bool
isValidScientific s = let (mantissa, exponentPart) = break (\c -> c == 'E' || c == 'e') s
                          exponent = drop 1 exponentPart
                      in isValidDouble mantissa && isValidExponent exponent

-- Fonction pour vérifier si une chaîne est un exposant valide
isValidExponent :: String -> Bool
isValidExponent ('+':xs) = all isDigit xs
isValidExponent ('-':xs) = all isDigit xs
isValidExponent xs       = all isDigit xs