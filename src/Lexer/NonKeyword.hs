module Lexer.NonKeyword where

import Data.List (isPrefixOf, isInfixOf)
import Data.Char (isAlpha, isAlphaNum, isDigit, toLower, count)

import Lexer.Header

import Lexer.NonKeyword.NKAlpha
import Lexer.NonKeyword.NKNumber
import Lexer.NonKeyword.NKString

--------------------
-- Core functions --
--------------------

buildNonKeyword :: String -> Token
buildNonKeyword [] = error "buildNonKeyword : Empty string cannot be a token"
buildNonKeyword s@(x:xs)
  | isAlpha x = buildAlpha s
  | isDigit x = buildNumber s
  | x == '"'  = buildString s
  | otherwise = error "buildNonKeyword : unknown string " ++ take 31 xs
