module Lexer.NonKeyword where

import Data.Char (isAlpha, isDigit)

import Lexer.Header

import Lexer.NonKeyword.NKAlpha
import Lexer.NonKeyword.NKNumber
import Lexer.NonKeyword.NKString

--------------------
-- Core functions --
--------------------

buildNonKeyword :: String -> (Token, Int)
buildNonKeyword [] = error "buildNonKeyword : Empty string cannot be a token"
buildNonKeyword s@(x:_)
  | isAlpha x = buildAlpha s
  | isDigit x = buildNumber s
  | x == '"'  = buildString s
  | otherwise = error ("buildNonKeyword : unknown word not recognized neither as a litteral nor an id " ++ take 31 s)
