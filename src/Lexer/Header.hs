module Lexer.Header (
    Token(..),
    (<<),
    (<|)
) where

import Data.List (isPrefixOf)

separators = [' ', '\n', '\t', ';', ',', '(', ')', '{', '}', '[', ']', 
              '=', '<', '>', '!', '+', '-', '*', '/', '%', '&', '|']

-- Vérification faible de préfixe 
-- Vérifie seulement s'il s'agit d'un préfixe
(<<) :: String -> String -> Bool
(<<) = isPrefixOf

-- Vérification forte de préfixe
-- Vérifie aussi qu'il s'agit bien d'un mot-clef réservé (le caractère suivant est un séparateur)
(<|) :: String -> String -> Bool
(<|) prefix str = (prefix `isPrefixOf` str) && 
                  (let (c:s) = drop (length prefix) str
                  in c `elem` separators)

-- Définition des différents types de tokens
data Token
  = VOID      -- Types
  | INT
  | DOUBLE
  | BOOL
  | STRING
  | NULL
  | CLASS     -- OOP
  | INTERFACE
  | THIS
  | EXTENDS
  | IMPLEMENTS
  | FOR       -- Control Flow
  | WHILE
  | IF
  | ELSE
  | ELIF
  | RETURN
  | BREAK
  | NEW       -- Lang operations
  | NEWARRAY
  | PRINT
  | READINTEGER
  | READLINE
  | ASSIGN
  | PLUS      -- Math operators
  | MINUS
  | TIMES
  | DIV
  | MODULO
  | LTHAN     -- Bool operators
  | LEQU
  | GTHAN
  | GEQU
  | EQUAL
  | NEQUAL
  | AND
  | OR
  | NOT
  | SEMICOLON -- Separators
  | EOL
  | COMMA
  | DOT
  | BRACKS
  | OPENBRAKT
  | CLOSEBRAKT
  | OPENPAR
  | CLOSEPAR
  | OPENBRACE
  | CLOSEBRACE
  | BSLASH    -- ID / Literals / Misc
  | ID
  | TRUE
  | FALSE
  | INTLITT
  | DOUBLELITT
  | STRLITT
  | OPENCOM
  | CLOSECOM
  | SINGCOMM
  deriving (Show, Eq, Enum)