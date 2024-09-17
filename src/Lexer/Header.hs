module Lexer.Header (
    Token(..),
    (<<),
    (<|)
) where

import Data.List (isPrefixOf)

separators :: [Char]
separators = [' ', '\n', '\t', ';', ',', '(', ')', '{', '}', '[', ']', 
              '=', '<', '>', '!', '+', '-', '*', '/', '%', '&', '|']

-- Vérification faible de préfixe 
-- Vérifie seulement s'il s'agit d'un préfixe
(<<) :: String -> String -> Bool
(<<) = isPrefixOf

-- Vérification forte de préfixe
-- Vérifie aussi qu'il s'agit bien d'un mot-clef réservé (le caractère suivant est un séparateur)
(<|) :: String -> String -> Bool
(<|) _ [] = False
(<|) prefix str =
    let rest = drop (length prefix) str
    in (prefix `isPrefixOf` str) &&
       (case rest of
          (c:_) -> c `elem` separators
          []    -> False)

-- Définition des différents types de tokens
data Token
  = VOID      -- Types
  | INT
  | DOUBLE
  | BOOL
  | STRING
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
  | BSLASH    -- ID / Literals 
  | ID
  | TRUE
  | FALSE
  | INTLITT
  | DOUBLELITT
  | STRLITT
  | NULL
  | OPENCOM   -- Misc
  | CLOSECOM
  | SINGCOMM
  deriving (Show, Eq, Enum)