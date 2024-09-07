module Lexer (
    scan
) where

import Lexer.Header
import Lexer.NonKeyword

type File = String

-- Fonction scan
scan :: File -> [Token]
scan ""                 = []
scan ((' '):rest)       = scan rest
scan file
  | "void"      <| file = VOID : scan (drop 4 file)
  | "int"       <| file = INT : scan (drop 3 file)
  | "double"    <| file = DOUBLE : scan (drop 6 file)
  | "bool"      <| file = BOOL : scan (drop 4 file)
  | "string"    <| file = STRING : scan (drop 6 file)
  | "null"      <| file = NULL : scan (drop 4 file)
  | "class"     <| file = CLASS : scan (drop 5 file)
  | "interface" <| file = INTERFACE : scan (drop 9 file)
  | "this"      <| file = THIS : scan (drop 4 file)
  | "extends"   <| file = EXTENDS : scan (drop 7 file)
  | "implements" <| file = IMPLEMENTS : scan (drop 10 file)
  | "for"       <| file = FOR : scan (drop 3 file)
  | "while"     <| file = WHILE : scan (drop 5 file)
  | "if"        <| file = IF : scan (drop 2 file)
  | "elif"      <| file = ELIF : scan (drop 7 file)
  | "else"      <| file = ELSE : scan (drop 4 file)
  | "return"    <| file = RETURN : scan (drop 6 file)
  | "break"     <| file = BREAK : scan (drop 5 file)
  | "new"       <| file = NEW : scan (drop 3 file)
  | "newArray"  <| file = NEWARRAY : scan (drop 8 file)
  | "print"     <| file = PRINT : scan (drop 5 file)
  | "readInteger" <| file = READINTEGER : scan (drop 11 file)
  | "readLine"  <| file = READLINE : scan (drop 8 file)
  | "+"         << file = PLUS : scan (drop 1 file)
  | "-"         << file = MINUS : scan (drop 1 file)
  | "*"         << file = TIMES : scan (drop 1 file)
  | "/"         << file = DIV : scan (drop 1 file)
  | "%"         << file = MODULO : scan (drop 1 file)
  | "<="        << file = LEQU : scan (drop 2 file)
  | "<"         << file = LTHAN : scan (drop 1 file)
  | ">="        << file = GEQU : scan (drop 2 file)
  | ">"         << file = GTHAN : scan (drop 1 file)
  | "=="        << file = EQUAL : scan (drop 2 file)
  | "!="        << file = NEQUAL : scan (drop 2 file)
  | "&&"        << file = AND : scan (drop 2 file)
  | "||"        << file = OR : scan (drop 2 file)
  | "!"         << file = NOT : scan (drop 1 file)
  | ";"         << file = SEMICOLON : scan (drop 1 file)
  | "\n"        << file = EOL : scan (drop 1 file)
  | ","         << file = COMMA : scan (drop 1 file)
  | "."         << file = DOT : scan (drop 1 file)
  | "="         << file = ASSIGN : scan (drop 1 file)
  | "[]"        << file = BRACKS : scan (drop 2 file)
  | "["         << file = OPENBRAKT : scan (drop 1 file)
  | "]"         << file = CLOSEBRAKT : scan (drop 1 file)
  | "("         << file = OPENPAR : scan (drop 1 file)
  | ")"         << file = CLOSEPAR : scan (drop 1 file)
  | "{"         << file = OPENBRACE : scan (drop 1 file)
  | "}"         << file = CLOSEBRACE : scan (drop 1 file)
  | "\\"        << file = BSLASH : scan (drop 1 file)
  | "//"        << file = SINGCOMM : scan (drop 2 file)
  | "/*"        << file = OPENCOM : scan (drop 2 file)
  | "*/"        << file = CLOSECOM : scan (drop 2 file)
  | otherwise   = 
    let (token, count) = buildNonKeyword file
    in token : scan (drop count file)