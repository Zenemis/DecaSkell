module Lexer (
    scan
) where

import Lexer.Header
import Lexer.NonKeyword

import Error (LexicalError(..), handleLexicalErrors)

import File

import Data.List (isPrefixOf)

-- Fonction scan
scan :: File -> [Token]
scan (FileCons "" _)           = []
scan (FileCons ((' '):rest) l) = scan (FileCons rest l)
scan file@(FileCons code line)
  | "void"      <| code = VOID : scan (advance 4 file)
  | "int"       <| code = INT : scan (advance 3 file)
  | "double"    <| code = DOUBLE : scan (advance 6 file)
  | "bool"      <| code = BOOL : scan (advance 4 file)
  | "string"    <| code = STRING : scan (advance 6 file)
  | "null"      <| code = NULL : scan (advance 4 file)
  | "class"     <| code = CLASS : scan (advance 5 file)
  | "interface" <| code = INTERFACE : scan (advance 9 file)
  | "this"      <| code = THIS : scan (advance 4 file)
  | "extends"   <| code = EXTENDS : scan (advance 7 file)
  | "implements" <| code = IMPLEMENTS : scan (advance 10 file)
  | "for"       <| code = FOR : scan (advance 3 file)
  | "while"     <| code = WHILE : scan (advance 5 file)
  | "if"        <| code = IF : scan (advance 2 file)
  | "elif"      <| code = ELIF : scan (advance 7 file)
  | "else"      <| code = ELSE : scan (advance 4 file)
  | "return"    <| code = RETURN : scan (advance 6 file)
  | "break"     <| code = BREAK : scan (advance 5 file)
  | "New"       <| code = NEW : scan (advance 3 file)
  | "NewArray"  <| code = NEWARRAY : scan (advance 8 file)
  | "Print"     <| code = PRINT : scan (advance 5 file)
  | "ReadInteger" <| code = READINTEGER : scan (advance 11 file)
  | "ReadLine"  <| code = READLINE : scan (advance 8 file)
  | "//" << code = case ignoreComment code of
                     Left err -> handleLexicalErrors err line
                     Right n  -> SINGCOMM : scan (advance n file)
  | "/*" << code = case ignoreComment code of
                     Left err -> handleLexicalErrors err line
                     Right n  -> OPENCOM : scan (advance n file)
  | "*/"        << code = CLOSECOM : scan (advance 2 file)
  | "+"         << code = PLUS : scan (advance 1 file)
  | "-"         << code = MINUS : scan (advance 1 file)
  | "*"         << code = TIMES : scan (advance 1 file)
  | "/"         << code = DIV : scan (advance 1 file)
  | "%"         << code = MODULO : scan (advance 1 file)
  | "<="        << code = LEQU : scan (advance 2 file)
  | "<"         << code = LTHAN : scan (advance 1 file)
  | ">="        << code = GEQU : scan (advance 2 file)
  | ">"         << code = GTHAN : scan (advance 1 file)
  | "=="        << code = EQUAL : scan (advance 2 file)
  | "!="        << code = NEQUAL : scan (advance 2 file)
  | "&&"        << code = AND : scan (advance 2 file)
  | "||"        << code = OR : scan (advance 2 file)
  | "!"         << code = NOT : scan (advance 1 file)
  | ";"         << code = SEMICOLON : scan (advance 1 file)
  | "\n"        << code = EOL : scan (advance 1 file)
  | ","         << code = COMMA : scan (advance 1 file)
  | "."         << code = DOT : scan (advance 1 file)
  | "="         << code = ASSIGN : scan (advance 1 file)
  | "[]"        << code = BRACKS : scan (advance 2 file)
  | "["         << code = OPENBRAKT : scan (advance 1 file)
  | "]"         << code = CLOSEBRAKT : scan (advance 1 file)
  | "("         << code = OPENPAR : scan (advance 1 file)
  | ")"         << code = CLOSEPAR : scan (advance 1 file)
  | "{"         << code = OPENBRACE : scan (advance 1 file)
  | "}"         << code = CLOSEBRACE : scan (advance 1 file)
  | "\\"        << code = BSLASH : scan (advance 1 file)
  | otherwise = case buildNonKeyword code of
      Left err -> handleLexicalErrors err line
      Right (token, count) -> token : scan (advance count file)



ignoreComment :: String -> Either LexicalError Int
ignoreComment ('/':'*':xs) = fmap (2 +) (ignoreCommentWhileOpened xs)
ignoreComment ('/':'/':xs) = Right (length (takeWhile (/= '\n') xs))
ignoreComment _ = Right 0

ignoreCommentWhileOpened :: String -> Either LexicalError Int
ignoreCommentWhileOpened [] = Left (CommentError "Comment opened but not closed")
ignoreCommentWhileOpened s@(x:xs)
  | "*/" `isPrefixOf` s = Right 2
  | otherwise           = fmap (1 +) (ignoreCommentWhileOpened xs)
