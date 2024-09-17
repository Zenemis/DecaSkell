module Parser (
    parse,
    AST
) where

import Parser.AST

parse :: [Token] -> AST
parse _ = Nil
