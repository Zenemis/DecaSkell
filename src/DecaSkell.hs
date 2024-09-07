module Main where

import System.Environment (getArgs)
import System.IO (readFile)

import Lexer

main :: IO ()
main = do
    -- Get the file path from the command line arguments
    args <- getArgs
    case args of
        [filePath] -> do
            -- Read the content of the file
            content <- readFile filePath
            -- Run the scanner on the content
            let tokens = scan content
            -- Print the resulting tokens
            print tokens
            -- print content
        _ -> putStrLn "Usage: runhaskell Scanner.hs <filePath>"
