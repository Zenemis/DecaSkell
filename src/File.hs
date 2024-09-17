module File (
    File(..),
    Line,
    advance,
    create
) where 

type Line = Int
data File = FileCons String Line
    deriving (Show, Eq)

-- Custom drop function for File
advance :: Int -> File -> File
advance _ file@(FileCons [] _) = file
advance n file@(FileCons (x:xs) line)
   | n > 0     = if (x=='\n')
                 then advance (n-1) (FileCons xs (line+1))
                 else advance (n-1) (FileCons xs line)
   | otherwise = file

create :: String -> File
create s = FileCons s 1