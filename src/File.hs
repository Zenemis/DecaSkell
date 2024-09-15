module File (
    File,
) where 

type File = String

type Line = Int
-- data File = FileCons String Line
--     deriving (Show, Eq)

-- Custom drop function for File
-- advance :: Int -> File -> File
-- advance _ file@(FileCons [] _) = file
-- advance n file@(FileCons (x:xs) line)
--    | n > 0     = if (x=='\n')
--                  then (FileCons (drop xs) (line+1))
--                  else (FileCons (drop xs) line)
--    | otherwise = file
