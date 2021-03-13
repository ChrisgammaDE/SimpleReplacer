import System.Environment
import System.IO
import System.Directory
import Data.Maybe
import Data.List (findIndex)

import Replacer


convertContent :: [(String, String)] -> String -> String
convertContent configs content =
        foldl (\acc cur -> convert cur) "" configs
    where
        convert conf = uncurry replace conf content

convertContents configs = 
    return . unlines . map (convertContent confs) . lines
    where
        confs = parseConfigs configs



convertFile :: [String] -> FilePath -> IO ()
convertFile configs path = do
    fileExists <- doesFileExist path
    if fileExists then do

        contents    <- readFile path
        newContent  <- convertContents configs contents
        writeFile (path ++ ".fixed.txt") newContent

        putStrLn "Done :)"
    else
        error "404 - File not found"


main :: IO ()
main = do
    config <- lines <$> readIfExisting defMappings "mappings.txt"

    args <- getArgs
    if length args == 0 then do
        -- Verbal Mode
        putStrLn "Please enter the Filename:"
        file <- getLine
        convertFile config file

    else do
        -- Argument Mode
        sequence_ (map (convertFile config) args)


    -- if doesFileExist file then


    -- let content = unlines' . insertCC .  dropInvalidLines . lines $


    putStrLn $ "Now have fun subtitling!\n"

    putStrLn "You can close this window now"
    idk <- getChar
    putStrLn "Bye"



-- Helpers

readIfExisting :: [String] -> FilePath -> IO String
readIfExisting def path = do
    existent <- doesFileExist path
    if existent then do
        contents <- readFile path
        return contents
    else do
        writeFile path $ unlines def
        return $ unlines def




splitOn :: Eq e => e -> [e] -> ([e], [e])
splitOn e l =
    case t of
        [] -> (h, [])
        (x:_) -> (h, tail t)
    where
        (h, t) =
            splitAt (fromMaybe 0 (findIndex (== e) l)) l

parseConfigs :: [String] -> [(String, String)]
parseConfigs [] = []
parseConfigs (x:xs) = 
    let
        (pat, supp) = splitOn '>' x
    in
    (reverse $ drop 1 $ reverse $ pat, supp) : parseConfigs xs



-- Default Files
defMappings :: [String]
defMappings =
    [ "Hello->Hallo"
    , "World->Welt"
    , " I -> i "
    ]