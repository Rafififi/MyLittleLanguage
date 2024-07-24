module Assembler where

changeName :: String -> String -> String
changeName filePath diff = takeWhile (/= '.') filePath <> diff

makeIntoHaskell :: FilePath -> String -> IO ()
makeIntoHaskell filePath finalStr = writeFile (changeName filePath ".hs") pastedStr
  where 
    pastedStr = "main = " <> finalStr
