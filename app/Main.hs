module Main where
import Parser
import Assembler
import System.Environment
import System.Exit
import System.Process

tokenizeList :: [String] -> IO [Token]
tokenizeList = return . (fmap tokenize . formatLines)

compileFail :: Bool -> IO ()
compileFail True = return ()
compileFail False = do
                      print "Compiling Is Unable To Be Done" 
                      exitWith (ExitFailure 255)

main :: IO ()
main = do
  tokenizedList <- tokenizedListIO
  filePath <- filePathIO
  compileFail $ isCompileable tokenizedList
  makeIntoHaskell filePath $ foldr (<>) "" $ reverse $ map removeMaybe tokenizedList
  callCommand ("ghc " <> changeName filePath ".hs")
  callCommand ("rm " <> changeName filePath ".hi")
  callCommand ("rm " <> changeName filePath ".o")
  where 
    filePathIO = fmap head getArgs
    unformatedLines = getArgs >>= readLines
    tokenizedListIO = unformatedLines >>= tokenizeList
