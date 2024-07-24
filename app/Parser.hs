module Parser where

type Token = Maybe String

readLines :: [String] -> IO [String]
readLines [] = pure ["No arguments Provided"]
readLines file = lines <$> readFile (head file)

tokenize :: [String] -> Token
tokenize ["PUSH", x]  = Just (" ("++x++") ")
tokenize ["ADD"]      = Just " $ (+) "
tokenize ["SUB"]      = Just " $ (-) "
tokenize ["PRINT", x] = Just (" print " ++ x)
tokenize ["PRINT"]    = Just " print "
tokenize ["DIV"]      = Just " $ (/) "
tokenize ["MUL"]      = Just " $ (*) "
tokenize _            = Nothing


formatLines :: [String] -> [[String]]
formatLines = map (words . removeTrailingWhiteSpace) . removeEmptyLines
              where 
                removeTrailingWhiteSpace (' ':xs) = removeTrailingWhiteSpace xs
                removeTrailingWhiteSpace xs = xs
                removeEmptyLines = foldr (\x acc -> if null x then acc else x:acc) []

isCompileable :: [Token] -> Bool
isCompileable list = sumBoolList (map compileable list) == length list 
                  && checkFinalStatment list && multiplePrints list
                  where 
                    sumBoolList = sum . map fromEnum
                    compileable Nothing = False
                    compileable _ = True
                    multiplePrints xs = 1 == length (filter ((== Just " print ") . fmap (take 7)) xs)
                    checkFinalStatment xs = fmap (take 7) (last xs) == Just " print "

removeMaybe :: Token -> String
removeMaybe Nothing = ""
removeMaybe (Just x) = x


