{-# LANGUAGE OverloadedStrings  #-}
import Control.Applicative((<*))
import Control.Monad  (liftM)
import Data.Attoparsec
import Data.Attoparsec.Char8
import qualified Data.ByteString.Char8 as B
import System.Environment
import Data.List
-- Parser 

data Output = Guess Int | Bad | Cheating 


myshow (Guess x) = show x
myshow (Bad) = "Bad magician!"
myshow (Cheating) = "Volunteer cheated!"

parseNumber = decimal

parseLine = manyTill' (parseNumber <* many' (string " ")) endOfLine <?> "line da grid"
parseGrid = count 4 parseLine <?> "grid"

parseCase =do
        guess1<- parseNumber <* endOfLine
        grid1 <- parseGrid
        guess2<- parseNumber <* endOfLine
        grid2 <- parseGrid
        return (guess1,grid1,guess2,grid2)
        
parseProblem = do
        size <- parseNumber <* endOfLine
        problems <- count size parseCase
        skipSpace
        return problems
        
 
translate input = case parseOnly parseProblem  input of
        Left con -> error con
        Right r -> r

parserTest = do
        print $ parseOnly parseLine "1  2  3  4\n"
        print $ parseOnly parseCase "3\n1  2  3  4\n1  2  3  4\n1  2  3  4\n1  2  3  4\n4\n1  2  3  4\n1  2  3  4\n1  2  3  4\n1  2  3  4\n"
        print $ parseOnly parseProblem "1\n3\n1  2  3  4\n1  2  3  4\n1  2  3  4\n1  2  3  4\n4\n1  2  3  4\n1  2  3  4\n1  2  3  4\n1  2  3  4\n"
-- cmdArg test 
cmdArg []  = error "\nNecessario um argumento com extensão"
cmdArg (x:xs) = x

testing :: [Int]-> [Int] -> [Int]
testing xs ys = filter (`elem` xs) ys 

magician []  = Cheating
magician [x] = Guess x
magician _   =  Bad

run (guess1,grid1, guess2, grid2) = testing (grid1 !! (guess1-1))  (grid2 !! (guess2-1)) 


-- parserTest optional        
main = do
        parserTest
        
        fileName <- return . cmdArg =<< getArgs
        contents <- liftM translate $ B.readFile fileName-- input
        B.writeFile "results.txt" $ helper contents
        putStrLn "\nDone\n"
        where 
        helper  input = B.unlines $  zipWith doline [1..] $ map (myshow . magician . run)  input
        doline a b = B.pack $ "Case #"++show a++": "++ b