{-# LANGUAGE OverloadedStrings  #-}
import Control.Applicative((<*))
import Control.Monad  (liftM)
import Data.Attoparsec
import Data.Attoparsec.Char8
import qualified Data.ByteString.Char8 as B
import System.Environment
import Data.List
import Text.Printf
-- Parser 
parseNumber :: Parser Double
parseNumber = rational 
parseCase =do
        c <- parseNumber
        string " "
        f <- parseNumber
        string " "
        x <- parseNumber
        endOfLine
        return (c,f,x)
        
parseProblem = do
        size <- decimal <* endOfLine
        problems <- count size parseCase
        skipSpace
        return problems
        
 
translate input = case parseOnly parseProblem  input of
        Left con -> error con
        Right r -> r

parserTest = do
        print $ parseOnly parseCase "30.0 1.0 2.0\n"
        print $ parseOnly parseProblem "1\n30.0 1.0 2.0\n"
-- cmdArg test 
cmdArg []  = error "\nNecessario um argumento com extensão"
cmdArg (x:xs) = x


run (c,f,x) =  if n==0 then x/2 else x/(2+f*n)+sum[c/(2+f*k)| k<-[0..(n-1)]]
    where   
    n = max (calcn c f x) 0
    calcn c f x = fromInteger $ ceiling (x/c-1-2/f)
    

-- parserTest optional        
main = do
        parserTest
        
        fileName <- return . cmdArg =<< getArgs
        contents <- liftM translate $ B.readFile fileName-- input
        B.writeFile "results.txt" $ helper contents
        putStrLn "\nDone\n"
        where 
        helper  input = B.unlines $  zipWith doline [1..] $ map run input
        doline a b = B.pack $ "Case #"++show a++": "++ printf "%.7f" b