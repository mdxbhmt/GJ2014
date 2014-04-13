{-# LANGUAGE OverloadedStrings  #-}
import Control.Applicative((<*),(<$>))
import Control.Monad  (liftM)
import Data.Attoparsec
import Data.Attoparsec.Char8
import qualified Data.ByteString.Char8 as B
import System.Environment
import Text.Printf
import qualified Data.Sequence as S

data Barbie = Ken Double | Naomi Double

(Ken b1) `mycompare` (Naomi b2) = b1 `compare` b2
(Naomi b1) `mycompare` (Ken b2) = b1 `compare` b2
(Naomi b1) `mycompare` (Naomi b2) = b1 `compare` b2
(Ken b1) `mycompare` (Ken b2) = b1 `compare` b2

instance Eq Barbie where
    (Ken b1) == (Naomi b2) = b1 == b2
    (Naomi b1) == (Ken b2) = b1 == b2
    (Naomi b1) == (Naomi b2) = b1 == b2
    (Ken b1) == (Ken b2) = b1 == b2
instance Ord Barbie where
    compare = mycompare
-- Parser 
parseNumber :: Parser Double
parseNumber = rational 
parseLine = manyTill' (parseNumber <* many' (string " ")) endOfLine
parseCase =do
        n <- decimal <* endOfLine
        boxNaomi <- liftM (S.sort . S.fromList) $ parseLine
        boxKen   <- liftM (S.sort . S.fromList) $ parseLine
        return (boxNaomi,boxKen)
        
parseProblem = do
        size <- decimal <* endOfLine
        problems <- count size parseCase
        skipSpace
        return problems
 
translate input = case parseOnly parseProblem  input of
        Left con -> error con
        Right r -> r

parserTest = do
        print $ parseOnly parseCase "1\n0.5\n0.6\n"
        print $ parseOnly parseProblem "1\n3\n0.5 0.1 0.9\n0.6 0.4 0.3\n"
cmdArg []  = error "\nNecessario um argumento com extensão"
cmdArg (x:xs) = x

run = proxy 0
proxy :: Int->S.Seq Double -> S.Seq Double -> Int
proxy n naomi ken | last==1 = if worstNaomi > worstKen then n+1 else n
                  | worstNaomi> bestKen   = n+last
                  | worstKen  > bestNaomi = n
                  | worstNaomi > worstKen = proxy (n+1) (S.drop 1 naomi) (S.drop 1 ken)
                  | otherwise = proxy n (S.drop 1 naomi) (S.take (last-1) ken)
    where
    last = S.length naomi
    worstNaomi = S.index naomi 0 
    worstKen   = S.index ken 0
    bestNaomi  = S.index naomi (last-1)
    bestKen    = S.index ken (last-1)


run2 = proxy2 0
proxy2 :: Int->S.Seq Double -> S.Seq Double -> Int
proxy2 n naomi ken | last==1 = if worstNaomi > worstKen then n+1 else n
                   | worstNaomi> bestKen   = n+last
                   | worstKen  > bestNaomi = n
                   | bestNaomi > bestKen = proxy2 (n+1) (S.take (last-1) naomi) (S.drop 1 ken)
                   | otherwise = proxy2 n (S.take (last-1) naomi) (S.take (last-1) ken)
    where
    last = S.length naomi
    worstNaomi = S.index naomi 0 
    worstKen   = S.index ken 0
    bestNaomi  = S.index naomi (last-1)
    bestKen    = S.index ken (last-1)
-- parserTest optional        
main = do
        parserTest
        
        fileName <- return . cmdArg =<< getArgs
        contents <- liftM translate $ B.readFile fileName-- input
        B.writeFile "results.txt" $ helper contents
        putStrLn "\nDone\n"
        where 
        helper  input = B.unlines $  zipWith doline [1..] $ zipWith (,) (map (uncurry run) input) (map (uncurry run2) input)
        doline a (t1,t2) = B.pack $ "Case #"++show a++": "++ show t1++" "++show t2