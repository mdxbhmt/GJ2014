{-# LANGUAGE OverloadedStrings  #-}
import Control.Applicative((<*))
import Control.Monad  (liftM)
import Data.Attoparsec
import Data.Attoparsec.Char8 hiding (isSpace)
import qualified Data.ByteString.Char8 as B
import System.Environment
import Data.List
import Text.Printf
import qualified Data.Vector as V
import qualified Data.Matrix as M

data Sweeper = Mine | Number Int | Space  | Click


mychar (Mine)    = '*'
mychar (Number x)= '.'
mychar (Space)   = '.'
mychar (Click)   = 'c'

myshow (Mine)    = "*"
myshow (Number x)= "."
myshow (Space)   = "."
myshow (Click)   = "C"
instance Show Sweeper where
    show=myshow
-- Parser 
isSpace Space = True
isSpace _ = False
isMine Mine = True
isMine _ = False
isNumber (Number _) = True
isNumber _ = False
parseNumber = decimal
parseCase =do
        r <- parseNumber
        string " "
        c <- parseNumber
        string " "
        m <- parseNumber
        endOfLine
        return (r,c,m)
        
parseProblem = do
        size <- decimal <* endOfLine
        problems <- count size parseCase
        skipSpace
        return problems
        
 
translate input = case parseOnly parseProblem  input of
        Left con -> error con
        Right r -> r

parserTest = do
        print $ parseOnly parseCase "5 5 23\n"
        print $ parseOnly parseProblem "1\n5 5 23\n"
-- cmdArg test 
cmdArg []  = error "\nNecessario um argumento com extensão"
cmdArg (x:xs) = x

makematrix r c =  M.matrix r c (\_ -> Space)


makematrix2 r c m
        |m >= c = (M.rowVector $ V.replicate c Mine) M.<-> makematrix2 (r-1)c (m-c)
        |m >= r = (M.colVector $ V.replicate r Mine) M.<|> makematrix2 r (c-1) (m-r)
        |otherwise = (M.rowVector $ V.fromList (replicate m Mine ++ replicate (c-m) Space) )M.<-> makematrix (r-1) c 


makematrix3 r c m
        |m==0   = makematrix r c
        |r > c  = makeRow r c m 
        |c >= r = makeCol r c m

makeRow r c m 
    |m >= c = (M.rowVector $ V.replicate c Mine)  M.<-> makematrix3 (r-1) c (m-c)
    |otherwise = (M.colVector $ V.fromList (replicate m Mine ++ replicate (r-m) Space)) M.<|> makematrix r (c-1)   
    
makeCol r c m 
    |m >= r = (M.colVector $ V.replicate r Mine) M.<|> makematrix3 r (c-1) (m-r)
    |otherwise = (M.rowVector $ V.fromList (replicate m Mine ++ replicate (c-m) Space)) M.<-> makematrix (r-1) c  
    
spaceIndex matrix = [(i,j)|i<-[1..r],j<-[1..c], isSpace (matrix M.!(i,j))]
    where 
    r = M.nrows matrix
    c = M.ncols matrix

numberIndexes matrix = [(i,j)|i<-[1..r],j<-[1..c], isNumber(matrix M.!(i,j))]
    where 
    r = M.nrows matrix
    c = M.ncols matrix
run (r,c,m,matrix) = do
      let m2= first_transform matrix
      m3 <- if (r*c-m==1) then return matrix else test m2
      second_transform m3

test matrix = if all (numberWithSpace matrix) $ indexes then Just matrix else Nothing
    where
    indexes = numberIndexes matrix
    
second_transform matrix = if null indexes then Nothing else Just $  M.setElem Click (head indexes) matrix
    where
    indexes = spaceIndex matrix 
first_transform matrix =  foldl' (\m (v,(i,j)) -> M.setElem v (i,j) m) matrix (zipWith (,) numbers indexes)
    where
    numbers =  map (mineNumber matrix) indexes
    indexes = spaceIndex matrix 
--indexes = spaceIndex matrix
mineNumber matrix (u,v) = if value==0 then Space else Number value
    where 
    value =  sum [1|i<-[u-1,u,u+1],j<-[v-1,v,v+1], i>=1,j>=1,i<=r,j<=c,isMine(matrix M.!(i,j))]
    r = M.nrows matrix
    c = M.ncols matrix

numberWithSpace matrix (u,v) = if value>0 then True else False
    where 
    value =  sum [1|i<-[u-1,u,u+1],j<-[v-1,v,v+1], i>=1,j>=1,i<=r,j<=c, isSpace(matrix M.!(i,j))]
    r = M.nrows matrix
    c = M.ncols matrix    
prettyM matrix = concat . intersperse "\n" . ( map ((map mychar) . V.toList . ((flip M.getRow) matrix) ) ) $  [1..r]
    where
    r = M.nrows matrix

    
-- parserTest optional        
main = do
        parserTest        
        fileName <- return . cmdArg =<< getArgs
        contents <- liftM translate $ B.readFile fileName-- input
        B.writeFile "results.txt" $ helper contents
        putStrLn "\nDone\n"
        where 
        helper  input = B.unlines $  zipWith doline [1..] $ map (run. \(r,c,m)-> (r,c,m,makematrix3 r c m)) input
        doline a b = B.pack $ "Case #"++show a++": \n" ++ pretty b
        pretty :: Maybe (M.Matrix Sweeper) -> String
        pretty (Nothing) = "Impossible"
        pretty (Just matrix) = prettyM matrix