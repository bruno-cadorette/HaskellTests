import Criterion.Main 

import Data.Char
import Data.List
import Data.Ord

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map
import qualified Data.IntMap as IMap

{-
    The problem: read a list of words, keep the ones between 4 and 15 characters, 
    then group a upper version of them by length.
-}

between :: Ord a => a -> a -> a -> Bool     
between l v u = l <= v && v <= u   

toLookup :: (Ord key) => (a -> key) -> (a -> v) -> [a] -> Map.Map key [v]
toLookup getKey getValue = Map.fromListWith (++) . map (\x-> (getKey x, [getValue x]))

generateDict1 :: String -> [[String]]
generateDict1 = 
    groupBy (\a b -> EQ == comparing length a b) . 
    sortOn length . map (map toUpper) . 
    filter (\x ->between 4 (length x) 15) . words
    
generateDict2 :: String -> [[String]]
generateDict2 = 
    map (map fst) . groupBy (\a b -> EQ == comparing snd a b) . 
    sortOn snd . map (\(x,l)-> (map toUpper x, l)) . 
    filter (\x ->between 4 (snd x) 15) . 
    map(\x -> (x, length x)) . words    
    
generateDict3 :: B.ByteString -> [[B.ByteString]]
generateDict3 = 
    groupBy (\a b -> EQ == comparing B.length a b) . 
    sortOn B.length . map (B.map toUpper) .
    filter (\x ->between 4 (B.length x) 15) . B.words  
        
generateDict4 :: B.ByteString -> Map.Map Int [B.ByteString]      
generateDict4 =
    toLookup B.length (B.map toUpper) .
    filter (\x ->between 4 (B.length x) 15) . B.words
        
generateDict5 :: String -> Map.Map Int [String]   
generateDict5 =
    toLookup length (map toUpper) .
    filter (\x ->between 4 (length x) 15) . words    

generateDict6 :: String -> Map.Map Int [String]   
generateDict6 =
    toLookup fst (map toUpper . snd) .
    filter (\x ->between 4 (fst x) 15) . 
    map(\x -> (length x, x)) . words
    
generateDict7 :: B.ByteString -> [[B.ByteString]]
generateDict7 = 
    map (map fst) .
    groupBy (\a b -> EQ == comparing snd a b) . 
    sortOn snd . map (\(x,l)->(B.map toUpper x, l)) .
    filter (\x ->between 4 (snd x) 15) . 
    map(\x -> (x, B.length x)) . B.words   

generateDict8 :: B.ByteString -> Map.Map Int [B.ByteString]   
generateDict8 =
    toLookup fst (B.map toUpper . snd) .
    filter (\x ->between 4 (fst x) 15) . 
    map(\x -> (B.length x, x)) . B.words    
    
generateDict9 :: B.ByteString -> IMap.IntMap [B.ByteString] 
generateDict9 =
    IMap.fromListWith (++) . map (\x-> (B.length x, [B.map toUpper x])) .
    filter (\x ->between 4 (B.length x) 15) . B.words
    
generateDict10 :: String -> IMap.IntMap [String] 
generateDict10 =
    IMap.fromListWith (++) . map (\x-> (length x, [map toUpper x])) .
    filter (\x ->between 4 (length x) 15) . words
    
main::IO()    
main = do
    fileContent <- readFile "enable1.txt"
    fileContentBS <- B.readFile "enable1.txt"
    defaultMain [
        bgroup "string" [ bench "groupby" $ whnf generateDict1 fileContent,
                      bench "groupby, caching length" $ whnf generateDict2 fileContent,
                      bench "map" $ whnf generateDict5 fileContent,
                      bench "map, caching length" $ whnf generateDict6 fileContent,
                      bench "intmap" $ whnf generateDict10 fileContent],
        bgroup "bytestring" [
                      bench "groupby" $ whnf generateDict3 fileContentBS,
                      bench "map" $ whnf generateDict4 fileContentBS,
                      bench "groupby, caching length" $ whnf generateDict7 fileContentBS,
                      bench "map, caching length" $ whnf generateDict8 fileContentBS,
                      bench "intmap" $ whnf generateDict9 fileContentBS]]