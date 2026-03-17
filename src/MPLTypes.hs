module MPLTypes (
    DNA (..),
    RNA (..),
    transcribe,
    countNucleotides,
    reverseComplement,
    frequentKmers,
) where

import Data.List (sortBy)
import Data.Ord (Down (..), comparing)
import Dataframe (Dataframe, makeKmerDf)

newtype DNA = DNA String deriving (Eq)
instance Show DNA where
    show (DNA dna) = dna

newtype RNA = RNA String deriving (Eq)
instance Show RNA where
    show (RNA rna) = rna

transcribe :: DNA -> RNA
transcribe (DNA dna) = go dna []
  where
    go [] res = RNA res
    go (x : xs) res
        | x == 'T' = go xs (res ++ "U")
        | otherwise = go xs (res ++ [x])

countNucleotides :: DNA -> (Integer, Integer, Integer, Integer)
countNucleotides (DNA dna) =
    ( fromIntegral $ length $ filter (== 'A') dna
    , fromIntegral $ length $ filter (== 'C') dna
    , fromIntegral $ length $ filter (== 'G') dna
    , fromIntegral $ length $ filter (== 'T') dna
    )

complement :: Char -> Char
complement 'A' = 'T'
complement 'C' = 'G'
complement 'G' = 'C'
complement 'T' = 'A'
complement c = error $ c : " is not a nucleotide."

reverseComplement :: DNA -> DNA
reverseComplement (DNA dna) = DNA (map complement (reverse dna))

frequentKmers :: DNA -> Integer -> IO (Maybe Dataframe)
frequentKmers (DNA dna) k =
    let kmers = extractKmers (fromIntegral k) dna
        counted = countKmers kmers []
        sorted = sortBy (comparing (Down . snd)) counted
     in makeKmerDf sorted

extractKmers :: Int -> String -> [String]
extractKmers k s
    | k <= 0 = []
    | length s < k = []
    | otherwise = take k s : extractKmers k (tail s)

countKmers :: [String] -> [(String, Int)] -> [(String, Int)]
countKmers xs acc = foldl (flip increment) acc xs

increment :: String -> [(String, Int)] -> [(String, Int)]
increment kmer [] = [(kmer, 1)]
increment kmer ((k, c) : rest)
    | kmer == k = (k, c + 1) : rest
    | otherwise = (k, c) : increment kmer rest
