module MPLTypes (
    DNA (..),
    RNA (..),
    transcribe,
    countNucleotides,
    reverseComplement,
    frequentKmers,
) where

import Data.Map.Strict qualified as Map
import Dataframe (Table, buildKmerTable)

newtype DNA = DNA String deriving (Eq)
instance Show DNA where
    show (DNA dna) = dna

newtype RNA = RNA String deriving (Eq)
instance Show RNA where
    show (RNA rna) = rna

newtype Protein = Protein String deriving (Eq)
instance Show Protein where
    show (Protein protein) = protein

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

frequentKmers :: DNA -> Integer -> IO Table
frequentKmers (DNA dna) k = buildKmerTable (Map.toDescList counts)
  where
    kmers = [take (fromInteger k) (drop i dna) | i <- [0 .. length dna - (fromInteger k)]]
    counts = foldl (\m kmer -> Map.insertWith (+) kmer 1 m) Map.empty kmers
