{-# LANGUAGE TemplateHaskell #-}

module MPLTypes (
    DNA(..),
    RNA(..),
    transcribe,
    Column(..),
    Dataframe(..),
    -- Dataframe getters
    getRows,
    getColumns,
    getColumn,
    getColumnNames,
    getIntColumn,
    getDoubleColumn,
    getStringColumn,
    getDNAColumn,
    getRNAColumn,
    -- Dataframe setters
    setRows,
    setColumns,
    setColumn,
    removeColumn
) where

import Data.Vector (Vector, (!))
import Data.Word (Word64)
import Data.Bits ((.&.), shiftR)
import Data.Map (Map, keys, lookup, insert, delete, toAscList)
import Control.Lens (makePrisms, makeLenses)
import Prelude hiding (lookup)

newtype DNA = DNA (Vector Word64, Int) deriving (Eq)
instance Show DNA where
    show (DNA (ws, len)) = showSeq dnaChar ws len

newtype RNA = RNA (Vector Word64, Int) deriving (Eq)
instance Show RNA where
    show (RNA (ws, len)) = showSeq rnaChar ws len

showSeq :: (Word64 -> Char) -> Vector Word64 -> Int -> String
showSeq decode v = go 0
  where
    go _ 0 = []
    go idx remaining =
        let w = v ! idx
            n = min 32 remaining
        in [decode ((w `shiftR` (2 * (n - 1 - i))) .&. 3) | i <- [0..n-1]] ++ go (idx + 1) (remaining - n)

dnaChar :: Word64 -> Char
dnaChar 0 = 'A'
dnaChar 1 = 'C'
dnaChar 2 = 'G'
dnaChar _ = 'T'

rnaChar :: Word64 -> Char
rnaChar 0 = 'A'
rnaChar 1 = 'C'
rnaChar 2 = 'G'
rnaChar _ = 'U'

transcribe :: DNA -> RNA
transcribe (DNA (vec, len)) = RNA (vec, len)

data Column
    = ColInt (Vector Int)
    | ColDouble (Vector Double)
    | ColString (Vector String)
    | ColDNA (Vector DNA)
    | ColRNA (Vector RNA)
    deriving (Eq)
makePrisms ''Column

data Dataframe = Dataframe
    { _columns :: Map String Column
    , _rows :: Int
    } deriving (Eq)
makeLenses ''Dataframe

instance Show Dataframe where
    show df
        | null pairs = "(empty dataframe)"
        | otherwise  = unlines $ header : separator : dataRows ++ [footer]
      where
        pairs    = toAscList (_columns df)
        names    = map fst pairs
        cols     = map snd pairs
        nRows    = _rows df
        nCols    = length pairs
        colStrs  = map (\col -> [showCell col r | r <- [0..nRows-1]]) cols
        widths   = zipWith (\name cells -> maximum (length name : map length cells)) names colStrs
        idxW     = if nRows > 0 then length (show (nRows - 1)) else 1
        padR w s = replicate (w - length s) ' ' ++ s
        header   = replicate idxW ' ' ++ "  " ++ unwords (zipWith padR widths names)
        separator = replicate (idxW + 2 + sum widths + length widths - 1) '-'
        dataRows = [ padR idxW (show r) ++ "  " ++ unwords (zipWith padR widths [cs !! r | cs <- colStrs])
                   | r <- [0..nRows-1] ]
        footer   = "(" ++ show nRows ++ " rows x " ++ show nCols ++ " columns)"

showCell :: Column -> Int -> String
showCell (ColInt v)    i = show (v ! i)
showCell (ColDouble v) i = show (v ! i)
showCell (ColString v) i = v ! i
showCell (ColDNA v)    i = show (v ! i)
showCell (ColRNA v)    i = show (v ! i)

-- Getters

getRows :: Dataframe -> Int
getRows = _rows

getColumns :: Dataframe -> Map String Column
getColumns = _columns

getColumn :: String -> Dataframe -> Maybe Column
getColumn name df = lookup name (_columns df)

getColumnNames :: Dataframe -> [String]
getColumnNames = keys . _columns

getIntColumn :: String -> Dataframe -> Maybe (Vector Int)
getIntColumn name df = case getColumn name df of
    Just (ColInt v) -> Just v
    _               -> Nothing

getDoubleColumn :: String -> Dataframe -> Maybe (Vector Double)
getDoubleColumn name df = case getColumn name df of
    Just (ColDouble v) -> Just v
    _                  -> Nothing

getStringColumn :: String -> Dataframe -> Maybe (Vector String)
getStringColumn name df = case getColumn name df of
    Just (ColString v) -> Just v
    _                  -> Nothing

getDNAColumn :: String -> Dataframe -> Maybe (Vector DNA)
getDNAColumn name df = case getColumn name df of
    Just (ColDNA v) -> Just v
    _               -> Nothing

getRNAColumn :: String -> Dataframe -> Maybe (Vector RNA)
getRNAColumn name df = case getColumn name df of
    Just (ColRNA v) -> Just v
    _               -> Nothing

-- Setters

setRows :: Int -> Dataframe -> Dataframe
setRows n df = df { _rows = n }

setColumns :: Map String Column -> Dataframe -> Dataframe
setColumns cols df = df { _columns = cols }

setColumn :: String -> Column -> Dataframe -> Dataframe
setColumn name col df = df { _columns = insert name col (_columns df) }

removeColumn :: String -> Dataframe -> Dataframe
removeColumn name df = df { _columns = delete name (_columns df) }
