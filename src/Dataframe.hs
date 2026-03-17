{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ViewPatterns #-}

module Dataframe (
    Dataframe,
    readCsv,
    writeCsv,
    printDf,
    headDf,
    tailDf,
    makeKmerDf,
) where

import Foreign
import Foreign.C

data CDataframe

newtype Dataframe = Dataframe (ForeignPtr CDataframe) deriving (Eq, Show)

foreign import ccall "read_csv" c_read_csv :: CString -> IO (Ptr CDataframe)
foreign import ccall "write_csv" c_write_csv :: CString -> Ptr CDataframe -> IO ()
foreign import ccall "print_df" c_print_df :: Ptr CDataframe -> IO ()
foreign import ccall "head_df" c_head_df :: Ptr CDataframe -> CInt -> IO (Ptr CDataframe)
foreign import ccall "tail_df" c_tail_df :: Ptr CDataframe -> CInt -> IO (Ptr CDataframe)
foreign import ccall "make_kmer_df" c_make_kmer_df :: Ptr CString -> Ptr CInt -> CInt -> IO (Ptr CDataframe)
foreign import ccall "&free_df" c_free_df :: FunPtr (Ptr CDataframe -> IO ())

wrapTable :: Ptr CDataframe -> IO (Maybe Dataframe)
wrapTable ptr
    | nullPtr == ptr = pure Nothing
    | otherwise = Just . Dataframe <$> newForeignPtr c_free_df ptr

readCsv :: FilePath -> IO (Maybe Dataframe)
readCsv path = withCString path c_read_csv >>= wrapTable

writeCsv :: FilePath -> Dataframe -> IO ()
writeCsv path (Dataframe fptr) = withCString path $ \cpath ->
    withForeignPtr fptr (c_write_csv cpath)

printDf :: Dataframe -> IO ()
printDf (Dataframe fptr) = withForeignPtr fptr c_print_df

headDf :: Dataframe -> Integer -> IO (Maybe Dataframe)
headDf (Dataframe fptr) (fromIntegral -> i) = withForeignPtr fptr $ \ptr -> c_head_df ptr i >>= wrapTable

tailDf :: Dataframe -> Integer -> IO (Maybe Dataframe)
tailDf (Dataframe fptr) (fromIntegral -> i) = withForeignPtr fptr $ \ptr -> c_tail_df ptr i >>= wrapTable

makeKmerDf :: [(String, Int)] -> IO (Maybe Dataframe)
makeKmerDf [] = pure Nothing
makeKmerDf pairs = do
    let (kmers, counts) = unzip pairs
        n = length pairs
    cKmers <- mapM newCString kmers
    let cCounts = map fromIntegral counts
    result <- withArray cKmers $ \kmerArr ->
        withArray cCounts $ \countArr ->
            c_make_kmer_df kmerArr countArr (fromIntegral n)
    mapM_ free cKmers
    wrapTable result
