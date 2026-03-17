{-# LANGUAGE ForeignFunctionInterface #-}

module Dataframe (
    Dataframe,
    readCsv,
    printDf,
) where

import Foreign
import Foreign.C

data CDataframe

newtype Dataframe = Dataframe (ForeignPtr CDataframe) deriving (Eq, Show)

foreign import ccall "read_csv" c_read_csv :: CString -> IO (Ptr CDataframe)
foreign import ccall "print_df" c_print_df :: Ptr CDataframe -> IO ()
foreign import ccall "&free_df" c_free_df :: FunPtr (Ptr CDataframe -> IO ())

wrapTable :: Ptr CDataframe -> IO (Maybe Dataframe)
wrapTable ptr
    | nullPtr == ptr = pure Nothing
    | otherwise = Just . Dataframe <$> newForeignPtr c_free_df ptr

readCsv :: FilePath -> IO (Maybe Dataframe)
readCsv path = withCString path c_read_csv >>= wrapTable

printDf :: Dataframe -> IO ()
printDf (Dataframe fptr) = withForeignPtr fptr c_print_df