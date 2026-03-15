{-# LANGUAGE ForeignFunctionInterface #-}

module Dataframe (
    Table,
    readCsv,
    printTable,
    filterTableGt,
    filterTableLt,
    filterTableEq,
) where

import Foreign
import Foreign.C

data CTable

newtype Table = Table (ForeignPtr CTable) deriving (Eq, Show)

foreign import ccall "read_csv" c_read_csv :: CString -> IO (Ptr CTable)
foreign import ccall "print_table" c_print_table :: Ptr CTable -> Int64 -> IO ()
foreign import ccall "filter_gt" c_filter_gt :: Ptr CTable -> CString -> CDouble -> IO (Ptr CTable)
foreign import ccall "filter_lt" c_filter_lt :: Ptr CTable -> CString -> CDouble -> IO (Ptr CTable)
foreign import ccall "filter_eq" c_filter_eq :: Ptr CTable -> CString -> CDouble -> IO (Ptr CTable)
foreign import ccall "&table_free" c_table_free :: FunPtr (Ptr CTable -> IO ())

wrapTable :: Ptr CTable -> IO Table
wrapTable ptr
    | nullPtr == ptr = error "mpl: null table pointer"
    | otherwise = Table <$> newForeignPtr c_table_free ptr

readCsv :: FilePath -> IO Table
readCsv path = withCString path c_read_csv >>= wrapTable

printTable :: Table -> IO ()
printTable (Table ptr) = withForeignPtr ptr $ \p -> c_print_table p 20

filterTableGt :: Table -> String -> Double -> IO Table
filterTableGt (Table ptr) column val = withForeignPtr ptr $ \p -> withCString column $ \f -> c_filter_gt p f (CDouble val) >>= wrapTable

filterTableLt :: Table -> String -> Double -> IO Table
filterTableLt (Table ptr) column val = withForeignPtr ptr $ \p -> withCString column $ \f -> c_filter_lt p f (CDouble val) >>= wrapTable

filterTableEq :: Table -> String -> Double -> IO Table
filterTableEq (Table ptr) column val = withForeignPtr ptr $ \p -> withCString column $ \f -> c_filter_eq p f (CDouble val) >>= wrapTable
