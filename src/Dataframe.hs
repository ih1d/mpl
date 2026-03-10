{-# LANGUAGE ForeignFunctionInterface #-}

module Dataframe (
    Table,
    readCsv,
    printTable,
) where

import Foreign
import Foreign.C

data CTable

newtype Table = Table (ForeignPtr CTable) deriving (Eq, Show)

foreign import ccall "read_csv" c_read_csv :: CString -> IO (Ptr CTable)
foreign import ccall "print_table" c_print_table :: Ptr CTable -> Int64 -> IO ()
foreign import ccall "&table_free" c_table_free :: FunPtr (Ptr CTable -> IO ())

wrapTable :: Ptr CTable -> IO Table
wrapTable ptr
    | nullPtr == ptr = error "mpl: null table pointer"
    | otherwise = Table <$> newForeignPtr c_table_free ptr

readCsv :: FilePath -> IO Table
readCsv path = withCString path c_read_csv >>= wrapTable

printTable :: Table -> IO ()
printTable (Table ptr) = withForeignPtr ptr $ \p -> c_print_table p 20