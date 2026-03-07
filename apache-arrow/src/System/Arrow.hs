{-# LANGUAGE CApiFFI #-}

module System.Arrow where

import Foreign
import Foreign.C

data CArrowTable
newtype Table = Table (ForeignPtr CArrowTable) deriving (Eq, Show)

foreign import capi "hs_apache_arrow.h read_csv"
    c_read_csv :: CString -> IO (Ptr CArrowTable)

foreign import ccall "hs_apache_arrow.h &free_arrow_table"
    c_free_arrow_table :: FunPtr (Ptr CArrowTable -> IO ())

readCsv :: FilePath -> IO Table
readCsv path = do
    ptr <- withCString path c_read_csv
    if ptr == nullPtr
        then error ("readCsv: failed to read " ++ path)
        else Table <$> newForeignPtr c_free_arrow_table ptr
