{-# LANGUAGE CApiFFI #-}

module Dataframe where
import Foreign.C
import Foreign

data CTable

newtype Table = Table (ForeignPtr CTable) deriving (Eq, Show)

foreign import capi "mpl_runtime.h read_csv" c_read_csv :: CString -> IO (Ptr CTable)
foreign import capi "mpl_runtime.h &free_object" c_free_object :: FunPtr (Ptr a -> IO ())

readCsv :: FilePath -> IO Table
readCsv f = do
    ptr <- withCString f c_read_csv
    if ptr == nullPtr
        then error ("readCsv: failed to read " ++ f)
        else Table <$> newForeignPtr c_free_object ptr

freeTable :: Table -> IO ()
freeTable (Table ptr) = finalizeForeignPtr ptr