{-# LANGUAGE ForeignFunctionInterface #-}

module Dataframe
  ( Table
  , readCsv
  , nrows
  , ncols
  , colName
  , colNames
  , isNull
  , getCell
  , freeTable
  ) where

import Foreign
import Foreign.C
import Control.Monad (forM)

data CTable
newtype Table = Table (ForeignPtr CTable)

-- FFI
foreign import ccall "mpl_read_csv"     c_read    :: CString -> IO (Ptr CTable)
foreign import ccall "mpl_nrows"        c_nrows   :: Ptr CTable -> IO Int64
foreign import ccall "mpl_ncols"        c_ncols   :: Ptr CTable -> IO CUInt
foreign import ccall "mpl_colname"      c_colname :: Ptr CTable -> CUInt -> IO CString
foreign import ccall "mpl_is_null"      c_isnull  :: Ptr CTable -> CUInt -> Int64 -> IO CInt
foreign import ccall "mpl_get_cell"     c_cell    :: Ptr CTable -> CUInt -> Int64 -> IO CString
foreign import ccall "mpl_free_string"  c_freestr :: CString -> IO ()
foreign import ccall "&mpl_free"        c_free    :: FunPtr (Ptr CTable -> IO ())

-- API
readCsv :: FilePath -> IO Table
readCsv path = do
  ptr <- withCString path c_read
  if ptr == nullPtr
    then error ("Cannot read: " ++ path)
    else Table <$> newForeignPtr c_free ptr

nrows :: Table -> IO Int
nrows (Table fp) = withForeignPtr fp (fmap fromIntegral . c_nrows) 

ncols :: Table -> IO Int
ncols (Table fp) = withForeignPtr fp (fmap fromIntegral . c_ncols) 

colName :: Table -> Int -> IO String
colName (Table fp) i =
  withForeignPtr fp $ \p -> c_colname p (fromIntegral i) >>= peekCString

colNames :: Table -> IO [String]
colNames t = ncols t >>= \n -> forM [0..n-1] (colName t)

isNull :: Table -> Int -> Int -> IO Bool
isNull (Table fp) col row =
  withForeignPtr fp $ \p ->
    (/= 0) <$> c_isnull p (fromIntegral col) (fromIntegral row)

getCell :: Table -> Int -> Int -> IO String
getCell (Table fp) col row =
  withForeignPtr fp $ \p -> do
    cs <- c_cell p (fromIntegral col) (fromIntegral row)
    s <- peekCString cs
    c_freestr cs
    pure s

freeTable :: Table -> IO ()
freeTable (Table fp) = finalizeForeignPtr fp