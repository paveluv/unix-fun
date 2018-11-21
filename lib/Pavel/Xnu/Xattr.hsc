{-# LANGUAGE CApiFFI #-}

module Pavel.Xnu.Xattr where

import Data.Bits
import Foreign.C
import Foreign.C.Error
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import System.Posix.Internals
import System.Posix.Types

#include <sys/xattr.h>

foreign import capi unsafe "sys/xattr.h listxattr" c_listxattr
  :: CString -> Ptr CChar -> CSize -> CInt -> IO CSsize

data XattrFlag
  = XATTR_NOFOLLOW
  | XATTR_SHOWCOMPRESSION

packXattrFlag :: XattrFlag -> CInt
packXattrFlag XATTR_NOFOLLOW = #const XATTR_NOFOLLOW
packXattrFlag XATTR_SHOWCOMPRESSION = #const XATTR_SHOWCOMPRESSION

packXattrFlags :: [XattrFlag] -> CInt
packXattrFlags flags = foldl (\s f -> (packXattrFlag f) .|. s) 0 flags

hasXattr :: [XattrFlag] -> FilePath -> IO Bool
hasXattr flags path =
  withFilePath path $ \p -> do
    size <-
      throwErrnoPathIfMinus1
        "hasXattr"
        path
        (c_listxattr p nullPtr 0 (packXattrFlags flags))
    return $ size > 0

getXattr :: [XattrFlag] -> FilePath -> IO String
getXattr flags path =
  withFilePath path $ \p -> do
    buf <- mallocForeignPtrBytes 64
    withForeignPtr buf $ \bufp -> do
      size <-
        throwErrnoPathIfMinus1
          "hasXattr"
          path
          (c_listxattr p bufp (fromIntegral 64) (packXattrFlags flags))
      if size > 64
        then do
          buf <- mallocForeignPtrBytes (fromIntegral size)
          withForeignPtr buf $ \bufp -> do
            throwErrnoPathIfMinus1_
              "hasXattr"
              path
              (c_listxattr p bufp (fromIntegral size) (packXattrFlags flags))
            peekCStringLen (bufp, fromIntegral size)
        else peekCStringLen (bufp, fromIntegral size)
