{-# LANGUAGE CPP #-}

-- |

module GhciIO (touchIfExists) where

import System.Directory

#ifdef mingw32_HOST_OS
import qualified System.Win32 as Win32
#else
import System.Posix (touchFile)
#endif

#ifdef mingw32_HOST_OS
import Data.Bits ((.&.))
#endif

{-| Touch a file, updating the access and modification times to the current time

    Creates an empty file if it does not exist

Copied from turtle.
-}
touchIfExists :: FilePath -> IO ()
touchIfExists file = do
    exists <- doesFileExist file
    if exists
#ifdef mingw32_HOST_OS
        then do
            handle <- Win32.createFile
                file
                Win32.gENERIC_WRITE
                Win32.fILE_SHARE_NONE
                Nothing
                Win32.oPEN_EXISTING
                Win32.fILE_ATTRIBUTE_NORMAL
                Nothing
            (creationTime, _, _) <- Win32.getFileTime handle
            systemTime <- Win32.getSystemTimeAsFileTime
            Win32.setFileTime handle creationTime systemTime systemTime
#else
        then touchFile file
#endif
        else pure ()
