{-# language ForeignFunctionInterface, EmptyDataDecls #-}

module Sound.SFML.LowLevel where


import qualified Data.ByteString.Lazy as LBS
import Data.Word

import Control.Applicative

import System.IO

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array


-- * Common

toStatus :: Int -> Status
toStatus = toEnum

data Status
    = Stopped
    | Paused
    | Playing
  deriving (Eq, Enum, Show)


-- * SoundBuffer

data SoundBuffer

sfSoundBuffer_CreateFromFile :: FilePath -> IO (Ptr SoundBuffer)
sfSoundBuffer_CreateFromFile path = withCString path c__sfSoundBuffer_CreateFromFile

foreign import ccall "sfSoundBuffer_CreateFromFile" c__sfSoundBuffer_CreateFromFile ::
    CString -> IO (Ptr SoundBuffer)

foreign import ccall sfSoundBuffer_Destroy :: Ptr SoundBuffer -> IO ()


-- * Sound

data Sound

foreign import ccall sfSound_Create :: IO (Ptr Sound)

foreign import ccall sfSound_SetBuffer :: Ptr Sound -> Ptr SoundBuffer -> IO ()

foreign import ccall sfSound_Play :: Ptr Sound -> IO ()

foreign import ccall sfSound_Stop :: Ptr Sound -> IO ()

foreign import ccall sfSound_Destroy :: Ptr Sound -> IO ()

foreign import ccall sfSound_SetLoop :: Ptr Sound -> Bool -> IO ()

foreign import ccall sfSound_SetVolume :: Ptr Sound -> Float -> IO ()

foreign import ccall sfSound_GetStatus :: Ptr Sound -> IO Int

getSoundStatus s = toStatus <$> sfSound_GetStatus s


-- * Music

data Music

sfMusic_CreateFromFile :: FilePath -> IO (Ptr Music)
sfMusic_CreateFromFile path =
    withBinaryFile path ReadMode $ \ handle -> do
        size <- hFileSize handle
        bytes <- LBS.hGetContents handle
        arrayPtr <- mallocArray (fromIntegral size)
        pokeArray arrayPtr (LBS.unpack bytes)
        c__sfMusic_CreateFromMemory arrayPtr (fromIntegral size)

-- not needed anymore
foreign import ccall "sfMusic_CreateFromFile" c__sfMusic_CreateFromFile :: CString -> IO (Ptr Music)

foreign import ccall "sfMusic_CreateFromMemory" c__sfMusic_CreateFromMemory :: Ptr Word8 -> CUInt -> IO (Ptr Music)

foreign import ccall sfMusic_Destroy :: Ptr Music -> IO ()

foreign import ccall sfMusic_Play :: Ptr Music -> IO ()

foreign import ccall sfMusic_Stop :: Ptr Music -> IO ()

foreign import ccall sfMusic_Pause :: Ptr Music -> IO ()

foreign import ccall sfMusic_SetLoop :: Ptr Music -> Bool -> IO ()

foreign import ccall sfMusic_SetVolume :: Ptr Music -> Float -> IO ()

foreign import ccall sfMusic_GetDuration :: Ptr Music -> IO Float

foreign import ccall sfMusic_GetStatus :: Ptr Music -> IO Int

getMusicStatus s = toStatus <$> sfMusic_GetStatus s
