
-- High level api for playing sounds (and background music)

module Sound.SFML (
    -- * PolySounds
    PolySound,
    newPolySound,
    freePolySound,
    triggerPolySound,

    -- * LoopedSounds
    LoopedSound,
    newLoopedSound,
    freeLoopedSound,
    startLoopedSound,
    stopLoopedSound,

    -- * background music
    playMusic,
    playMusicLooped,
    stopMusic,
    pauseMusic,

  ) where


import Prelude hiding (mapM_)

import Data.Maybe
import Data.IORef
import Data.Foldable (forM_, mapM_)
import Data.Traversable (forM)

import Control.Monad (when)
import Control.Concurrent.MVar

import System.IO.Unsafe

import Foreign.Ptr

import Sound.SFML.LowLevel

-- * PolySounds

-- | A PolySound allows you to trigger one sound multiple times.
-- The played sounds will then overlap.
-- (Internally, there will be multiple sound instances, that will
-- be triggered one after the other. If there are not enough internal
-- instances, sounds will be cut.)
data PolySound = PolySound FilePath (Ptr SoundBuffer) [Ptr Sound] (IORef Int)

instance Show PolySound where
    show (PolySound file _ _ _) = "PolySound " ++ show file

-- | Loads a sound into memory.
newPolySound ::
    FilePath -- ^ soundfile
    -> Int -- ^ number of internal sound instances.
    -> IO PolySound
newPolySound path numberOfVoices = do
    buffer <- sfSoundBuffer_CreateFromFile path
    sounds <- forM [1 .. numberOfVoices] $ \ _ -> do
        sound <- sfSound_Create
        sfSound_SetBuffer sound buffer
        return sound
    ref <- newIORef 0
    return $ PolySound path buffer sounds ref

-- | Frees the memory allocated by a sound. Don't use the PolySound afterwards.
freePolySound :: PolySound -> IO ()
freePolySound (PolySound _ buffer sounds _) = do
    sfSoundBuffer_Destroy buffer
    mapM_ sfSound_Destroy sounds

-- | Trigger a sound
triggerPolySound :: PolySound -> Maybe Float -> IO ()
triggerPolySound (PolySound _ _ sounds ref) volume = do
    i <- readIORef ref
    let sound = sounds !! i
    status <- getSoundStatus sound
    when (status == Stopped) $ do
        writeIORef ref ((i + 1) `mod` length sounds)
        sfSound_SetVolume sound ((fromMaybe 1 volume) * 100)
        sfSound_Play sound


-- * LoopedSounds

-- | LoopedSounds are sounds that will always loop.
-- They can just be switched on and off.
newtype LoopedSound = LoopedSound (Ptr Sound)
  deriving Show

-- | Loads a sound into memory.
newLoopedSound :: FilePath -> IO LoopedSound
newLoopedSound path = do
    buffer <- sfSoundBuffer_CreateFromFile path
    sound <- sfSound_Create
    sfSound_SetBuffer sound buffer
    sfSound_SetLoop sound True
    return $ LoopedSound sound

-- | Releases the allocated memory of a LoopedSound.
-- Don't use the LoopedSound afterwards.
freeLoopedSound :: LoopedSound -> IO ()
freeLoopedSound (LoopedSound ptr) =
    sfSound_Destroy ptr

-- | Starts a looped sound.
startLoopedSound :: Maybe Float -> LoopedSound -> IO ()
startLoopedSound volume (LoopedSound ptr) = do
    sfSound_SetVolume ptr ((fromMaybe 1 volume) * 100)
    sfSound_Play ptr

-- | Stops a looped sound.
stopLoopedSound :: LoopedSound -> IO ()
stopLoopedSound (LoopedSound ptr) =
    sfSound_Stop ptr


-- * Music

-- There is always only one music being played at a single point in time.

-- | Loads and plays a music file once in a background thread.
-- Stops other music that is playing.
-- If the current music is Paused and the given file is the same as the one playing,
-- the music is continued.
-- The volume is set again in any case.
playMusic :: FilePath -> Maybe Float -> IO ()
playMusic = _playMusic False

-- |Like 'playMusic', but looping.
playMusicLooped :: FilePath -> Maybe Float -> IO ()
playMusicLooped = _playMusic True

_playMusic :: Bool -> FilePath -> Maybe Float -> IO ()
_playMusic looped file volume = modifyMVar_ _globalMusic $ \ mOldMusic -> do
    case mOldMusic of
        Just (oldFile, oldMusic) -> do
            status <- getMusicStatus oldMusic
            case status of
                Paused | file == oldFile -> do
                    sfMusic_SetLoop oldMusic looped
                    sfMusic_SetVolume oldMusic ((fromMaybe 1 volume) * 100)
                    sfMusic_Play oldMusic
                    return $ Just (file, oldMusic)
                _ -> do
                    sfMusic_Stop oldMusic
                    sfMusic_Destroy oldMusic
                    startNewMusic
        Nothing -> startNewMusic
  where
    startNewMusic = do
        music <- sfMusic_CreateFromFile file
        sfMusic_SetLoop music looped
        sfMusic_SetVolume music ((fromMaybe 1 volume) * 100)
        sfMusic_Play music
        return $ Just (file, music)

-- | Stops any background music that is playing.
stopMusic :: IO ()
stopMusic = modifyMVar_ _globalMusic $ \ mOldMusic -> do
    forM_ mOldMusic $ \ (_, oldMusic) -> do
        sfMusic_Stop oldMusic
        sfMusic_Destroy oldMusic
    return Nothing

-- | Pauses the current music.
pauseMusic :: IO ()
pauseMusic = modifyMVar_ _globalMusic $ \ mMusic -> do
    mapM_ (sfMusic_Pause . snd) mMusic
    return mMusic


{-# noinline _globalMusic #-}
-- all music-related functions are synchronized by this global MVar.
_globalMusic :: MVar (Maybe (FilePath, Ptr Music))
_globalMusic = unsafePerformIO $ newMVar Nothing
