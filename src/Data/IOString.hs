{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.IOString
    ( IOString
    , writeFile
    , readFile
    , appendFile
    , putStr
    , putStrLn
    , FilePath
    ) where

import           Data.Char                  (Char)
import           Data.Function              (($), (.))
import           GHC.IO                     (FilePath, IO)

import qualified System.IO                  as Base

import           Control.Monad.IO.Class     (MonadIO, liftIO)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BL

import qualified Data.Text                  as T
import qualified Data.Text.IO               as T

import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.IO          as TL

import           Path

class IOString s where
    iosWriteFile  :: FilePath -> s -> IO ()
    iosReadFile   :: FilePath -> IO s
    iosAppendFile :: FilePath -> s -> IO ()
    iosPutStr     :: s -> IO ()
    iosPutStrLn   :: s -> IO ()

instance IOString T.Text where
    iosWriteFile   = T.writeFile
    iosReadFile    = T.readFile
    iosAppendFile  = T.appendFile
    iosPutStr      = T.putStr
    iosPutStrLn    = T.putStrLn

instance IOString TL.Text where
    iosWriteFile   = TL.writeFile
    iosReadFile    = TL.readFile
    iosAppendFile  = TL.appendFile
    iosPutStr      = TL.putStr
    iosPutStrLn    = TL.putStrLn

instance IOString BS.ByteString where
    iosWriteFile   = BS.writeFile
    iosReadFile    = BS.readFile
    iosAppendFile  = BS.appendFile
    iosPutStr      = BS.putStr
    iosPutStrLn    = BS.putStrLn

instance IOString BL.ByteString where
    iosWriteFile   = BL.writeFile
    iosReadFile    = BL.readFile
    iosAppendFile  = BL.appendFile
    iosPutStr      = BL.putStr
    iosPutStrLn    = BL.putStrLn

instance IOString [Char] where
    iosWriteFile   = Base.writeFile
    iosReadFile    = Base.readFile
    iosAppendFile  = Base.appendFile
    iosPutStr      = Base.putStr
    iosPutStrLn    = Base.putStrLn

writeFile :: (IOString s, MonadIO m) => Path Abs File -> s -> m ()
writeFile p s = liftIO $ iosWriteFile (toFilePath p) s

readFile :: (IOString s, MonadIO m) => Path Abs File -> m s
readFile = liftIO . iosReadFile . toFilePath

appendFile :: (IOString s, MonadIO m) => Path Abs File -> s -> m ()
appendFile p s = liftIO $ iosAppendFile (toFilePath p) s

putStr :: (IOString s, MonadIO m) => s -> m ()
putStr = liftIO . iosPutStr

putStrLn :: (IOString s, MonadIO m) => s -> m ()
putStrLn = liftIO . iosPutStrLn
