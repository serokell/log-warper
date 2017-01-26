{-# LANGUAGE NoImplicitPrelude #-}

-- | Some helpers and utilites to work with files

module System.Wlog.FileUtils
       ( whenExist
       ) where

import           Universum

import           System.Directory (doesFileExist)

-- | Performs given action on file if file exists.
whenExist :: MonadIO m => FilePath -> (FilePath -> m ()) -> m ()
whenExist filePath action = whenM (liftIO $ doesFileExist filePath) $ action filePath
