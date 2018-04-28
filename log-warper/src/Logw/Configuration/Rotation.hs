{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Parameters for logger rotation.

module Logw.Configuration.Rotation
       ( -- * Rotation type
         RotationParameters (..)
       , rpMaxSizeBytes
       , rpKeepFiles
       , isValidRotation

         -- * Extension
       , NoRotation (..)
       , RotationExtension
       ) where

import Universum

import Data.Yaml (FromJSON (..), Object, Parser, ToJSON, withObject, (.:))
import Fmt (build, (||+))
import Lens.Micro.Platform (makeLenses)

import Logw.Configuration.Extension (Extension (Rotation), ExtensionType)

import qualified Data.HashMap.Strict as HashMap

-- | Parameters for logging rotation.
data RotationParameters = RotationParameters
    { _rpMaxSizeBytes :: !Word64  -- ^ max size of file in bytes
    , _rpKeepFiles    :: !Word    -- ^ number of files to keep
    } deriving (Show, Eq, Generic)

instance Buildable RotationParameters where
    build x = x||+""

instance ToJSON RotationParameters

{- | In YAML this must be specified like this:

@
rotation:
  maxSizeBytes: 1000000
  keepFiles: 7
@
-}
instance FromJSON RotationParameters where
    parseJSON = withObject "RotationParameters" $ \o -> do
        rotationValue <- o .: "rotation"
        withObject "RotationFields" rotationP rotationValue
      where
        rotationP :: Object -> Parser RotationParameters
        rotationP o = do
            _rpMaxSizeBytes <- o .: "maxSizeBytes"
            _rpKeepFiles    <- o .: "keepFiles"
            let result = RotationParameters{..}
            if isValidRotation result
            then pure result
            else fail "All RotationParameters must be positive"

-- | Checks if logger rotation parameters are valid. All fields must be positive.
isValidRotation :: RotationParameters -> Bool
isValidRotation RotationParameters{..} = _rpMaxSizeBytes > 0 && _rpKeepFiles > 0

-- | Dummy type to represent absence of 'RotationParameters' in global configuration.
data NoRotation = NoRotation
    deriving (Show, Eq, Generic)

instance ToJSON NoRotation

-- | Throws parsing error if you specified @"rotation"@ parameters.
instance FromJSON NoRotation where
    parseJSON = withObject "NoRotation" $ \o -> case HashMap.lookup "rotation" o of
        Nothing -> pure NoRotation
        Just _  -> fail "Found 'rotation' parameters in configuration but \
                        \rotation action wasn't applied to this configuration!"

-- | Deduces type of rotation parameters in configuration based on list of extensions.
type RotationExtension exts = ExtensionType 'Rotation exts RotationParameters NoRotation

makeLenses ''RotationParameters
