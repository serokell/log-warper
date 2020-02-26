{-# OPTIONS_GHC -Wno-orphans #-}

-- | Tests for rolling logger.

module Test.Wlog.RollingSpec
       ( spec
       ) where

import Universum

import Control.Concurrent.Async (mapConcurrently)
import Lens.Micro.Mtl (zoom, (.=), (?=))
import System.Directory (doesFileExist, removeFile)
import System.FilePath (takeExtension)
import System.IO (hFileSize)

import Test.Hspec (Spec, describe, it, shouldThrow)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.HUnit.Base (assert)
import Test.QuickCheck (Arbitrary (..), Property, choose, (==>))
import Test.QuickCheck.Monadic (PropertyM, monadicIO, run)

import System.Wlog (HandlerWrap (..), InvalidRotation (..), LoggerConfig (..),
                    RotationParameters (..), debugPlus, fromScratch, isValidRotation,
                    lcLogsDirectory, lcRotation, lcTree, logDebug, logIndex, ltFiles, ltSeverity,
                    removeAllHandlers, rotationFileHandler, setupLogging, usingLoggerName,
                    whenExist, zoomLogger)

import qualified Prelude (read)

spec :: Spec
spec = do
    let smaller = modifyMaxSuccess $ const 30
    describe "System.Wlog.Roller" $ do
      describe "Exception" $
          it "throws exception in case of invalid roller params" $ do
              let wrongRP1 = RotationParameters 0 1
              let wrongRP2 = RotationParameters 1 0
              let expectedRollException rp =
                    (== InvalidRotation ("Rotation parameters must be positive: " <> show rp))
              let rollExceptionChecker rp
                    = rotationFileHandler rp "" (error "Test roll!")
                        `shouldThrow`
                      expectedRollException rp

              rollExceptionChecker wrongRP1
              rollExceptionChecker wrongRP2

      describe "Concurrent rolling" $ smaller $
          prop description_verifyLoggerRotation verifyLoggerRotation
  where
    description_verifyLoggerRotation =
      "logger rotation successfully creates files in concurrent environment"

instance Arbitrary RotationParameters where
    arbitrary = do
        rpLogLimit  <- choose (1000, 10000)
        rpKeepFiles <- choose (1, 10)
        pure RotationParameters{..}

newtype LinesToLog = LinesToLog { getNumberOfLinesToLog :: Word64 }
    deriving (Show)

instance Arbitrary LinesToLog where
    arbitrary = LinesToLog <$> choose (1, 500)

testLogFile :: FilePath
testLogFile = "patak.log"

testLoggerConfig :: RotationParameters -> LoggerConfig
testLoggerConfig rotParam = fromScratch $ do
    lcRotation      ?= rotParam
    lcLogsDirectory ?= "logs"
    zoom lcTree $ do
        ltSeverity ?= debugPlus
        zoomLogger "test" $
            ltFiles .= [HandlerWrap testLogFile Nothing]

logThreadsNum :: Word
logThreadsNum = 4

-- | Runs 'logThreadsnum' threads to write dummy logs into @log-warper\/logs\/patak.log@ file.
writeConcurrentLogs :: RotationParameters -> LinesToLog -> IO ()
writeConcurrentLogs rp@RotationParameters{..} (getNumberOfLinesToLog -> linesNum) =
    bracket_
        (setupLogging Nothing $ testLoggerConfig rp)
        removeAllHandlers
        concurrentWriting
  where
    LoggerConfig{..}  = testLoggerConfig rp
    concurrentWriting = () <$ mapConcurrently logThread [1 .. logThreadsNum]

    -- | Starting thread with number @i@
    logThread i = forM_ [1 .. linesNum] $ \j ->
        usingLoggerName "test" $ logDebug $ "skovoroda: " <> show (i, j)

-- | Verifies that logging in concurrent application works correctly. Thus
-- * it actually works
-- * it works when multiple threads trying to write to log file
-- * size of each file is not very big
-- * number of files is not bigger than allowed
-- * at least one (root) file with logs is created
-- TODO: more properties?
verifyLoggerRotation :: RotationParameters -> LinesToLog -> Property
verifyLoggerRotation rp@RotationParameters{..} linesNum = isValidRotation rp ==> monadicIO $ do
    run $ writeConcurrentLogs rp linesNum
    checkFilesNumber
    checkFilesSize
    run cleanupFiles
  where
    testLogPath  = "logs/patak.log"

    -- | Checks assert on every rolled file if this file exist
    forRolledOut :: MonadIO m => Bool -> (FilePath -> m ()) -> m ()
    forRolledOut checkNonIndexed rollAction = do
        for_ [0 .. rpKeepFiles - 1] $ \(fromIntegral -> i) -> do
            let logFileName = logIndex testLogPath i
            whenExist logFileName rollAction
        when checkNonIndexed $ rollAction testLogPath

    -- | Removes all files after performing tests
    cleanupFiles :: IO ()
    cleanupFiles = forRolledOut True removeFile

    checkFilesNumber :: MonadIO m => PropertyM m ()
    checkFilesNumber = do
        forRolledOut False $ \filePath -> do
            let ('.' : rest) = takeExtension  filePath
            let index = Prelude.read rest
            liftIO $ assert (0 <= index && index < rpKeepFiles - 1)

        for_ [rpKeepFiles .. 2 * rpKeepFiles] $ \(fromIntegral -> i) -> do
            let logFileName = logIndex testLogPath i
            liftIO $ assert (not <$> doesFileExist logFileName)

    checkFilesSize :: MonadIO m => PropertyM m ()
    checkFilesSize = forRolledOut True $ \filePath -> do
        fileSize <- liftIO $ withFile filePath ReadMode hFileSize
        liftIO $ assert (fromIntegral fileSize < 2 * rpLogLimit)
