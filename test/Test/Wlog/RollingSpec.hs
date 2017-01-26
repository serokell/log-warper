-- | Tests for rolling logger.

module Test.Wlog.RollingSpec
       ( spec
       ) where

import           Universum

import           Control.Concurrent.Async (mapConcurrently)
import           Data.Default             (def)
import qualified Data.HashMap.Strict      as HM (fromList)
import qualified Prelude                  (read)
import           System.Directory         (doesFileExist, removeFile)
import           System.FilePath          (takeExtension)
import           System.IO                (hFileSize)

import           Test.Hspec               (Spec, describe, it, shouldThrow)
import           Test.Hspec.QuickCheck    (prop)
import           Test.QuickCheck          (Arbitrary (..), Large (..), Positive (..),
                                           Property, (==>))
import           Test.QuickCheck.Monadic  (PropertyM, assert, monadicIO, run)

import           System.Wlog              (InvalidRotation (..), LoggerConfig (..),
                                           LoggerTree (..), RotationParameters (..),
                                           Severity (..), isValidRotation, logDebug,
                                           logIndex, releaseAllHandlers,
                                           rotationFileHandler, traverseLoggerConfig,
                                           usingLoggerName, whenExist)

spec :: Spec
spec = describe "System.Wlog.Roller" $ do
    describe "Exception" $ do
        it "throws exception in case of invalid roller params" $ do
            let wrongRP1 = RotationParameters 0 1
            let wrongRP2 = RotationParameters 1 0
            let expectedRollException rp =
                  (== InvalidRotation ("Rotation parameters must be positive: " <> pretty rp))
            let rollExceptionChecker rp
                  = rotationFileHandler rp "" (panic "Test roll!")
                      `shouldThrow`
                    expectedRollException rp

            rollExceptionChecker wrongRP1
            rollExceptionChecker wrongRP2

    describe "Concurrent rolling" $
        prop description_verifyLoggerRotation verifyLoggerRotation
  where
    description_verifyLoggerRotation =
      "logger rotation successfully creates files in concurrent environment"

instance Arbitrary RotationParameters where
    arbitrary = do
        Large    rpLogLimit  <- arbitrary
        Positive rpKeepFiles <- arbitrary
        pure RotationParameters{..}

testLogFile :: FilePath
testLogFile = "patak.log"

testLoggerConfig :: RotationParameters -> LoggerConfig
testLoggerConfig (Just -> lcRotation) = LoggerConfig{..}
  where
    lcTree = def { ltSeverity   = Just Debug
                 , ltSubloggers = HM.fromList [("test", def { ltFile = Just testLogFile })]
                 }

logThreadsNum :: Word
logThreadsNum = 1

-- | Runs 'logThreadsnum' threads to write dummy logs into @log-warper\/logs\/patak.log@ file.
writeConcurrentLogs :: RotationParameters -> IO ()
writeConcurrentLogs rp@RotationParameters{..} =
    bracket_
        (traverseLoggerConfig identity lcRotation lcTree $ Just "logs")
        releaseAllHandlers
        concurrentWriting
  where
    LoggerConfig{..}  = testLoggerConfig rp
    concurrentWriting = () <$ mapConcurrently logThread [1 .. logThreadsNum]

    -- | Starting thread with number @i@
    logThread _i = replicateM 10 $ usingLoggerName "test" $ logDebug "skovoroda"

-- | Verifies that logging in concurrent application works correctly. Thus
-- * it actually works
-- * it works when multiple threads trying to write to log file
-- * size of each file is not very big
-- * number of files is not bigger than allowed
-- TODO: more properties?
verifyLoggerRotation :: RotationParameters -> Property
verifyLoggerRotation rp@RotationParameters{..} = isValidRotation rp ==> monadicIO $ do
    run $ writeConcurrentLogs rp
    checkFilesNumber
    checkFilesSize
    run $ cleanupFiles
  where
    testLogPath  = "logs/patak.log"

    -- | Checks assert on every rolled file if this file exist
    forRolledOut :: MonadIO m => Bool -> (FilePath -> m ()) -> m ()
    forRolledOut checkNonIndexed rollAction = do
        for_ [0 .. rpKeepFiles - 1] $ \i -> do
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
            assert (0 <= index && index < rpKeepFiles - 1)
        for_ [rpKeepFiles .. 2 * rpKeepFiles] $ \i -> do
            let logFileName = logIndex testLogPath i
            assert =<< (not <$> liftIO (doesFileExist logFileName))

    checkFilesSize :: MonadIO m => PropertyM m ()
    checkFilesSize = forRolledOut True $ \filePath -> do
        fileSize <- liftIO $ withFile filePath ReadMode hFileSize
        assert (fromIntegral fileSize < 2 * rpLogLimit)
