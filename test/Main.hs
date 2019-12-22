{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson
import qualified Data.ByteString.Lazy.Char8
import qualified Development.Shake as Shake
import Hedgehog
import qualified Network.Bugsnag.Gen
import qualified System.Exit

main :: IO ()
main = do
  res <-
    checkParallel $
      Group
        "Network.Hedgehog"
        [("validated by schema", validatedBySchema)]
  System.Exit.exitWith $ if res then System.Exit.ExitSuccess else System.Exit.ExitFailure 1

validatedBySchema :: Property
validatedBySchema = property $ do
  r <- forAll Network.Bugsnag.Gen.report
  let json = Data.ByteString.Lazy.Char8.unpack (Data.Aeson.encode r)
  annotate json
  evalEither =<< evalIO (validateAgainstSchema json)

validateAgainstSchema :: (MonadIO m) => String -> m (Either String ())
validateAgainstSchema json = do
  (Shake.Exit code, Shake.Stderr stdout) <- liftIO $ Shake.cmd (Shake.Stdin json) ["jsonschema", "-i", "/dev/stdin", "test/report-schema.json" :: String]
  pure $ case code of
    System.Exit.ExitSuccess -> Right ()
    System.Exit.ExitFailure _ -> Left (stdout :: String)
