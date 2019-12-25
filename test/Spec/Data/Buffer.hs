{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.Data.Buffer
  ( tests,
  )
where

import qualified Control.Concurrent
import qualified Control.Concurrent.STM as STM
import qualified Data.Buffer as Buffer
import qualified Data.Foldable
import qualified Data.List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Maybe
import GHC.Natural (Natural)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified System.Exit

tests :: IO ()
tests = do
  res <- tests'
  if res
    then pure ()
    else System.Exit.die "Spec.Data.Buffer tests failed"

tests' :: IO Bool
tests' =
  checkParallel $
    Group
      "Spec.Data.Buffer"
      [ ("all items get sent", allItemsGetSent)
      ]

allItemsGetSent :: Property
allItemsGetSent = property $ do
  steps <- forAll $ numberItems <$> Gen.list (Range.linear 0 100) step
  size <- forAll $ fromIntegral <$> Gen.int (Range.linear 1 10)
  frequencyInMicroSeconds <- forAll $ waitMicroS
  actualOutput <- evalIO (run size frequencyInMicroSeconds steps)
  let expectedOutput = Data.Maybe.mapMaybe toItem steps
  Data.List.sort actualOutput === Data.List.sort expectedOutput

data Step
  = Push Item
  | WaitMicroS Int
  | Flush
  deriving (Show)

newtype Item = Item Int
  deriving (Eq, Ord, Show)

step :: Gen Step
step =
  Gen.frequency
    [ (5, Gen.constant (Push (Item 0))),
      (5, WaitMicroS <$> waitMicroS),
      (1, Gen.constant Flush)
    ]

numberItems :: [Step] -> [Step]
numberItems = snd . Data.List.mapAccumL numberItem 0

numberItem :: Int -> Step -> (Int, Step)
numberItem counter step =
  case step of
    Push (Item _) -> (counter + 1, Push (Item counter))
    _ -> (counter, step)

waitMicroS :: Gen Int
waitMicroS = Gen.int (Range.linear 0 maxWaitMicroS)

maxWaitMicroS :: Int
maxWaitMicroS = 10000

toItem :: Step -> Maybe Item
toItem step =
  case step of
    Push item -> Just item
    WaitMicroS _ -> Nothing
    Flush -> Nothing

run :: Natural -> Int -> [Step] -> IO [Item]
run size frequencyInMicroSeconds steps = do
  -- Setup the buffer to play the steps through.
  resultQueue <- STM.newTQueueIO
  buffer <-
    Buffer.new
      Buffer.defaultSettings
        { Buffer.write = writeToQueue resultQueue,
          Buffer.size,
          Buffer.frequencyInMicroSeconds
        }
  -- Play the steps one by one.
  Data.Foldable.traverse_ (play buffer) steps
  -- Give the queue the opportunity to flush by itself.
  Control.Concurrent.threadDelay (2 * maxWaitMicroS)
  mconcat <$> STM.atomically (STM.flushTQueue resultQueue)

play :: Buffer.Buffer Item -> Step -> IO ()
play buffer step =
  case step of
    Push item -> Buffer.push buffer item
    WaitMicroS delay -> Control.Concurrent.threadDelay delay
    Flush -> Buffer.flush buffer

writeToQueue :: STM.TQueue [a] -> NonEmpty.NonEmpty a -> IO ()
writeToQueue queue list = STM.atomically (STM.writeTQueue queue (NonEmpty.toList list))
