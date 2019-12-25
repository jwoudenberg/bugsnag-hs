{-# LANGUAGE NamedFieldPuns #-}

-- |
-- A buffer for batched write operations. Push individual items into the buffer
-- and provide an operation that writes out batches of them.
module Data.Buffer
  ( -- * Buffer
    Buffer,
    new,
    push,
    flush,

    -- * Settings
    Settings,
    defaultSettings,
    write,
    size,
    frequencyInMicroSeconds,
  )
where

import qualified Control.Concurrent.STM as STM
import qualified Control.Debounce as Debounce
import qualified Data.List.NonEmpty as NonEmpty
import GHC.Natural (Natural)

-- | A buffer for write operations.
data Buffer a
  = Buffer
      { -- | Push a new item into the buffer.
        push :: a -> IO (),
        -- | Flush the current contents of the buffer.
        flush :: IO ()
      }

-- | Configuration settings for a new 'Buffer'.
data Settings a
  = Settings
      { -- | Function to write a batch of items.
        write :: NonEmpty.NonEmpty a -> IO (),
        -- | The maximum amount of items to write in a single batch.
        size :: Natural,
        -- | The frequency with which the buffer gets flushed automatically.
        frequencyInMicroSeconds :: Int
      }

-- | Default 'Buffer' settings. A buffer created by these settings has a size
-- of 1 and writes items out as soon as they come in.
defaultSettings :: Settings a
defaultSettings =
  Settings
    { write = \_ -> pure (),
      size = 1,
      frequencyInMicroSeconds = 0
    }

-- | Creates a new 'Buffer'.
new :: Settings a -> IO (Buffer a)
new settings = do
  queue <- STM.atomically $ STM.newTBQueue (fromIntegral (size settings))
  let writeList xs = maybe (pure ()) (write settings) (NonEmpty.nonEmpty xs)
  let flush = writeList =<< STM.atomically (STM.flushTBQueue queue)
  scheduleFlush <-
    Debounce.mkDebounce
      Debounce.defaultDebounceSettings
        { Debounce.debounceAction = flush,
          Debounce.debounceFreq = frequencyInMicroSeconds settings
        }
  let push x = do
        overflow <- STM.atomically $ do
          full <- STM.isFullTBQueue queue
          toFlush <- if full then STM.flushTBQueue queue else pure []
          STM.writeTBQueue queue x
          pure toFlush
        -- Flush the items that overflow the queue immediately.
        writeList overflow
        -- Schedule a regular flush for the rest.
        scheduleFlush
  pure Buffer {push, flush}
