{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

-- |
-- Module: Control.Concurrent.STM.ETQueue
-- Copyright: Copyright © 2014 AlephCloud Systems, Inc.
-- License: MIT
-- Maintainer: Jon Sterling <jsterling@alephcloud.com>
-- Stability: experimental
--
module Control.Concurrent.STM.ETQueue
( ETQueue
, newETQueue
, newETQueueIO
, readETQueue
, tryReadETQueue
, peekETQueue
, tryPeekETQueue
, writeETQueue
, unGetETQueue
, isEmptyETQueue
, awaitEmpty
) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad

-- | A variant of 'TQueue' such that you can await its emptiness.
data ETQueue α
  = ETQueue
  { etqQueue ∷ TQueue α
  , etqVar ∷ TMVar ()
  }

newETQueue ∷ STM (ETQueue α)
newETQueue = do
  etqQueue ← newTQueue
  etqVar ← newTMVar ()
  return ETQueue {..}

newETQueueIO ∷ IO (ETQueue α)
newETQueueIO = do
  etqQueue ← newTQueueIO
  etqVar ← newTMVarIO ()
  return ETQueue {..}

readETQueue
  ∷ ETQueue α
  → STM α
readETQueue ETQueue{..} = do
  a ← readTQueue etqQueue
  isEmpty ← isEmptyTQueue etqQueue
  when isEmpty $
    () <$ tryPutTMVar etqVar ()
  return a

tryReadETQueue
  ∷ ETQueue α
  → STM (Maybe α)
tryReadETQueue q =
  (Just <$> readETQueue q)
    `orElse` return Nothing

peekETQueue
  ∷ ETQueue α
  → STM α
peekETQueue ETQueue{..} =
  peekTQueue etqQueue

tryPeekETQueue
  ∷ ETQueue α
  → STM (Maybe α)
tryPeekETQueue ETQueue{..} =
  tryPeekTQueue etqQueue

writeETQueue
  ∷ ETQueue α
  → α
  → STM ()
writeETQueue ETQueue{..} a = do
  writeTQueue etqQueue a
  () <$ tryTakeTMVar etqVar

unGetETQueue
  ∷ ETQueue α
  → α
  → STM ()
unGetETQueue ETQueue{..} a = do
  unGetTQueue etqQueue a
  () <$ tryTakeTMVar etqVar

isEmptyETQueue
  ∷ ETQueue α
  → STM Bool
isEmptyETQueue ETQueue{..} =
  isEmptyTQueue etqQueue

awaitEmpty
  ∷ ETQueue α
  → STM ()
awaitEmpty ETQueue{..} =
  readTMVar etqVar
