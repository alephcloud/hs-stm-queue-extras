-- Copyright (c) 2013-2014 PivotCloud, Inc.
--
-- Control.Concurrent.STM.Queue
--
-- Please feel free to contact us at licensing@pivotmail.com with any
-- contributions, additions, or other feedback; we would love to hear from
-- you.
--
-- Licensed under the Apache License, Version 2.0 (the "License"); you may
-- not use this file except in compliance with the License. You may obtain a
-- copy of the License at http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
-- WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
-- License for the specific language governing permissions and limitations
-- under the License.
--

{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}

module Control.Concurrent.STM.Queue
( Queue(..)
, awaitQueueEmpty
) where

import Control.Concurrent.STM
import Control.Concurrent.STM.TBMQueue
import Control.Concurrent.STM.TMQueue
import Control.Monad

class Queue q where
  type QueueValue q α

  readQueue
    ∷ q α
    → STM (QueueValue q α)

  tryReadQueue
    ∷ q α
    → STM (Maybe (QueueValue q α))

  peekQueue
    ∷ q α
    → STM (QueueValue q α)

  tryPeekQueue
    ∷ q α
    → STM (Maybe (QueueValue q α))

  writeQueue
    ∷ q α
    → α
    → STM ()

  unGetQueue
    ∷ q α
    → α
    → STM ()

  isEmptyQueue
    ∷ q α
    → STM Bool

instance Queue TQueue where
  type QueueValue TQueue α = α
  readQueue = readTQueue
  tryReadQueue = tryReadTQueue
  peekQueue = peekTQueue
  tryPeekQueue = tryPeekTQueue
  writeQueue = writeTQueue
  unGetQueue = unGetTQueue
  isEmptyQueue = isEmptyTQueue

instance Queue TBQueue where
  type QueueValue TBQueue α = α
  readQueue = readTBQueue
  tryReadQueue = tryReadTBQueue
  peekQueue = peekTBQueue
  tryPeekQueue = tryPeekTBQueue
  writeQueue = writeTBQueue
  unGetQueue = unGetTBQueue
  isEmptyQueue = isEmptyTBQueue

instance Queue TMQueue where
  type QueueValue TMQueue α = Maybe α
  readQueue = readTMQueue
  tryReadQueue = tryReadTMQueue
  peekQueue = peekTMQueue
  tryPeekQueue = tryPeekTMQueue
  writeQueue = writeTMQueue
  unGetQueue = unGetTMQueue
  isEmptyQueue = isEmptyTMQueue

instance Queue TBMQueue where
  type QueueValue TBMQueue α = Maybe α
  readQueue = readTBMQueue
  tryReadQueue = tryReadTBMQueue
  peekQueue = peekTBMQueue
  tryPeekQueue = tryPeekTBMQueue
  writeQueue = writeTBMQueue
  unGetQueue = unGetTBMQueue
  isEmptyQueue = isEmptyTBMQueue

-- | Block until a queue is empty.
awaitQueueEmpty
  ∷ Queue q
  ⇒ q α
  → STM ()
awaitQueueEmpty =
  isEmptyQueue >=> check

