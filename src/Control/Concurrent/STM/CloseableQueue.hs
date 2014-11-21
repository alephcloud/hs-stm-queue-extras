-- Copyright (c) 2013-2014 PivotCloud, Inc.
--
-- Control.Concurrent.STM.CloseableQueue
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

module Control.Concurrent.STM.CloseableQueue
( CloseableQueue(..)
) where

import Control.Concurrent.STM
import Control.Concurrent.STM.TBMQueue
import Control.Concurrent.STM.TMQueue
import Control.Concurrent.STM.Queue

class Queue q ⇒ CloseableQueue q where
  closeQueue
    ∷ q α
    → STM ()

  isClosedQueue
    ∷ q α
    → STM Bool

instance CloseableQueue TMQueue where
  closeQueue = closeTMQueue
  isClosedQueue = isClosedTMQueue

instance CloseableQueue TBMQueue where
  closeQueue = closeTBMQueue
  isClosedQueue = isClosedTBMQueue
