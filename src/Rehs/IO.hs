------------------------------------------------------------
-- IO Helpers
-- This modules declares a helper functions to be able to
-- execute table transactions within IO.
-- Notice that all functions in this module return IO's
------------------------------------------------------------

module Rehs.IO
  ( newTableIO
  , updateAndReadSlotIO
  ) where

import Rehs
import Control.Concurrent.STM

newTableIO :: IO Table
newTableIO = atomically newTable

updateAndReadSlotIO :: SlotTransaction -> Table -> IO KVTable
updateAndReadSlotIO transaction = atomically . updateAndReadSlot transaction
