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
import Rehs.Commands

newTableIO :: IO Table
newTableIO = atomically newTable

updateAndReadSlotIO :: Operation SlotTransaction -> Table -> IO (Operation KVTable)
updateAndReadSlotIO (Op t transaction) table = Op t <$> (applyTrans table)
  where
    applyTrans = atomically . updateAndReadSlot transaction
