------------------------------------------------------------
-- Main definitions.
-- This modules defines a Table and its operations
-- Notice that all functions in this module return STM's
------------------------------------------------------------

module Rehs
  ( Table
  , SlotTransaction
  , newTable
  , updateAndReadSlot
  , setTransaction
  , clearTransaction
  , readTransaction
  , emptyKVTable
  , KVTable
  ) where

import Control.Concurrent.STM

type KVTable = [(String, String)]
type Table = TVar KVTable
type SlotTransaction = Table -> STM ()

newTable :: STM Table
newTable =  newTVar []

emptyKVTable :: KVTable
emptyKVTable = []

updateAndReadSlot :: SlotTransaction -> Table -> STM KVTable
updateAndReadSlot transaction table = transaction table >> readTVar table

setTransaction :: String -> String -> SlotTransaction
setTransaction key value = \table -> modifyTVar table
  (\oldTable -> (key, value):oldTable)

clearTransaction :: SlotTransaction
clearTransaction  = \table -> writeTVar table []

readTransaction :: SlotTransaction
readTransaction = \table -> return ()
