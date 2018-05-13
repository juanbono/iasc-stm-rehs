------------------------------------------------------------
-- Command parsing functions
-- This module declares functions for parsing the user input
-- and converting it into Slot Transaction - functions that
-- return an STM ().
------------------------------------------------------------

module Rehs.Commands
  ( parseSlotTransactionLine
  , OpType (..)
  , Operation (..)
  ) where

import Data.List.Split (splitOn)
import Rehs

type Command = [String]

data OpType
  = Set | Clear | Read | Schema
  deriving (Eq, Show)

data Operation a
  = Op OpType a

instance Show a => Show (Operation a) where
  show (Op Schema _) = "<OK>"
  show (Op _ a) = show a

parseSlotTransactionLine :: String -> Operation SlotTransaction
parseSlotTransactionLine = parseSlotTransactionCommand . splitOn ":"

parseSlotTransactionCommand :: Command -> Operation SlotTransaction
parseSlotTransactionCommand ["set", k, v] = Op Set $ Rehs.setTransaction k v
parseSlotTransactionCommand ["clear"]     = Op Clear $ Rehs.clearTransaction
parseSlotTransactionCommand ["read"]      = Op Read $ Rehs.readTransaction
parseSlotTransactionCommand ("schema":xs) = Op Schema $ Rehs.setSchemaTransaction xs



