module Test.MySolutions where

import Prelude

import Data.AddressBook (AddressBook, Entry)
import Data.List (filter, head, nubByEq, null)
import Data.Maybe (Maybe)

-- Note to reader: Add your solutions to this file

findEntryByStreetVerbose :: String -> AddressBook -> Maybe Entry
findEntryByStreetVerbose street = (filter \entry -> entry.address.street == street) >>> head

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street = filter (_.address.street >>> eq street) >>> head

findEntryByStreetWithWhere :: String -> AddressBook -> Maybe Entry 
findEntryByStreetWithWhere street = filter filterEntry >>> head
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.address.street == street

isInBook :: String -> String ->  AddressBook -> Boolean
isInBook firstName lastName =
  filter (_.firstName >>> eq firstName && _.lastName >>> eq lastName) >>> not null

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubByEq haveDuplicates
  where
    haveDuplicates :: Entry -> Entry -> Boolean
    haveDuplicates entry1 entry2 =
      entry1.firstName == entry2.firstName && entry1.lastName == entry2.lastName
