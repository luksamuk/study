module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List(List(..), filter, head, null, nubBy)
import Data.Maybe (Maybe)

type Entry =
  { firstName :: String
  , lastName  :: String
  , address   :: Address }

type Address =
  { street :: String
  , city   :: String
  , state  :: String }

type AddressBook = List Entry

-- Print entries as string
showEntry :: Entry -> String
showEntry entry = entry.lastName  <> ", " <>
                  entry.firstName <> ": " <>
                  showAddress entry.address

showAddress :: Address -> String
showAddress addr = addr.street <> ", " <>
                   addr.city   <> ", " <>
                   addr.state

-- Test data
fake_address :: Address
fake_address = { street: "123 Fake St.", city: "Faketown", state: "CA" }

fake_entry :: Entry
fake_entry = { firstName: "John", lastName: "Smith", address: fake_address }


-- Manage address books
emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName book = (head <<< filter filterEntry) book
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

-- Alternative, equal function definitions:
-- findEntry firstName lastName book = head (filter filterEntry book)
-- findEntry firstName lastName book = head $ filter filterEntry book
-- findEntry firstName lastName book = (filter filterEntry >>> head) book

printEntry :: String -> String -> AddressBook -> Maybe String
printEntry firstName lastName book =
  map showEntry (findEntry firstName lastName book)

-- More test data
book1 :: AddressBook
book1 = insertEntry fake_entry emptyBook

other_entry :: Entry
other_entry =
  { firstName: "Fulano"
  , lastName:  "Silva"
  , address:
    { street: "Rua dos Bobos, 0"
    , city:   "Tangamandapio"
    , state:  "WAT" } }

book2 :: AddressBook
book2 = insertEntry other_entry book1

-- printEntry "Fulano" "Silva" book2

-- Exercise 1
-- head :: AddressBook -> Maybe Entry
-- filterEntry :: Entry -> Boolean
-- filter :: filterEntry -> AddressBook -> AddressBook

-- Exercise 2
-- TODO: Return whole list instead of first entry?
findEntryByAddress :: String -> String -> String -> AddressBook -> Maybe Entry
findEntryByAddress street city state book =
  (head <<< filter filterEntryByAddress) book
  where
    filterEntryByAddress :: Entry -> Boolean
    filterEntryByAddress entry = entry.address.street == street &&
                                 entry.address.state  == state  &&
                                 entry.address.city   == city

-- findEntryByAddress "123 Fake St." "Faketown" "CA" book2


-- Exercise 3
nameExists :: String -> String -> AddressBook -> Boolean
nameExists firstName lastName book = not (null <<< filter filterEntry) book
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.firstName == firstName &&
                        entry.lastName == lastName

-- nameExists "Fulano" "Silva" book2


-- Exercise 4
-- Data.List.nubBy removes duplicates based on a predicate.
removeDuplicates :: AddressBook -> AddressBook
removeDuplicates book = nubBy isSamePerson book
  where
    isSamePerson :: Entry -> Entry -> Boolean
    isSamePerson a b = a.firstName == b.firstName &&
                       a.lastName  == b.lastName

-- book_dup :: AddressBook
-- book_dup = insertEntry other_entry $ insertEntry fake_entry book2
-- removeDuplicates book_dup
