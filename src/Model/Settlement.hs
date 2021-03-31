{-|
Module      : Model.Settlement
Description : Settling differences in spending month by month
Stability   : experimental

twogather works periodically in calendar months. Every month the balance
between both parties is checked and a settlement is created. A settlement
is a task to transfer monay from one party to the other dependend on the
amounts spent during that month.

E.g. Alice has spent 234.80 and Bob has spent 421.42 of the same currency.
The average amount spent that month is (234.80 + 421.42) / 2 = __328.11__.
Since Alice spent less than Bob she has to transfer 93.31 to Bob to settle
the month.

Todo:
  - only recalculate while in 'OUTSTANDING' state
    (pin amount in action OUTSTANDING->TRANSACTED)
  - allow partial settlements 
    - save transfered ammount
-}

{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}

module Model.Settlement where

import           Data.Aeson
import           Data.Text                      ( Text )
import           Database.Persist.TH

data SettleState
  = OUTSTANDING -- ^ Todo
  | TRANSACTED -- ^ Money sent, settlement initiated
  | SETTLED -- ^ Done, settlement cleared
  deriving (Show, Read, Eq)
derivePersistField "SettleState"

-- |Settlement describes a necessary transaction to happen to equal out
-- spendings between the parties in a given month.
-- Here, 'creditor' is the party to whom the settlement is owed (the one
-- who spent more during the month).
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Settlement
  state    SettleState
  year     Integer
  month    Int
  creditor Text

  deriving Eq Show Generic
|]

instance ToJSON Settlement
instance FromJSON Settlement
