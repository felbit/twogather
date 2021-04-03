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

{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Model.Settlement where

import           Data.Aeson
import           Data.Text                      ( Text )
import           Database.Persist.TH
import           GHC.Generics
import           Lucid
-------------------------------------------------------------------------------
import           Model.SettleState              ( SettleState(..) )

-- |Settlement describes a necessary transaction to happen to equal out
-- spending between the parties in a given month. 'creditor' is the party
-- to whom the settlement is owed (the one who spent more during the month).
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Settlement
  state    SettleState
  year     Int
  month    Int
  creditor Text

  deriving Eq Show Generic
|]

instance ToJSON Settlement
instance FromJSON Settlement

instance ToHtml Settlement where
  toHtml s = with div_ [class_ "row my-3 d-flex justify-content-between"] $ do
    with div_
         [class_ "col"]
         (toHtml $ show (settlementYear s) <> "-" <> show (settlementMonth s))
    -- with div_ [class_ "col"] (toHtml $ settlementAmount s)
    with div_ [class_ "col"] $ if settlementState s == OUTSTANDING
      then with button_ [class_ "btn btn-primary"] "Transferred"
      else with button_ [class_ "btn btn-success"] "Settled"
  toHtmlRaw = toHtml

-- settlementAmount :: Settlement -> [Record] -> Text
-- settlementAmount s db = _impl
