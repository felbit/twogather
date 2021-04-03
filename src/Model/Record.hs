{-|
Module      : Model.Record
Description : Transaction Record
Stability   : experimental

A record represents a single transaction -- usually a spending from one of the
involved parties.
-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Model.Record where

import           Data.Aeson
import           Data.Text                      ( Text )
import           Data.Time.Calendar
import           Database.Persist.TH
import           GHC.Generics
import           Lucid
import           Web.FormUrlEncoded             ( FromForm )

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase| 
Record
  username Text
  amount   Int
  comment  Text
  date     Day

  deriving Eq Show Generic
|]

instance ToJSON Record
instance FromJSON Record

instance ToHtml Record where
  toHtml r = tr_ $ do
    td_ $ toHtml (showGregorian $ recordDate r)
    td_ $ toHtml (recordUsername r)
    td_ $ toHtml (recordComment r)
    td_ $ toHtml $ show (recordAmount r) ++ " SEK"
  toHtmlRaw = toHtml

instance ToHtml [Record] where
  toHtml rs = with table_ [class_ "table"] $ do
    thead_ $ tr_ $ do
      th_ "Date"
      th_ "Name"
      th_ "Comment"
      th_ "Amount"
    tbody_ $ mapM_ toHtml rs
    tfoot_ $ tr_ $ do
      td_ ""
      td_ ""
      td_ "Sum:"
      td_ $ toHtml (show (sum $ recordAmount <$> rs) ++ " SEK")
  toHtmlRaw = toHtml

data RecordForm = RecordForm
  { ramount  :: Int
  , rcomment :: !Text
  , ruser    :: !Text
  }
  deriving (Eq, Show, Generic)

instance FromForm RecordForm
