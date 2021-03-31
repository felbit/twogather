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
{-# LANGUAGE MonoLocalBinds             #-}
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
    td_ $ toHtml (recordComment r)
    with td_ [class_ "d-flex justify-content-end"]
      $  toHtml
      $  show (recordAmount r)
      ++ " SEK"
    td_ $ toHtml (recordUsername r)
  toHtmlRaw = toHtml
