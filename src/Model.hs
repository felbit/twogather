{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE UndecidableInstances #-}

module Model where

import           Data.Aeson
import           Data.Text          (Text)
import qualified Data.Text          as T
import           Data.Time.Calendar
import           GHC.Generics
import           Lucid

data TgRecord =
  TgRecord
    { amount  :: Int
    , comment :: Text
    , date    :: Day
    , user    :: Text
    }
  deriving (Eq, Show, Generic)

instance ToJSON TgRecord

instance FromJSON TgRecord

instance ToHtml TgRecord where
  toHtml r =
    tr_ $ do
      td_ $ toHtml (showGregorian $ date r)
      td_ $ toHtml (comment r)
      with td_ [class_ "d-flex justify-content-end"] $
        toHtml $ show (amount r) ++ " SEK"
      td_ $ toHtml (user r)
  toHtmlRaw = toHtml

-- Form data
data TgRecord' =
  TgRecord'
    { amount'  :: Int
    , comment' :: Text
    , user'    :: Text
    }
  deriving (Eq, Show, Generic)

instance FromJSON TgRecord'

-- Database
records :: [TgRecord]
records =
  [ TgRecord 134 "ICA" (fromGregorian 2021 3 26) "Martin"
  , TgRecord 231 "MAX Burgers" (fromGregorian 2021 3 25) "Martin"
  , TgRecord 59 "Helgkasse Godisbanken" (fromGregorian 2021 3 18) "Paula"
  , TgRecord 29 "Karma" (fromGregorian 2021 3 17) "Martin"
  , TgRecord 129 "Willy:s" (fromGregorian 2021 3 15) "Paula"
  ]
