{-# LANGUAGE DeriveGeneric #-}

module Model where

import           Data.Aeson
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Time.Calendar
import           GHC.Generics

data TgRecord = TgRecord
  { amount  :: Float
  , comment :: Text
  , date    :: Day
  , user    :: TgUser
  }
  deriving (Eq, Show, Generic)

instance ToJSON TgRecord
instance FromJSON TgRecord

data TgUser = TgUser
  { name :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON TgUser
instance FromJSON TgUser

-- Database
users :: [TgUser]
users = [TgUser "Martin", TgUser "Paula"]

records :: [TgRecord]
records =
  [ TgRecord 134.00 "ICA"         (fromGregorian 2021 3 26) (TgUser "Martin")
  , TgRecord 231.80 "MAX Burgers" (fromGregorian 2021 3 25) (TgUser "Martin")
  , TgRecord 59.00
             "Helgkasse Godisbanken"
             (fromGregorian 2021 3 18)
             (TgUser "Paula")
  , TgRecord 29.00  "Karma"   (fromGregorian 2021 3 17) (TgUser "Martin")
  , TgRecord 129.00 "Willy:s" (fromGregorian 2021 3 15) (TgUser "Paula")
  ]
