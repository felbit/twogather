{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.SettleState where

import Data.Aeson
import Database.Persist.TH
import GHC.Generics

data SettleState
  = OUTSTANDING -- ^ Todo
  | TRANSACTED  -- ^ Money sent, settlement initiated
  | SETTLED     -- ^ Done, settlement cleared
  deriving (Show, Read, Eq, Generic)

instance ToJSON SettleState
instance FromJSON SettleState

derivePersistField "SettleState"
