{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE UndecidableInstances #-}

module Model.Record where

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
