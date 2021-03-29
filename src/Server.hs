{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Server where

import           Control.Monad.Except
import           Data.Text
import           Lucid
import           Network.HTTP.Media   ((//), (/:))
import           Servant

import           Dashboard
import           Data.Maybe
import           Data.Time
import           Model                (TgRecord (TgRecord, comment),
                                       TgRecord' (amount', comment', user'),
                                       records)

-- Serving HTML with Lucid
data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance ToHtml a => MimeRender HTML a where
  mimeRender _ = renderBS . toHtml

instance MimeRender HTML (Html a) where
  mimeRender _ = renderBS

-- Defining the API
type GetDashboard = Get '[ HTML] Dashboard

type PostTgRecord
   = ReqBody '[ FormUrlEncoded] RecordForm :> Post '[ HTML] Dashboard

type API = GetDashboard :<|> PostTgRecord

server :: Server API
server = dashboardH :<|> recordH
  where
    dashboardH :: Handler Dashboard
    dashboardH = return dashboard
    recordH :: RecordForm -> Handler Dashboard
    recordH record' = do
      currentDay <- liftIO $ utctDay <$> getCurrentTime
      return $
        Dashboard
          (TgRecord
             (ramount record')
             (rcomment record')
             currentDay
             (ruser record') :
           records)

app :: Application
app = serve (Proxy :: Proxy API) server
