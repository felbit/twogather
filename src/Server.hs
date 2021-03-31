{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Server where

import           Control.Monad.Except
import           Data.Maybe
import           Data.Text
import           Data.Time
import           Lucid
import           Network.HTTP.Media   ((//), (/:))
import           Servant

import           Dashboard
import           Database             (records)
import           Model.Record

-- Serving HTML with Lucid
data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance ToHtml a => MimeRender HTML a where
  mimeRender _ = renderBS . toHtml

instance MimeRender HTML (Html a) where
  mimeRender _ = renderBS

-- Defining the API
type API = Get '[ HTML] Dashboard 
      :<|> ReqBody '[ FormUrlEncoded] RecordForm :> Post '[ HTML] Dashboard

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
          (Record
             (ruser record')
             (ramount record')
             (rcomment record')
             currentDay :
           records)

app :: Application
app = serve (Proxy :: Proxy API) server
