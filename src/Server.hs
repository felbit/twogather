{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Data.List
import Servant

import Model

type API = Get '[JSON] [TgRecord]
      :<|> ReqBody '[JSON] TgRecord :> Post '[JSON]  [TgRecord]

server :: Server API
server = recordsH :<|> recordH
  where
    recordsH :: Handler [TgRecord]
    recordsH = return records

    recordH :: TgRecord -> Handler [TgRecord]
    recordH r = return $ r : records

app :: Application
app = serve (Proxy :: Proxy API) server
