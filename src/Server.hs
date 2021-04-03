{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Server where

import           Control.Monad.Except
import           Control.Monad.Logger (runStderrLoggingT)
import           Data.Maybe
import           Data.String.Conversions (cs)
import           Data.Text
import           Data.Time
import           Database.Persist.Sqlite
import           Lucid
import           Network.HTTP.Media   ((//), (/:))
import           Network.Wai.Handler.Warp as Warp
import           Servant
-------------------------------------------------------------------------------
import           Dashboard
import           Model.Record

-- Serving HTML with Lucid
data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance ToHtml a => MimeRender HTML a where
  mimeRender _ = renderBS . toHtml

instance MimeRender HTML (Html a) where
  mimeRender _ = renderBS

--------- API
type API = Get '[ HTML] Dashboard 
      :<|> ReqBody '[ FormUrlEncoded] RecordForm :> PostCreated '[HTML] Dashboard

--------- Serving Handlers
server :: ConnectionPool -> Server API
server pool = dashboardH 
         :<|> recordTransactionH
  where
    recordTransactionH :: RecordForm -> Handler Dashboard
    recordTransactionH recordForm = do 
      liftIO $
        flip runSqlPersistMPool pool $ do
          currentDay <- liftIO $ utctDay <$> getCurrentTime
          let newRecord = Record (ruser recordForm) (ramount recordForm) (rcomment recordForm) currentDay
            in insert newRecord
      records <- liftIO $ 
        flip runSqlPersistMPool pool $ do
          recordVals <- selectList [] []
          return $ entityVal <$> recordVals
      return $ Dashboard records

    dashboardH :: Handler Dashboard
    dashboardH = do
      records <- liftIO $ 
        flip runSqlPersistMPool pool $ do
          recordVals <- selectList [RecordUsername ==. "Martin"] []
          return $ entityVal <$> recordVals
      return $ Dashboard records

--------- APP
app :: ConnectionPool -> Application
app pool = serve (Proxy :: Proxy API) $ server pool

mkApp :: FilePath -> IO Application
mkApp sqliteFile = do
  pool <- runStderrLoggingT $ do
    createSqlitePool (cs sqliteFile) 5
  runSqlPool (runMigration migrateAll) pool
  return $ app pool

run :: FilePath -> IO ()
run sqliteFile = Warp.run 4000 =<< mkApp sqliteFile
