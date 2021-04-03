{-# LANGUAGE DeriveGeneric #-}

module Dashboard where

import           Data.ByteString                ( ByteString )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Lucid
-------------------------------------------------------------------------------
import           Model.Record

newtype Dashboard =
  Dashboard
    { transactionRecords :: [Record]
    }
  deriving (Eq, Show)

instance ToHtml Dashboard where
  toHtml db = do
    doctype_
    html_ $ do
      head_ $ link_
        [ href_
          "https://cdn.jsdelivr.net/npm/bootstrap@5.0.0-beta3/dist/css/bootstrap.min.css"
        , rel_ "stylesheet"
        ]
      body_ $ do
        with nav_ [class_ "navbar fixed-top navbar-light bg-light"]
          $ with div_ [class_ "container-fluid"]
          $ h1_ "two·gather"
        with div_ [class_ "container", style_ "padding-top:100px;"] $ do
          with form_ [method_ "post"] $ with div_ [class_ "row mb-3"] $ do
            with div_ [class_ "col-md"] $ input_
              [ type_ "text"
              , class_ "form-control"
              , placeholder_ "230"
              , name_ "ramount"
              , required_ "true"
              ]
            with div_ [class_ "col-md-5"] $ input_
              [ type_ "text"
              , class_ "form-control"
              , placeholder_ "I <3 Pizza!"
              , name_ "rcomment"
              , required_ "true"
              ]
            with div_ [class_ "col-md-2"] $ input_
              [ type_ "text"
              , class_ "form-control"
              , placeholder_ "Martin"
              , name_ "ruser"
              , required_ "true"
              ]
            with div_ [class_ "col-md"]
              $ with button_ [type_ "submit", class_ "btn btn-primary"] "Save"
          with div_ [class_ "row mb-3"] $ toHtml (transactionRecords db)
  -- don't give me compiler warnings
  toHtmlRaw = toHtml
