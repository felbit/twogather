module Dashboard where

import           Data.ByteString (ByteString)
import           Lucid
import qualified Model           as M

data Dashboard =
  Dashboard
    { records :: [M.TgRecord]
    }
  deriving (Eq, Show)

instance ToHtml Dashboard where
  toHtml db = do
    doctype_
    html_ $ do
      head_ $
        link_
          [ href_
              "https://cdn.jsdelivr.net/npm/bootstrap@5.0.0-beta3/dist/css/bootstrap.min.css"
          , rel_ "stylesheet"
          ]
      body_ $ do
        with nav_ [class_ "navbar fixed-top navbar-light bg-light"] $
          with div_ [class_ "container-fluid"] $ h1_ "two·gather"
        with div_ [class_ "container", style_ "padding-top:100px;"] $ do
          with form_ [method_ "post"] $
            with div_ [class_ "row mb-3"] $ do
              with div_ [class_ "col-md"] $
                input_
                  [ type_ "text"
                  , class_ "form-control"
                  , placeholder_ "23.00"
                  , name_ "amount"
                  , id_ "amount"
                  , required_ "true"
                  ]
              with div_ [class_ "col-md-7"] $
                input_
                  [ type_ "text"
                  , class_ "form-control"
                  , placeholder_ "I <3 Pizza!"
                  , name_ "comment"
                  , id_ "comment"
                  , required_ "true"
                  ]
              with div_ [class_ "col-md-7"] $
                input_
                  [ type_ "text"
                  , class_ "form-control"
                  , placeholder_ "I <3 Pizza!"
                  , name_ "user"
                  , id_ "user"
                  , required_ "true"
                  ]
              with div_ [class_ "col-md"] $
                with button_ [type_ "submit", class_ "btn btn-primary"] "Save"
          with div_ [class_ "row mb-3"] $
            with table_ [class_ "table table-striped"] $ do
              thead_ $
                tr_ $ do
                  th_ "When?"
                  th_ "For what?"
                  th_ "How much?"
                  th_ "Who?"
              tbody_ $ mapM_ toHtml (records db)
  -- don't give me compiler warnings
  toHtmlRaw = toHtml

dashboard :: Dashboard
dashboard = Dashboard {records = M.records}
