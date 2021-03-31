module Database where

import           Data.Time.Calendar
import           Model.Record

-- Database
records :: [Record]
records =
  [ Record "Martin" 134 "ICA"                   (fromGregorian 2021 3 26)
  , Record "Martin" 231 "MAX Burgers"           (fromGregorian 2021 3 25)
  , Record "Paula"  59  "Helgkasse Godisbanken" (fromGregorian 2021 3 18)
  , Record "Martin" 29  "Karma"                 (fromGregorian 2021 3 17)
  , Record "Paula"  129 "Willy:s"               (fromGregorian 2021 3 15)
  ]
