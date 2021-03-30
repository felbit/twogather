module Database where

import           Data.Time.Calendar

import           Model.Record

-- Database
records :: [TgRecord]
records =
  [ TgRecord 134 "ICA" (fromGregorian 2021 3 26) "Martin"
  , TgRecord 231 "MAX Burgers" (fromGregorian 2021 3 25) "Martin"
  , TgRecord 59 "Helgkasse Godisbanken" (fromGregorian 2021 3 18) "Paula"
  , TgRecord 29 "Karma" (fromGregorian 2021 3 17) "Martin"
  , TgRecord 129 "Willy:s" (fromGregorian 2021 3 15) "Paula"
  ]
