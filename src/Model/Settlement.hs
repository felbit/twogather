{-|
Module      : Model.Settlement
Description : Settling differences in spending month by month
Stability   : experimental

twogather works periodically in calendar months. Every month the balance
between both parties is checked and a settlement is created. A settlement
is a task to transfer monay from one party to the other dependend on the
amounts spent during that month.

E.g. Alice has spent 234.80 and Bob has spent 421.42 of the same currency.
The average amount spent that month is (234.80 + 421.42) / 2 = __328.11__.
Since Alice spent less than Bob she has to transfer 93.31 to Bob to settle
the month.
-}
module Model.Settlement where

data SettleState
  = OUTSTANDING -- ^ Todo
  | TRANSACTED -- ^ Money sent, settlement initiated
  | SETTLED -- ^ Done, settlement cleared

data Task =
  Task
    { state :: SettleState
    , year  :: Integer
    , month :: Int
    }
