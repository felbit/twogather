module Main where

import           Network.Wai.Handler.Warp

import           Server

main :: IO ()
main = run 4000 app
