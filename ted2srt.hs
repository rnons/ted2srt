module Main where

import           Database.Redis (connect, defaultConnectInfo)
import           Yesod (warp)

import Foundation (Ted(..))
import Settings (staticSite)

main :: IO ()
main = do
    s <- staticSite
    c <- connect defaultConnectInfo
    let foundation = Ted s c
    warp 3000 foundation
