module Main where

import           Database.Redis (connect, defaultConnectInfo)
import           Yesod (warp)

import Foundation (Ted(..))
import Settings (staticSite)

main :: IO ()
main = do
    s <- staticSite
    conn <- connect defaultConnectInfo
    let foundation = Ted s conn
    warp 3000 foundation
