{-# LANGUAGE QuasiQuotes #-}

module ReTed.TestUtils
  ( resetDb
  ) where

import           System.Process (callCommand)
import           Text.Printf.TH (s)


resetDb :: IO ()
resetDb = do
    loadFixture "truncate"
    loadFixture "data"

loadFixture :: FilePath -> IO ()
loadFixture = loadSql . [s|test/fixtures/%s.sql|]

loadSql :: FilePath -> IO ()
loadSql name = callCommand $ [s|psql -U $DB_USER -d $DB_NAME -f %s|] name
