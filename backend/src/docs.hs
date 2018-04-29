{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
import           Data.Text (Text)
import           Data.Time (buildTime, defaultTimeLocale)
import           Servant (Capture, QueryParam, QueryParams)
import           Servant.Docs

import Web.TED (FileType)
import qualified Web.TED as API
import Handler.Util
import ReTed.API (tedApi)

instance ToSample [Char] [Char] where
    toSample _ = Just "abc"

instance ToSample Text Text where
    toSample _ = Just "abc"

instance ToSample RedisTalk RedisTalk where
    toSample _ = Just $
        RedisTalk { Handler.Util.id = 1
                  , name = "1"
                  , description = "1"
                  , slug = "1"
                  , images = API.Image "1" "1"
                  , publishedAt = buildTime defaultTimeLocale [('0', "0")]
                  , mSlug = "1"
                  , mPad = 1
                  }
instance ToSample [RedisTalk] [RedisTalk] where
    toSample _ = Just $
        [RedisTalk { Handler.Util.id = 1
                  , name = "1"
                  , description = "1"
                  , slug = "1"
                  , images = API.Image "1" "1"
                  , publishedAt = buildTime defaultTimeLocale [('0', "0")]
                  , mSlug = "1"
                  , mPad = 1
                  }]

instance ToSample TalkResp TalkResp where
    toSample _ = Just $
        TalkResp
            (RedisTalk { Handler.Util.id = 1
                      , name = "1"
                      , description = "1"
                      , slug = "1"
                      , images = API.Image "1" "1"
                      , publishedAt = buildTime defaultTimeLocale [('0', "0")]
                      , mSlug = "1"
                      , mPad = 1
                      }) $ Just [(API.Language "English" "en")]

instance ToParam (QueryParam "limit" Integer) where
    toParam _ =
        DocQueryParam "limit"
                      ["5", "10"]
                      "maximum number of return items"
                      Normal

instance ToParam (QueryParam "tid" Integer) where
    toParam _ =
        DocQueryParam "tid"
                      ["1194", "2015"]
                      "TED talk id"
                      Normal

instance ToCapture (Capture "tid" Int) where
    toCapture _ =
        DocCapture "tid" "TED talk id"

instance ToCapture (Capture "slug" Text) where
    toCapture _ =
        DocCapture "slug" "slug in talk url"

instance ToCapture (Capture "format" FileType) where
    toCapture _ =
        DocCapture "format" "format of transcripts"

instance ToParam (QueryParams "lang" Text) where
    toParam _ =
        DocQueryParam "lang"
                      ["en", "zh-cn"]
                      "language code"
                      List

instance ToParam (QueryParam "q" Text) where
    toParam _ =
        DocQueryParam "q"
                      ["google", "design"]
                      "keywords to search"
                      Normal

reTedDocs :: API
reTedDocs = docs tedApi

main :: IO ()
main = putStrLn $ markdown reTedDocs
