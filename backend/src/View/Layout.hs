{-# LANGUAGE QuasiQuotes #-}

module View.Layout where

import           Lucid
import           RIO
import           Text.RawString.QQ
import           Types

layout :: Html () -> Html () -> AppM (Html ())
layout headPartial bodyPartial = do
  Config { devMode } <- ask
  pure $ doctype_ *> (html_ [lang_ "en"] $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport"
            ,content_ "width=device-width, initial-scale=1"]
      link_ [title_ "TED2srt"
            ,href_ "/dist/search.xml"
            ,type_ "application/opensearchdescription+xml"
            ,rel_"search"]
      headPartial

    body_ $ do
      bodyPartial
      when (not devMode) $ do
        script_
          [type_ "application/ld+json"] (
          [r|{
            "@context": "http://schema.org",
            "@type": "WebSite",
            "url": "https://ted2srt.org",
            "potentialAction": {
              "@type": "SearchAction",
              "target": "https://ted2srt.org/search?q={search_term_string}",
              "query-input": "required name=search_term_string"
            }
          }|] :: Text)
        script_
          [src_ "https://www.googletagmanager.com/gtag/js?id=UA-109501213-1"]
          ("" :: Text)
        script_
          [r|
            window.dataLayer = window.dataLayer || [];
            function gtag(){dataLayer.push(arguments);}
            gtag('js', new Date());

            gtag('config', 'UA-109501213-1');
            |]
        script_
          [src_ "https://load.fomo.com/ads/load.js?id=XgTyE48BTxPWOg2xANJv3Q"]
          ("" :: Text)
    )
