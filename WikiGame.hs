{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module WikiGame where

import Parser

import qualified Data.ByteString as BS
import Data.Text (Text)
import Data.Text.Encoding
import Text.Regex.Base.RegexLike
import Text.Regex.TDFA ((=~))

extractLinks :: Text -> [] BS.ByteString
extractLinks page =
    map (\x -> BS.drop 2 $ BS.take (BS.length x - 2) x) link
    where link = getAllTextMatches $ page_bs =~ pattern
          page_bs = encodeUtf8 page :: BS.ByteString
          pattern = "\\[\\[[^]]*]]" :: BS.ByteString
