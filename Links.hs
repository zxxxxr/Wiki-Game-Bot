{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Links where

import Parser

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C (split, head, cons)
import Data.Char
import Data.Text as T (Text, isPrefixOf)
import Data.Text.Encoding
import Text.Regex.Base.RegexLike
import Text.Regex.TDFA ((=~))


extractLinks :: Text -> [BS.ByteString]
extractLinks page = getAllTextMatches $ page_bs =~ pattern
    where page_bs = encodeUtf8 page :: BS.ByteString
          pattern = "\\[\\[[^]]+]]" :: BS.ByteString

{- 1. Remove the double brackets wrapping the linked title
   2. Remove all file links
   3. Clean up the title to reflect redirects
-}
pruneLinks :: [BS.ByteString] -> [BS.ByteString]
pruneLinks = map cap . filter ((/=) "") . map trans . filter predi . map trunc
    where trunc x  = BS.drop 2 $ BS.take (BS.length x - 2) x
          predi bs = not $ "File:"  `BS.isPrefixOf` bs
                           || "Image:" `BS.isPrefixOf` bs
                           || "image:" `BS.isPrefixOf` bs
          trans    = (\bs -> if bs == "" then "" else head $ C.split '#' bs)
                    . head . C.split '|'
          cap bs   = toUpper (C.head bs) `C.cons` BS.drop 1 bs

parseLinks :: [Page] -> [(BS.ByteString, [BS.ByteString])]
parseLinks = map makePair . filter filterIrrelevant
    where makePair (Page t p) = (encodeUtf8 t, pruneLinks $ extractLinks p)
          filterIrrelevant (Page t _) = not $ "Template:" `T.isPrefixOf` t
                                         || "Wikipedia:" `T.isPrefixOf` t
                                         || "MediaWiki:" `T.isPrefixOf` t

main :: IO ()
main = do
    pages <- getPages "../data/simple.xml"
    mapM_ printPage $ parseLinks pages
    where printPage (title, page) = do
          print title
          print page

