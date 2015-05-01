{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Parser where

import Control.Monad.Trans.Resource
import Data.Conduit (($$), ConduitM, Consumer)
import Data.Text (Text, unpack)
import Data.XML.Types (Event)
import Text.XML (Name)
import Text.XML.Stream.Parse hiding (parseText)


data Page = Page Text Text
    deriving Show


{- Get text contents of an element. Ignores all attributes -}
getContent :: MonadThrow m => Name -> Consumer Event m (Maybe Text)
getContent name = tagName name ignoreAttrs $ \() -> content


{- The streaming API for xml-conduit doesn't support skipping elements so
   have to manually parse, consume and ignore all irrelevant elements
-}
skipTextElement :: MonadThrow m => Name -> Consumer Event m ()
skipTextElement name = do
    _ <- getContent name
    return ()


{- Skip the element with the given name and its direct children.
   Does NOT work if children have further descendent.
 -}
skipCurrentAndChildren :: MonadThrow m => Name -> Consumer Event m ()
skipCurrentAndChildren name = do
    _ <- tagName name ignoreAttrs $
         \() -> many $ tag
                (\x -> Just ())
                (\() -> ignoreAttrs)
                (\() -> content)
    return ()

skipSiteInfoTag :: MonadThrow m => Consumer Event m ()
skipSiteInfoTag = do
    _ <- tagNoAttr "siteinfo" $ do
        skipTextElement "sitename"
        skipTextElement "dbname"
        skipTextElement "base"
        skipTextElement "generator"
        skipTextElement "case"
        skipCurrentAndChildren "namespaces"
        return ()
    return ()

parseText :: MonadThrow m => Consumer Event m (Maybe Text)
parseText = tagNoAttr "revision" $ do
    skipTextElement "id"
    skipTextElement "parentid"
    skipTextElement "timestamp"
    skipCurrentAndChildren "contributor"
    skipTextElement "minor"
    skipTextElement "comment"
    skipTextElement "model"
    skipTextElement "format"
    text <- force "text is required" $ getContent "text"
    skipTextElement "sha1"
    return text


parsePage :: MonadThrow m => Consumer Event m Page
parsePage = do
    title <- force "title required" $ getContent "title"
    {- Skip irrelevant elements -}
    skipTextElement "ns"
    skipTextElement "id"
    skipTextElement "redirect"
    skipTextElement "restrictions"
    text <- parseText
    case text of
        Nothing   -> return $ Page title "NO TEXT IS RETURNED!!!!"
        Just page -> return $ Page title page


parseWiki :: MonadThrow m => Consumer Event m (Maybe [Page])
parseWiki = tagName "mediawiki" ignoreAttrs $ \() -> do
    skipSiteInfoTag
    many $ tagNoAttr "page" parsePage


getPages file = runResourceT $ parseFile def file $$
    force "wiki required" parseWiki


main = do
    pages <- getPages "../data/simple.xml"
    print pages

