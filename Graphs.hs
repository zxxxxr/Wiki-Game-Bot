{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Graphs where

import Data.ByteString (ByteString)
import Data.ByteString as BS (append)
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.NodeMap
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.BFS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M


getLEgStr :: ByteString -> ByteString -> ByteString
getLEgStr from = BS.append from . BS.append " -> "


buildLEdgeList :: [(ByteString, [ByteString])]
                -> [(ByteString, ByteString, ByteString)]
buildLEdgeList pages = concatMap makeEdges pages
    where alltitles = fst $ unzip pages :: [ByteString]
          makeEdges (title, links) = map (\l -> (title, l, getLEgStr title l)) $
                                     filter (`elem` alltitles) links

buildMap :: [(ByteString, [ByteString])] -> Map ByteString [ByteString]
buildMap pages = foldr insertEge M.empty pages
    where alltitles = fst $ unzip pages
          insertEge (tit, lnks) = M.insert tit (filter (`elem` alltitles) lnks)

buildGraph :: [(ByteString, [ByteString])]
                -> (Gr ByteString ByteString, NodeMap ByteString)
buildGraph pages = mkMapGraph (fst $ unzip pages)
                          $ buildLEdgeList pages


getShortestPath :: ByteString
                    -> ByteString
                    -> (Gr ByteString ByteString, NodeMap ByteString)
                    -> Maybe [ByteString]
getShortestPath from to g = case path of
    LP [] -> Nothing
    LP ls -> Just $ tail $ map snd ls -- sequence $ map (lab $ fst g) ls
    where path  = lesp fromN toN $ fst g :: LPath ByteString
          fromN = fst $ mkNode_ (snd g) from
          toN   = fst $ mkNode_ (snd g) to

mbfs :: ByteString -> ByteString -> Map ByteString [ByteString] -> [ByteString]
mbfs startK endK m =
    let (startVM, endVM) = (M.lookup startK m, M.lookup endK m) in
    case (startVM, endVM) of
        (Nothing, _)             -> []
        (_, Nothing)             -> []
        (_, _) -> traverse [[startK]] []

        where traverse [] _         = []
              traverse (hd:tl) seen = let node = last hd in
                  let Just children = M.lookup node m in
                  if node == endK then hd
                  else traverse (newq children) (newseen children)
                  where newq = foldr (\bs q -> if bs `elem` seen
                                                        then q
                                                        else q ++ [hd ++ [bs]])
                                               tl
                        newseen = foldr (\bs s -> if bs `elem` seen
                                                           then s
                                                           else bs : s)
                                               seen


getMapShortestPath :: ByteString
                        -> ByteString
                        -> Map ByteString [ByteString]
                        -> Maybe [ByteString]
getMapShortestPath from to m = case path of
    [] -> Nothing
    ls -> Just ls
    where path = mbfs from to m
