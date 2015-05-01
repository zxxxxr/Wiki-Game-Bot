{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Graphs where

import Data.ByteString (ByteString)
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.NodeMap
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.BFS


buildLEdgeList :: [(ByteString, [ByteString])] -> [(ByteString, ByteString, ())]
buildLEdgeList pages = concatMap makeEdges pages
    where alltitles = fst $ unzip pages :: [ByteString]
          makeEdges (title, links) = map (\l -> (title, l, ())) $
                                     filter (`elem` alltitles) links


buildGraph :: [(ByteString, [ByteString])]
                -> (Gr ByteString (), NodeMap ByteString)
buildGraph pages = mkMapGraph (fst $ unzip pages)
                          $ buildLEdgeList pages

getShortestPath :: ByteString
                    -> ByteString
                    -> (Gr ByteString (), NodeMap ByteString)
                    -> Maybe [ByteString]
getShortestPath from to g = case path of
    [] -> Nothing
    ls -> sequence $ map (lab $ fst g) ls
    where path = esp fromN toN $ fst g
          fromN = fst $ mkNode_ (snd g) from
          toN   = fst $ mkNode_ (snd g) to
