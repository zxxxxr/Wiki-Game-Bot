{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Graphs where

import Data.ByteString (ByteString)
import Data.ByteString as BS (append)
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.NodeMap
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.BFS


getLEgStr :: ByteString -> ByteString -> ByteString
getLEgStr from = BS.append from . BS.append " -> "


buildLEdgeList :: [(ByteString, [ByteString])]
                -> [(ByteString, ByteString, ByteString)]
buildLEdgeList pages = concatMap makeEdges pages
    where alltitles = fst $ unzip pages :: [ByteString]
          makeEdges (title, links) = map (\l -> (title, l, getLEgStr title l)) $
                                     filter (`elem` alltitles) links


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
    where path = lesp fromN toN $ fst g :: LPath ByteString
          fromN = fst $ mkNode_ (snd g) from
          toN   = fst $ mkNode_ (snd g) to
