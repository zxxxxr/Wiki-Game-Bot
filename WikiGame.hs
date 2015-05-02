{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module WikiGame where

import Parser
import Links
import Graphs

import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString.Char8 as C (pack)
import Data.Graph.Inductive.NodeMap
import Data.Graph.Inductive.PatriciaTree
import Data.Map.Strict (Map)
import System.Environment (getArgs)

loop :: Map ByteString [ByteString] -> IO ()
loop graph = do
    putStrLn "Please enter source article:"
    from <- getLine
    unless (from == "q") $ do
        putStrLn "Please enter target article"
        to <- getLine
        putStrLn "Calculating.."
        case getMapShortestPath (C.pack from) (C.pack to) graph of
            Nothing -> putStrLn "No path is found."
            Just ls -> print ls
        loop graph

main :: IO ()
main = do
    args <- getArgs
    case args of
        [file] -> do
            pages <- getPages file
            let pairs = parseLinks pages
            let graph =  buildMap pairs
            putStrLn "Welcome to Wiki Game. Enter \'q\' to quit"
            loop graph

        _      -> putStrLn "Please enter only 1 file"
