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
import System.Environment (getArgs)

loop :: (Gr ByteString ByteString, NodeMap ByteString) -> IO ()
loop graph = do
    putStrLn "Please enter source and destination articles on 1 line:"
    line <- getLine
    unless (line == "q") $ do
        let from:to:_ = words line -- TODO: can not only split on space
        case getShortestPath (C.pack from) (C.pack to) graph of
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
            let graph =  buildGraph pairs
            putStrLn "Welcome to Wiki Game. Enter \'q\' to quit"
            loop graph

        _      -> putStrLn "Please enter only 1 file"
