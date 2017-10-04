{-# LANGUAGE OverloadedStrings #-}

module Main where

import WTB.Config

main :: IO ()
main = do
  cfg <- read_config "cfg/config.json"
  case cfg of
    Right config ->
      do
        print config        
        return ()
    Left error ->
      do      
        putStrLn "Error parsing configuration:"
        putStrLn error
  
