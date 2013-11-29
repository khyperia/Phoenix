{-# LANGUAGE TemplateHaskell #-}
module PhoenixPrelude where

import Data.FileEmbed
import Data.ByteString.Char8

prelude :: String
prelude = unpack $(embedFile "prelude.txt")

