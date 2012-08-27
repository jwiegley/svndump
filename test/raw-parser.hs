module Main where

import qualified Codec.Compression.GZip as GZip
import           Control.Applicative
import qualified Data.ByteString.Lazy as B
import           Subversion.Dump

main :: IO ()
main = do
  file <- B.readFile "data/cunit.dump.gz"
  dump <- readSvnDumpRaw $ GZip.decompress file
  print $ length <$> dump
