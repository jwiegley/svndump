module Main where

import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as B
import           Subversion.Dump
import           System.Exit

main :: IO ()
main = do
  file <- B.readFile "data/cunit.dump.gz"
  dump <- readSvnDumpRaw $ GZip.decompress file
  case dump of
    Left _   -> exitFailure
    Right xs -> do
      let len = length xs
      putStrLn $ show len ++ " raw entries found, expecting 1950"
      if len == 1950
        then exitSuccess
        else exitFailure

-- test-raw.hs ends here
