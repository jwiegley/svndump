module Main where

import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as B
import           Subversion.Dump
import           System.Exit

main :: IO ()
main = do
  file <- B.readFile "data/cunit.dump.gz"
  dump <- readSvnDump $ GZip.decompress file
  case dump of
    Left _   -> exitFailure
    Right xs -> do
      let len = length xs
      putStrLn $ show len ++ " cooked entries found, expecting 157"
      if len == 157
        then exitSuccess
        else exitFailure

-- raw-parser.hs ends here
