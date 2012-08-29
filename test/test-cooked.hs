module Main where

import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as B
import           Subversion.Dump
import           System.Exit

main :: IO ()
main = do
  file <- B.readFile "data/cunit.dump.gz"
  let len = length $ readSvnDump (GZip.decompress file)
  putStrLn $ show len ++ " cooked entries found, expecting 157"
  if len == 157
    then exitSuccess
    else exitFailure

-- test-cooked.hs ends here
