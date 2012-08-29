module Main where

import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as B
import           Subversion.Dump.Raw
import           System.Exit
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      file <- B.readFile "data/cunit.dump.gz"
      let len = length $ readSvnDumpRaw (GZip.decompress file)
      putStrLn $ show len ++ " raw entries found, expecting 1950"
      if len == 1950
        then exitSuccess
        else exitFailure

    (fileName:_) -> do
      contents   <- B.readFile fileName
      print $ length $ readSvnDumpRaw contents

-- test-raw.hs ends here
