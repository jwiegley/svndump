{-# LANGUAGE OverloadedStrings #-}

module Subversion.Dump.Raw
       ( FieldMap
       , Entry(..)

       , readInt
       , readSvnDumpRaw
       ) where

import           Control.Applicative hiding (many)
import           Control.Monad
import qualified Data.Attoparsec.Char8 as AC
import           Data.Attoparsec.Combinator
import           Data.Attoparsec.Lazy as AL
import           Data.ByteString as B hiding (map)
import qualified Data.ByteString.Lazy as BL hiding (map)
import qualified Data.List as L
import           Data.Maybe
import           Data.Word (Word8)

import           Prelude hiding (getContents)

default (ByteString)

type FieldMap = [(ByteString, ByteString)]

data Entry = Entry { entryTags  :: FieldMap
                   , entryProps :: FieldMap
                   , entryBody  :: BL.ByteString }
           deriving Show

readSvnDumpRaw :: BL.ByteString -> [Entry]
readSvnDumpRaw dump =
  case parse parseHeader dump of
    Fail {}         -> error "Stream is not a Subversion dump file"
    Done contents _ -> parseDumpFile contents

parseDumpFile :: BL.ByteString -> [Entry]
parseDumpFile contents =
  case parse parseEntry contents of
    Fail {} -> []
    Done contents' (entry, bodyLen) ->
        entry { entryBody = BL.take (fromIntegral bodyLen) contents' }
      : parseDumpFile (BL.drop (fromIntegral bodyLen) contents')

-- These are the Parsec parsers for the various parts of the input file.

space :: Parser Word8
space = word8 32

newline :: Parser Word8
newline = word8 10

parseTag :: Parser (ByteString, ByteString)
parseTag =
  (,) <$> takeWhile1 fieldChar <* string ": " -- :
      <*> takeWhile1 (/= 10) <* newline
  where fieldChar w =   (w >= 65 && w <= 90)  -- A-Z
                      || (w >= 97 && w <= 121) -- a-z
                      || (w >= 48 && w <= 57)  -- 0-9
                      || w == 45            -- -
                      || w == 95            -- _

parseIndicator :: Parser (Word8, Int)
parseIndicator = (,) <$> satisfy (oneOf 75 86) <* space -- K or V
                     <*> AC.decimal <* newline
  where oneOf x y w = w == x || w == y

parseSpecValue :: Parser ByteString
parseSpecValue = do
  (_, len) <- parseIndicator
  AL.take len <* newline

parseProperty :: Parser (ByteString, ByteString)
parseProperty = (,) <$> parseSpecValue -- K
                    <*> parseSpecValue -- V

parseEntry :: Parser (Entry, Int)
parseEntry = do
  fields  <- AL.takeWhile (== 10) *> many1 parseTag <* newline
  props   <- case L.lookup "Prop-content-length" fields of
               Nothing -> return []
               Just _  -> manyTill parseProperty (try (string "PROPS-END\n"))

  -- Don't read the body here in AttoParsec, let the caller extract it from
  -- the ByteString (which might be lazy, saving us from strictifying it in
  -- AttoParsec).
  let bodyLen = fromMaybe 0 (readInt <$> L.lookup "Text-content-length" fields)

  return ( Entry { entryTags  = fields
                 , entryProps = props
                 , entryBody  = BL.empty }
         , bodyLen )

parseHeader :: Parser ByteString
parseHeader = do
  _ <- string "SVN-fs-dump-format-version: 2\n\n"
       <?> "Dump file starts without a recognizable tag"
  string "UUID: " *> takeWhile1 uuidMember
    <* newline <* newline
  where
    -- Accept any hexadecimal character, or '-'
    uuidMember w = w == 45 || (w >= 48 && w <= 57) || (w >= 97 && w <= 102)

-- | Efficiently convert a ByteString of integers into an Int.
--
--   >>> readInt (Data.ByteString.Char8.pack "12345")
--   12345

readInt :: ByteString -> Int
readInt = B.foldl' addup 0
  where addup acc x = acc * 10 + (fromIntegral x - 48) -- '0'

-- SvnDump.hs ends here
