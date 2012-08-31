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

parseHeader :: Parser ByteString
parseHeader =    string "SVN-fs-dump-format-version: 2\n\n"
              *> string "UUID: " *> takeWhile1 uuidMember
              <* newline <* newline
  -- Accept any hexadecimal character or '-'
  where uuidMember w =   w == 45
                       || (w >= 48 && w <= 57)
                       || (w >= 97 && w <= 102)

parseDumpFile :: BL.ByteString -> [Entry]
parseDumpFile contents =
  case parse parseEntry contents of
    --Fail _ _ y -> error y
    Fail {} -> []
    Done contents' (entry, bodyLen) ->
        entry { entryBody = BL.take (fromIntegral bodyLen) contents' }
      : parseDumpFile (BL.drop (fromIntegral bodyLen) contents')

-- Don't read the entry body here in the parser, rather let the caller extract
-- it from the ByteString (which might be lazy, saving us from needlessly
-- strictifying it here).

parseEntry :: Parser (Entry, Int)
parseEntry = do
  fields <- skipWhile (== 10) *> many1 parseTag <* newline
  props  <- case L.lookup "Prop-content-length" fields of
              Nothing -> return []
              Just _  -> manyTill parseProperty (try (string "PROPS-END\n"))
  return ( Entry { entryTags  = fields
                 , entryProps = props
                 , entryBody  = BL.empty }
         , fromMaybe 0 (readInt <$> L.lookup "Text-content-length" fields) )

parseTag :: Parser (ByteString, ByteString)
parseTag = (,) <$> takeWhile1 fieldChar <* string ": "
               <*> takeWhile1 (/= 10) <* newline
  where fieldChar w =   (w >= 97 && w <= 121) -- a-z
                      || (w >= 65 && w <= 90)  -- A-Z
                      || w == 45            -- -
                      || (w >= 48 && w <= 57)  -- 0-9

parseProperty :: Parser (ByteString, ByteString)
parseProperty = (,) <$> (string "K " *> getField <* newline)
                    <*> (string "V " *> getField <* newline)
  -- Read a decimal integer followed by \n and 'take' that many bytes
  where getField = AC.decimal <* newline >>= AL.take

newline :: Parser Word8
newline = word8 10

-- | Efficiently convert a ByteString of integers into an Int.
--
--   >>> readInt (Data.ByteString.Char8.pack "12345")
--   12345

readInt :: ByteString -> Int
readInt = B.foldl' addup 0
  where addup acc x = acc * 10 + (fromIntegral x - 48) -- '0'

-- SvnDump.hs ends here
