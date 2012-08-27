{-# LANGUAGE OverloadedStrings #-}

module Subversion.Dump
       ( RevDate
       , Revision(..)

       , OpKind(..)
       , OpAction(..)
       , Operation(..)

       , FieldMap
       , Entry(..)

       , readSvnDumpRaw
       , readSvnDump
       ) where

{-| This is a parser for Subversion dump files.  The objective is to convert a
dump file into a series of data structures representing that same information.
It uses `Data.ByteString.Lazy` to reading the file, and `Data.Text` to
represent text fields which may contain Unicode characters. -}

--import Debug.Trace
import           Control.Applicative hiding (many, (<|>))
import           Control.Monad
import qualified Data.ByteString.Lazy as B
--import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.List as L
--import qualified Data.Map as M
import           Data.Maybe
import           Data.Text.Lazy hiding (map, count)
import           Data.Text.Lazy.Encoding as E
import           System.FilePath
import           Text.Parsec
import           Text.Parsec.ByteString.Lazy as PB

import           Prelude hiding (getContents)

default (Data.Text.Lazy.Text)

{- At the topmost level, a dump file is simple an in-order, linear list of
revisions, where each revisions consist of a series of "operation nodes" that
represent the changes made by that revision to the repository.  The author
name and revision comment are decoded from UTF8. -}

type RevDate = Text

data Revision = Revision { revNumber     :: Int
                         , revDate       :: RevDate
                         , revAuthor     :: Maybe Text
                         , revComment    :: Maybe Text
                         , revOperations :: [Operation] }
              deriving Show

{- Each node reflects the changes to a single file.  Note that branches don't
need to be considered separately, since in Subversion, all files are stored
within a single filesystem.  Branches are something the user applies "after
the fact" by using specially named paths, such as "foo/branches".  The file's
contents are not decoded, as we have no way of knowing what the intended
encoding should be -- or even if there is in, in the case of binary files.

`opContentLength` is provided as a separate member to avoid reading in the
full contents of the operation solely to determine its length.  This way, you
can inspect the length while deferring the content read if you don't need
it. -}

data OpKind   = File | Directory deriving (Show, Enum, Eq)
data OpAction = Add | Change | Replace | Delete deriving (Show, Enum, Eq)

data Operation = Operation { opKind          :: OpKind
                           , opAction        :: OpAction
                           , opPathname      :: FilePath
                           , opContents      :: B.ByteString
                           , opContentLength :: Int
                           , opChecksumMD5   :: Maybe String
                           , opChecksumSHA1  :: Maybe String
                           , opCopyFromRev   :: Maybe Int
                           , opCopyFromPath  :: Maybe FilePath }
               deriving Show

{- A further note is needed on `opCopyFromRev` and `opCopyFromPath`, since these
two represent the only real complexity in a dump file.  Basically what they
say is that there is no `opContents` record for this `Operation`.  Rather, the
contents to be taken from another file in a past revision.  Since this
historical information would be expensive to maintain, `Operation` only
provides the data given by the dump file, and it is left as an analytical pass
on this data to build the structures necessary to figure out what those
contents would have been.

So, with our structures defined, we're ready to read in the file.  Since we
don't know what each element will be yet (revisions are interspersed with
nodes), we read them first into the much more general Node structure. -}

{-| Reads a dump file from a ByteString in the IO monad into a list of
    Revision values.  This is the "cooked" parallel of `readSvnDumpRaw`. -}
readSvnDump :: B.ByteString -> IO (Either ParseError [Revision])
readSvnDump io = do
  result <- readSvnDumpRaw io
  return $ map processRevs <$> (L.groupBy sameRev <$> result)

  where sameRev _ y     = isNothing $
                          L.lookup "Revision-number" (entryTags y)
        getField f n x  = L.lookup n (f x)
        getField' f n x = fromMaybe "" (getField f n x)
        tagM            = getField entryTags
        propM           = getField entryProps
        tag             = getField' entryTags
        prop            = getField' entryProps

        processRevs [] = error "Unexpected"
        processRevs (rev:ops) =
          Revision {
              revNumber     = read $ tag "Revision-number" rev
            , revDate       = parseDate $ prop "svn:date" rev
            , revAuthor     = propM "svn:author" rev
            , revComment    = propM "svn:log" rev
            , revOperations = map processOp ops }

        processOp op =
          Operation {
              opKind          = getOpKind $ tag "Node-kind" op
            , opAction        = getOpAction $ tag "Node-action" op
            , opPathname      = tag "Node-path" op
            , opContents      = entryBody op
            , opContentLength = read $ tag "Text-content-length" op
            , opCopyFromRev   = read <$>
                                tagM "Node-copyfrom-rev" op
            , opCopyFromPath  = tagM "Node-copyfrom-path" op
            , opChecksumMD5   = tagM "Text-content-md5" op
            , opChecksumSHA1  = tagM "Text-content-sha1" op }

        getOpKind kind = case kind of
          "file" -> File
          "dir"  -> Directory
          _      -> error "Unexpected"

        getOpAction kind = case kind of
          "add"     -> Add
          "delete"  -> Delete
          "change"  -> Change
          "replace" -> Replace
          _      -> error "Unexpected"

type FieldMap a = [(String, a)]

data Entry = Entry { entryTags  :: FieldMap String
                   , entryProps :: FieldMap Text
                   , entryBody  :: B.ByteString }
             deriving Show

readSvnDumpRaw :: B.ByteString -> IO (Either ParseError [Entry])
readSvnDumpRaw dump = return $ parse parseSvnDump "" dump

{- These are the Parsec parsers for the various parts of the input file. -}

parseTag :: PB.Parser (String, String)
parseTag = (,) <$> fieldKey   <* char ':' <* space
               <*> fieldValue <* newline
  where
    fieldKey   = (:) <$> letter <*> many fieldChar
    fieldChar  = letter <|> digit <|> oneOf "-_"
    fieldValue = many1 (noneOf "\n")

parseIndicator :: PB.Parser (Char, Integer)
parseIndicator = (,) <$> oneOf "KV" <* space
                     <*> (read <$> many1 digit <* newline)

readTextRange :: Integer -> PB.Parser B.ByteString
readTextRange len = do
  input <- getInput
  let value = B.take (fromIntegral len) input
  setInput $ B.drop (fromIntegral len) input
  return value

--readTextRange' :: Integer -> PB.Parser B.ByteString
--readTextRange' len = BC.pack <$> count (fromIntegral len) anyChar

parseSpecValue :: Char -> PB.Parser Text
parseSpecValue expected = do
  (kind, len) <- parseIndicator
  when (kind /= expected) $ unexpected "Unexpected spec value char"
  value <- readTextRange len
  --trace ("Value: " ++ (show value)) $ return ()
  _ <- newline
  return $ E.decodeUtf8 value

parseProperty :: PB.Parser (String, Text)
parseProperty = (,) <$> (unpack <$> parseSpecValue 'K')
                    <*> parseSpecValue 'V'

parseEntry :: PB.Parser Entry
parseEntry = do
  fields <- many1 parseTag <* newline

  props  <- case L.lookup "Prop-content-length" fields of
              Nothing -> return []
              Just _  -> many parseProperty <* string "PROPS-END\n"

  body   <- case L.lookup "Text-content-length" fields of
              Nothing  -> return B.empty
              Just len -> readTextRange (read len)

  _ <- many newline <?> "entry-terminating newline"

  return Entry { entryTags  = fields
               , entryProps = props
               , entryBody  = body }

parseHeader :: PB.Parser ()
parseHeader = do
  _ <- string "SVN-fs-dump-format-version: 2\n\n"
       <?> "Dump file starts without a recognizable tag"
  _ <- string "UUID: " <* many1 (hexDigit <|> char '-')
       <* newline <* newline
  return ()

parseSvnDump :: PB.Parser [Entry]
parseSvnDump = parseHeader >> many parseEntry

parseDate :: Text -> RevDate
parseDate = id

-- SvnDump.hs ends here
