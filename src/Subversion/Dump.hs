{-# LANGUAGE OverloadedStrings #-}

module Subversion.Dump
       ( RevDate
       , Revision(..)

       , OpKind(..)
       , OpAction(..)
       , Operation(..)

       , readSvnDump
       ) where

import           Control.Applicative hiding (many)
import           Data.ByteString as B hiding (map)
import qualified Data.ByteString.Char8 as BC hiding (map)
import qualified Data.ByteString.Lazy as BL hiding (map)
import qualified Data.List as L
import           Data.Text (Text)
import qualified Data.Text.Encoding as E
import           Data.Maybe

import           Subversion.Dump.Raw

import           Prelude hiding (getContents)

default (ByteString)

-- | A parser for Subversion dump files.  The objective is to convert a dump
--   file into a series of data structures representing that same information.
--   It uses 'Data.ByteString.Lazy' to reading the file, and 'Data.Text' to
--   represent text fields which may contain Unicode characters.

-- At the topmost level, a dump file is simple an in-order, linear list of
-- revisions, where each revisions consist of a series of "operation nodes"
-- that represent the changes made by that revision to the repository.  The
-- author name and revision comment are decoded from UTF8.

type RevDate = Text

data Revision = Revision { revNumber     :: Int
                         , revDate       :: RevDate
                         , revAuthor     :: Maybe Text
                         , revComment    :: Maybe Text
                         , revOperations :: [Operation] }
              deriving Show

-- Each node reflects the changes to a single file.  Note that branches don't
-- need to be considered separately, since in Subversion, all files are stored
-- within a single filesystem.  Branches are something the user applies "after
-- the fact" by using specially named paths, such as "foo/branches".  The
-- file's contents are not decoded, as we have no way of knowing what the
-- intended encoding should be -- or even if there is in, in the case of
-- binary files.
--
-- 'opContentLength' is provided as a separate member to avoid reading in the
-- full contents of the operation solely to determine its length.  This way,
-- you can inspect the length while deferring the content read if you don't
-- need it.

data OpKind   = File | Directory deriving (Show, Enum, Eq)
data OpAction = Add | Change | Replace | Delete deriving (Show, Enum, Eq)

data Operation = Operation { opKind          :: OpKind
                           , opAction        :: OpAction
                           , opPathname      :: FilePath
                           , opContents      :: ByteString
                           , opContentLength :: Int
                           , opChecksumMD5   :: Maybe Text
                           , opChecksumSHA1  :: Maybe Text
                           , opCopyFromRev   :: Maybe Int
                           , opCopyFromPath  :: Maybe FilePath }
               deriving Show

-- A further note is needed on 'opCopyFromRev' and 'opCopyFromPath', since
-- these two represent the only real complexity in a dump file.  Basically
-- what they say is that there is no 'opContents' record for this 'Operation'.
-- Rather, the contents to be taken from another file in a past revision.
-- Since this historical information would be expensive to maintain,
-- 'Operation' only provides the data given by the dump file, and it is left
-- as an analytical pass on this data to build the structures necessary to
-- figure out what those contents would have been.
--
-- So, with our structures defined, we're ready to read in the file.  Since we
-- don't know what each element will be yet (revisions are interspersed with
-- nodes), we read them first into the much more general Node structure.

-- | Reads a dump file from a ByteString in the IO monad into a list of
--   Revision values.  This is the "cooked" parallel of 'readSvnDumpRaw'.

readSvnDump :: BL.ByteString -> [Revision]
readSvnDump dump = map processRevs $ L.groupBy sameRev $ readSvnDumpRaw dump
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
              revNumber     = readInt $ tag "Revision-number" rev
            , revDate       = parseDate $ prop "svn:date" rev
            , revAuthor     = E.decodeUtf8 <$> propM "svn:author" rev
            , revComment    = E.decodeUtf8 <$> propM "svn:log" rev
            , revOperations = map processOp ops }

        processOp op =
          Operation {
              opKind          = getOpKind $ tag "Node-kind" op
            , opAction        = getOpAction $ tag "Node-action" op
            , opPathname      = BC.unpack $ tag "Node-path" op
            , opContents      = entryBody op
            , opContentLength = readInt $ tag "Text-content-length" op
            , opCopyFromRev   = readInt <$>
                                tagM "Node-copyfrom-rev" op
            , opCopyFromPath  = BC.unpack <$> tagM "Node-copyfrom-path" op
            , opChecksumMD5   = E.decodeUtf8 <$> tagM "Text-content-md5" op
            , opChecksumSHA1  = E.decodeUtf8 <$> tagM "Text-content-sha1" op }

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

parseDate :: ByteString -> RevDate
parseDate = E.decodeUtf8

-- Dump.hs ends here
