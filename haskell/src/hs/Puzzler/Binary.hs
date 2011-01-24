
-- | Formats for the binary files we use.
module Puzzler.Binary where

import Control.Applicative( (<$>) )
import Control.Monad
import Control.Exception
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Puzzler.Anagram( Dictionary )
import Puzzler.Conf
import Puzzler.Error


-- | `Dictionary' file format.  Wrap a dictionary in this and encode/decode it
-- to write/read a standalone dictionary file.
newtype DictionaryFile = DictFile{ unDictFile :: Dictionary }
instance Binary DictionaryFile where
    put (DictFile d) = do
        putWord64be puzzMagic
        putWord32be dictMagic
        put d

    get = do
        pm <- getWord64be
        when (pm /= puzzMagic)
          (do pos <- bytesRead
              throw $ DictError "bad puzz magic" pos)
        dm <- getWord32be
        when (dm /= dictMagic)
          (do pos <- bytesRead
              throw $ DictError "bad dict magic" pos)
        DictFile <$> get