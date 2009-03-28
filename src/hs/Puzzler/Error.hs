module Puzzler.Error where

import Control.Exception
import Data.Int( Int64 )
import Data.Typeable


-- | Parsing error when reading dictionaries.
data DictionaryError = DictError !String !Int64 
                       deriving (Show, Typeable)
instance Exception DictionaryError where
