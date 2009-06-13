module Puzzler.Pretty where

class Pretty p where
    pretty :: p -> String
