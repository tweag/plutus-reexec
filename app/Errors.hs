module Errors where

import Data.Text (Text)

data PSRErrors where
    OtherError :: Text -> PSRErrors
    deriving (Show, Eq)
