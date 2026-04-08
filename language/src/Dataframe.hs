module Dataframe where

import Data.Vector.Unboxed

data Dataframe r c = Dataframe
    { rows :: !(Vector r)
    , columns :: !(Vector c)
    , rowCount :: !Int
    , colCount :: !Int
    }

