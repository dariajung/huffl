module LZ where

import Data.List
import Data.Array

type Offset = Int
type Length = Int
type Index = Int

data Encoded a = Encoded 