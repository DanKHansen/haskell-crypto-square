module CryptoSquare (encode) where

import Data.Char (isAlphaNum, toLower)
import Data.List (transpose)
import Data.List.Split (chunksOf)
import GHC.Float (sqrtDouble)

encode :: String -> String
encode xs = unwords . transpose . padTo r $ chunksOf r n
  where
    n = map toLower (filter isAlphaNum xs)
    len = length n
    r = ceiling $ sqrtDouble $ fromIntegral len

    padTo x = map (\cs -> cs ++ replicate (x - length cs) ' ')
