--------------------------------------------------
-- |
-- Huffman Assignment
-- Read text from input.txt and produce the huffman
-- code corresponding to that text
-- author Tarek Nawara
--
---------------------------------------------------
module Main
  ( module Main
  ) where

import Control.Monad
import Data.Char (isAlphaNum)
import Huffman (codeList, convert, decode, fromList)
import System.IO

-- | Main function
--   Reads input file and output its
--   corresponding encoding
main :: IO ()
main = do
  handle <- openFile "./input.txt" ReadMode
  contents <- hGetContents handle
  let contents' = filter isAlphaNum contents
      tree = convert (fromList contents')
      codedMsg = show (codeList tree contents')
  writeFile "./output.txt" codedMsg
