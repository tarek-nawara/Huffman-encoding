--------------------------------------------------
-- |
-- Module  : Huffman
-- Huffman Assignment
-- Implementation of huffman encoding algorithm
-- author Tarek Nawara
--
--------------------------------------------------
module Huffman
  ( CodeTree(..)
  , makeCodeTree
  , fromList
  , decode
  ) where

import Data.List (partition, sortBy)
import Data.Ord (comparing)

-- * Types and Utilities
-- | Representation of Huffman CodeTree
data CodeTree
  = Leaf { char :: Char
           -- ^ character inside the leaf
         , weight :: Int
           -- ^ weight of the character in the leaf
          }
  | Fork { left :: CodeTree
           -- ^ left part of the tree
         , right :: CodeTree
           -- ^ right part of the tree
         , chars :: String
           -- ^ list of all the characters in
           -- both the left and right childs
         , weight :: Int
           -- ^ combined left and right childs' weight
          }

-- * Implementation
-- Utility Function to group characters and there frequences
-- encoding
times :: String -> [(Char, Int)]
times [] = []
times xs@(x:_) =
  let (p, rem) = partition (== x) xs
  in (x, length p) : times rem

-- Build Leafs from a list of frequences
makeOrdLeafList :: [(Char, Int)] -> [CodeTree]
makeOrdLeafList freqs = sortBy (comparing weight) unordList
  where
    unordList = map (\(x, y) -> Leaf {char = x, weight = y}) freqs

-- checks whether a list of trees contains only one single tree
singleton :: [CodeTree] -> Bool
singleton [x] = True
singleton _ = False

-- insert a tree into a list of trees in a sorted order
insert :: CodeTree -> [CodeTree] -> [CodeTree]
insert tree [] = [tree]
insert tree trees@(x:xs) =
  if weight x >= weight tree
    then tree : trees
    else x : insert tree xs

-- Combine a list of codetree elements into a single tree
-- if the list size is less than two we simply stop
combine :: [CodeTree] -> [CodeTree]
combine [] = []
combine [x] = [x]
combine (x:y:trees) = combine (insert (makeCodeTree x y) trees)

-- | Merge two code trees into a single one
makeCodeTree :: CodeTree -> CodeTree -> CodeTree
makeCodeTree l r =
  Fork
  { left = l
  , right = r
  , chars = chars l ++ chars r
  , weight = weight l + weight r
  }

-- | Construct a huffman tree from a string
fromList :: String -> CodeTree
fromList xs =
  let trees = (makeOrdLeafList . times) xs
      (h:_) = until singleton combine trees
  in h

-- Decoding
type Bit = Int

-- | Decode a bit sequence using the code tree
--   and returns the original string
decode :: CodeTree -> [Bit] -> String
decode tree bits = do
  let (c, remBits) = decodeChar tree bits
  c : decode tree remBits

-- Helper function to decode one character
-- and extract it from the `bit` sequence
decodeChar :: CodeTree -> [Bit] -> (Char, [Bit])
decodeChar Leaf {char = c, weight = w} bits = (c, bits)
decodeChar Fork {left = l, right = r} (x:xs) =
  if x == 0
    then decodeChar l xs
    else decodeChar r xs
