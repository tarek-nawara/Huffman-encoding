{-# LANGUAGE RecordWildCards #-}
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
  , convert
  , codeList
  ) where

import Data.List (find, partition, sortBy)
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
decodeChar (Leaf c w) bits = (c, bits)
decodeChar (Fork l r _ _) (x:xs) =
  if x == 0
    then decodeChar l xs
    else decodeChar r xs

-- * Encoding Using huffman tree
-- | encode text using encode tree
encode :: CodeTree -> String -> [Bit]
encode _ [] = []
encode tree (c:cs) = encodeChar tree c ++ encode tree cs

-- Helper method to encode a single character
-- using encode tree
encodeChar :: CodeTree -> Char -> [Bit]
encodeChar Leaf {..} _ = []
encodeChar (Fork l r _ _) c =
  if treeContains l c
    then 0 : encodeChar l c
    else 1 : encodeChar r c

-- Helper method to check whether a tree
-- contains a char
treeContains :: CodeTree -> Char -> Bool
treeContains (Leaf c _) x = x == c
treeContains Fork {chars = xs} c = c `elem` xs

-- * Encode Using code table
type CodeTable = [(Char, [Bit])]

-- | Fast Encoding for a String
codeList :: CodeTable -> String -> [Bit]
codeList _ [] = []
codeList table (x:xs) = codeBits table x ++ codeList table xs

-- Helper method to encode one single character
codeBits :: CodeTable -> Char -> [Bit]
codeBits table c =
  case find (\x -> fst x == c) table of
    Nothing -> []
    Just (_, xs) -> xs

-- Helper method to convert a codetree into a codetable
convert :: CodeTree -> CodeTable
convert tree@Leaf {char = c} = encodeAll tree [c]
convert tree@Fork {chars = cs} = encodeAll tree cs

-- Helper method to encode all characters in a tree
-- to its corresponding list of bits
encodeAll :: CodeTree -> String -> CodeTable
encodeAll _ [] = []
encodeAll tree (c:cs) = (c, encodeChar tree c) : encodeAll tree cs
