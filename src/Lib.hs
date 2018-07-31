module Lib
    ( Tree,
      singleton,
      treeInsert,
      treeElem
    ) where

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node root leftTree rightTree)
    | x == root = Node x leftTree rightTree
    | x < root  = Node root (treeInsert x leftTree) rightTree
    | otherwise = Node root leftTree (treeInsert x rightTree) 

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem _ EmptyTree = False
treeElem x (Node root leftTree rightTree)
    | x == root = True
    | x < root = treeElem x leftTree
    | otherwise = treeElem x rightTree
