{-
    Copyright (C) 2017, Garret Wassermann.

    This file is part of tigerc, the Tiger language compiler,
    based on the Tiger language in "Modern Compiler Implementation
    in ML" by Andrew W. Appel.

    tigerc is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    tigerc is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with tigerc.  If not, see <http://www.gnu.org/licenses/>.
-}

module Tiger.Semantic.RedBlackTree where

-- |Represents two colors of Red-Black tree
data Color = R | B deriving (Eq, Show)

-- |A RedBlackTree (or set) consists of either empty nodes or subtree nodes,
-- that themselves contain a left subtree, a value, and a right subtree.
-- Note: Empty nodes are considered to be black.
data RedBlackTree a = E | T Color (RedBlackTree a) a (RedBlackTree a)
                      deriving (Eq, Show)

empty :: Ord a => RedBlackTree a
empty = E
    
member :: Ord a => a -> RedBlackTree a -> Bool
member x E = False
member x (T _ a y b) = if x<y
                       then member x a
                       else if x>y
                            then member x b
                            else True

insert :: Ord a => a -> RedBlackTree a -> RedBlackTree a
insert x s = T B a y b
    where T _ a y b = ins s
          ins E = T R E x E
          ins s@(T color a y b) = if x<y
                                  then balance color (ins a) y b
                                  else if x>y
                                       then balance color a y (ins b)
                                       else s

-- |Balance a red-black tree to ensure invariants remain true
balance :: Ord a => Color -> RedBlackTree a -> a -> RedBlackTree a -> RedBlackTree a
balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance color a x b = T color a x b
