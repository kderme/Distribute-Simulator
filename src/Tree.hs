{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}

module Tree where

import Data.List as L (splitAt,map,sum,length)

type Tree a b = Trie b a

-- | The order of a b is reversed for the derivation.
data Trie b a = Tree a [(b,Trie b a)] deriving (Foldable, Functor)

leaf :: a -> Tree a b
leaf a = Tree a []

flatten :: Tree a b -> [a]
flatten = foldr (:) []

countLeaves :: Tree a b -> Integer
countLeaves = foldTree (const mySum)

-- | Catamorphism on trees.
foldTree :: (a -> [v] -> v) -> Tree a b -> v
foldTree f = go
  where
    go (Tree x mp) =
      let
        vals = map snd mp
      in
        f x (L.map go vals)

mySum :: Num b => [b] -> b
mySum [] = 1
mySum ls = L.sum ls
{-
-- | Catamorphism on trees.

Int
countLeaves foldTree :: (a -> [a] -> v) -> Tree a b -> v
foldTree f = go
  where
    go (Tree x mp) =
      let
        ls = M.toList mp
        trees = map snd ls
      in
        f x $ fmap go trees
-}

showRec :: (Show b,Show a) => Int -> (Maybe b,Tree a b) -> String -> String
showRec lvl (k,Tree val mp) str = inside ++ "\n" ++ childrenStr
  where
  toj (a,b) = (Just a,b)
  inside = "(" ++ showMaybe k ++ "," ++ show val ++ ")"
  size = L.length mp
  (x,y) = L.splitAt (size-1) mp
  jx = map toj x
  jy = map toj y
  stringOf newStr (k,child)=str++"|\n"++str++"+--"++chStr
    where
      chStr=showRec (lvl+1) (k,child) (str++newStr)
  res1 = concatMap (stringOf "|  ") jx
  res2 = concatMap (stringOf "   ") jy
  childrenStr=res1++res2

showMaybe :: (Show b) => Maybe b -> String
showMaybe Nothing = "Start"
showMaybe (Just c) = show c

showTree :: (Show b,Show a) => Tree a b -> String
showTree tree = showRec 0 (Nothing,tree) ""
{-}
printTree :: Show a => Tree a b -> IO ()
printTree tr= do
  putStrLn $ showTree tr
  return ()
-}
-- | For instance declaration we can`t use synonyms. So we use Trie.
instance (Show a,Show b) => Show (Trie b a) where
  show = showTree

tree3 = Tree 1 [('r', leaf 2),('z', leaf 3)]
