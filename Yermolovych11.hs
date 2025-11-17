{-# OPTIONS_GHC -Wall #-}
module Yermolovych11 where

data Tree23 a  = Empty23 -- порожнє 2-3-дерево!!! 
               | Node2 (Tree23 a) a (Tree23 a) 
               | Node3 (Tree23 a) a (Tree23 a) a (Tree23 a)
               | Leaf a   
                   deriving (Eq, Show)

-- Задача 1 ------------------------------------
h23  :: (Ord a) => Tree23 a -> Int
h23 Empty23             = -1
h23 (Leaf _)            = 0
h23 (Node2 l _ r)       = 1 + max (h23 l) (h23 r)
h23 (Node3 l _ m _ r)   = 1 + maximum [h23 l, h23 m, h23 r]

-- Задача 2 ------------------------------------
minmax23  :: (Ord a) => Tree23 a -> Maybe (a,a) 
minmax23 t
  | containsEmpty t = Nothing
  | otherwise =
      case collectKeys t of
        []     -> Nothing   -- формально не повинно траплятись, але на всякий випадок
        (x:xs) -> Just (foldl min x xs, foldl max x xs)

containsEmpty :: Tree23 a -> Bool
containsEmpty Empty23          = True
containsEmpty (Leaf _)         = False
containsEmpty (Node2 l _ r)    = containsEmpty l || containsEmpty r
containsEmpty (Node3 l _ m _ r) = containsEmpty l || containsEmpty m || containsEmpty r

collectKeys :: Tree23 a -> [a]
collectKeys Empty23          = []
collectKeys (Leaf v)         = [v]
collectKeys (Node2 l x r)    = collectKeys l ++ [x] ++ collectKeys r
collectKeys (Node3 l x m y r) = collectKeys l ++ [x] ++ collectKeys m ++ [y] ++ collectKeys r

-- Задача 3 ------------------------------------
isTree23  :: (Ord a) => Tree23 a -> Bool 
isTree23 Empty23 = True
isTree23 t       = case check23 t of
                     Nothing -> False
                     Just _  -> True
data Info a = Info { iMin :: a
                   , iMax :: a
                   , iHeight :: Int
                   }
check23 :: (Ord a) => Tree23 a -> Maybe (Info a)
check23 Empty23      = Nothing
check23 (Leaf v)     = Just (Info v v 0)
check23 (Node2 l x r) = do
  Info lmin lmax lh <- check23 l
  Info rmin rmax rh <- check23 r
  if lh /= rh then
    Nothing
  else if lmax <= x && x == rmin then
    Just (Info lmin rmax (lh + 1))
  else
    Nothing
check23 (Node3 l x m y r) = do
  Info lmin lmax lh <- check23 l
  Info mmin mmax mh <- check23 m
  Info rmin rmax rh <- check23 r
  if lh /= mh || mh /= rh then
    Nothing
  else if lmax <= x && x == mmin  
       && mmax <= y && y == rmin 
  then
    Just (Info lmin rmax (lh + 1))
  else
    Nothing

-- Задача 4 ------------------------------------
eqTree23 :: (Ord a) => Tree23 a -> Tree23 a -> Bool
eqTree23 t1 t2 = leaves t1 == leaves t2
  where
    leaves :: Tree23 a -> [a]
    leaves Empty23          = []
    leaves (Leaf v)         = [v]
    leaves (Node2 l _ r)    = leaves l ++ leaves r
    leaves (Node3 l _ m _ r) = leaves l ++ leaves m ++ leaves r

-- Задача 5 ------------------------------------
elemTree23 :: (Ord a) => Tree23 a -> a -> Bool
elemTree23 Empty23 _          = False
elemTree23 (Leaf v) x         = x == v
elemTree23 (Node2 l k r) x
  | x < k     = elemTree23 l x
  | otherwise = elemTree23 r x
elemTree23 (Node3 l x m y r) v
  | v < x     = elemTree23 l v
  | v < y     = elemTree23 m v
  | otherwise = elemTree23 r v

-- Задача 6 ------------------------------------
insNode :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insNode _ Empty23   = error "insNode: Empty23 as root (precondition violated)"
insNode _ (Leaf _)  = error "insNode: Leaf as root (precondition violated)"
insNode v (Node2 tl x tr)
  | v < x =
      let (a, split) = insert v tl in
      case split of
        Nothing       -> (Node2 a x tr, Nothing)
        Just (w, b)   -> (Node3 a w b x tr, Nothing)
  | otherwise =
      let (a, split) = insert v tr in
      case split of
        Nothing       -> (Node2 tl x a, Nothing)
        Just (w, b)   -> (Node3 tl x a w b, Nothing)
insNode v (Node3 tl x tm y tr)
  | v < x =
      let (a, split) = insert v tl in
      case split of
        Nothing     -> (Node3 a x tm y tr, Nothing)
        Just (w, b) -> (Node2 a w b, Just (x, Node2 tm y tr))
  | v < y =
      let (a, split) = insert v tm in
      case split of
        Nothing     -> (Node3 tl x a y tr, Nothing)
        Just (w, b) -> (Node2 tl x a, Just (w, Node2 b y tr))
  | otherwise =
      let (a, split) = insert v tr in
      case split of
        Nothing     -> (Node3 tl x tm y a, Nothing)
        Just (w, b) -> (Node2 tl x tm, Just (y, Node2 a w b))

-- Задача 7 ------------------------------------
sortList :: (Ord a) => [a] -> [a]
sortList xs = toLeaves (foldl insTree23 Empty23 xs)

toLeaves :: Tree23 a -> [a]
toLeaves Empty23          = []
toLeaves (Leaf v)         = [v]
toLeaves (Node2 l _ r)    = toLeaves l ++ toLeaves r
toLeaves (Node3 l _ m _ r) = toLeaves l ++ toLeaves m ++ toLeaves r

-------------------------------------------------------------
isTerminal :: (Ord a) => Tree23 a -> Bool
isTerminal (Node2 (Leaf _) _ _)     = True 
isTerminal (Node3 (Leaf _) _ _ _ _) = True
isTerminal _                        = False

----------------------------------------------------
insTree23 :: (Ord a) => Tree23 a -> a -> Tree23 a
insTree23 Empty23 v  = Leaf v 
insTree23 (Leaf a) v | v < a     = Node2 (Leaf v) a (Leaf a)
                     | otherwise = Node2 (Leaf a) v (Leaf v)
insTree23 t v = case insert v t of 
                     (tl,Just (x,tr)) -> Node2 tl x tr 
                     (tl, Nothing)    -> tl

insert :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insert v tr | isTerminal tr = insTerm v tr
            | otherwise     = insNode v tr

insTerm :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insTerm v (Node2 (Leaf l) x tr) 
        | v < l     = (Node3 (Leaf v) l (Leaf l) x tr, Nothing)
        | v < x     = (Node3 (Leaf l) v (Leaf v) x tr, Nothing)
        | otherwise = (Node3 (Leaf l) x tr v (Leaf v), Nothing)
insTerm v (Node3 (Leaf l) x tm y tr)
        | v < l     = (Node2 (Leaf v) l (Leaf l),Just (x, Node2 tm y tr))
        | v < x     = (Node2 (Leaf l) v (Leaf v),Just (x, Node2 tm y tr))
        | v < y     = (Node2 (Leaf l) x tm,Just (v, Node2 (Leaf v) y tr))
        | otherwise = (Node2 (Leaf l) x tm,Just (y, Node2 tr v (Leaf v)))
insTerm _ _         = error "insTerm"

----------------------------------------------------------------------------------
tr1, tr2,  tr3, tr4,tr5 :: Tree23 Int
tr1 =  Node2 (Node2 (Node2 (Leaf 0) 1 (Leaf 1)) 
                     2
                    (Node2 (Leaf 2) 3 (Leaf 3)))
              4
             (Node2 (Node2 (Leaf 4) 5 (Leaf 5)) 
                     6
                    (Node2 (Leaf 6) 7 (Leaf 7)))
        
tr2 =  Node3 (Node2 (Leaf 0) 1 (Leaf 1))
              2
             (Node3 (Leaf 2) 3 (Leaf 3) 4 (Leaf 4))
              5
             (Node3 (Leaf 5) 6 (Leaf 6) 7 (Leaf 7))

tr3 = Node3 (Node2 (Leaf 2) 5 (Leaf 5))
            7
            (Node3 (Leaf 7) 8 (Leaf 8) 12 (Leaf 12))
            16
            (Node2 (Leaf 16) 19 (Leaf 19))

tr4 = Node3 (Node2 (Leaf 2) 5 (Leaf 5))
            7
            (Node3 (Leaf 7) 8 (Leaf 8) 12 (Leaf 12))
            16
            (Node3 (Leaf 16) 18 (Leaf 18) 19 (Leaf 19))

tr5 = Node2 (Node2 (Node2 (Leaf 2) 5 (Leaf 5))
                    7
                   (Node2 (Leaf 7) 8 (Leaf 8)) 
            )
            10
            (Node2 (Node2 (Leaf 10) 12 (Leaf 12))
                   16
                   (Node3 (Leaf 16) 18 (Leaf 18) 19 (Leaf 19))
            )

tr1A, tr2A, tr2B :: Tree23 Int
tr1A =  Node2 (Node2 (Leaf 0) 1 (Leaf 1)) 
              2
              (Node2 (Leaf 2) 3 (Leaf 3))    
    
tr2A =  Node2 (Node3 (Leaf 2) 3 (Leaf 3) 4 (Leaf 4))
              5
             (Node3 (Leaf 5) 6 (Leaf 6) 7 (Leaf 7))

tr2B =  Node2 (Node2 (Leaf 3) 4 (Leaf 4))
              5
             (Node3 (Leaf 5) 6 (Leaf 6) 7 (Leaf 7))
