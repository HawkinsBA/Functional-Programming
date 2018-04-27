{-# LANGUAGE GADTs #-}
{-#  LANGUAGE InstanceSigs #-}


data Map k v where
  Empty :: Map k v
  Node :: (Ord k) => Integer -> k -> v -> Map k v -> Map k v -> Map k v
  deriving Show

{-}
data Map k v where
  Empty :: Map k v
  TwoNode :: Map k v -> (k, v) -> Map k v -> Map k v
  ThreeNode :: Map k v -> (k,v) -> Map k v -> (k,v) -> Map k v -> Map k v
  FourNode :: Map k v -> (k,v) -> Map k v -> (k,v) -> Map k v -> (k,v) -> Map k v -> Map k v

insert :: (Ord k) => k -> v -> Map k v -> Map k v
insert k v Empty                  = TwoNode Empty (k,v) Empty
insert k v (TwoNode l (k1, v1) r)
  |k > k1                         = TwoNode l (k1, v1), (insert k v l)
  |k < k1                         = TwoNode (insert k v l) (k1, v1) r
  |otherwise                      = TwoNode l (k1, v) r
-}

imbalance :: Map k v -> Integer
imbalance Empty = 0
imbalance Node n1 k1 v1 (Empty) (Node n2 k2 v2 l r) = n2
imbalance Node n1 k1 v1 (Node n2 k2 v2 l r) (Empty) = n2
imbalance Node n1 k1 v1 (Node n2 k2 v2 l2 r2) (Node n3 k3 v3 l3 r3) = n2 - n3



insert :: (Ord k) => k -> v -> Map k v -> Map k v
insert k v Empty = Node k v Empty Empty
insert k v (Node a b m1 m2)
  |k < a     = Node a b (insert k v m1) m2
  |k > a     = Node a b m1 (insert k v m2)
  |otherwise = Node k v m1 m2


lookup' :: (Ord k) => k -> Map k v -> Maybe v
lookup' _ Empty = Nothing
lookup' k (Node a b m1 m2)
  |k < a     = lookup' k m1
  |k > a     = lookup' k m2
  |otherwise = Just b

fromList :: (Ord k) => [(k, v)] -> Map k v
fromList = foldr (\x acc -> insert (fst x) (snd x) acc) Empty

toList :: (Ord k) => Map k v -> [(k,v)]
toList Empty = []
toList (Node k v l r) = toList l ++ [(k,v)] ++ toList r


instance Functor (Map k) where
  fmap :: (a -> b) -> Map k a -> Map k b
  fmap _ Empty = Empty
  fmap f (Node k a l r) = Node k (f a) (fmap f l) (fmap f r)
