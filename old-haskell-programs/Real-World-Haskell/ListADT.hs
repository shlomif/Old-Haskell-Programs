-- file: ch03/ListADT.hs
data List a = Cons a (List a)
            | Nil
              deriving (Show)

fromList :: (List a) -> [a]

fromList Nil = []
fromList (Cons x xs) = x:(fromList xs)

data Tree a = Node a (Maybe a) (Maybe a)
              deriving (Show)
