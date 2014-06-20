module Words.Join.Joined where 

    class Joined a where
    	joined :: a -> a -> Bool

    instance Eq a => Joined [a] where
    	joined (x:xs) 	(y:ys) 	= if x == y then joined xs ys else xs == ys
	joined [] 	[] 	= error "joined: equal strings"
	joined _ 	_ 	= error "joined: unequal length strings"
 
    remove :: Eq a => a -> [a] -> Maybe [a]
    remove _ [] = Nothing
    remove x (y:xs) | x == y 	= Just xs
		    | otherwise	= fmap (y:) (remove x xs)

    removed :: Eq a => a -> [a] -> (Bool,[a])
    removed x xs = maybe (False,xs) ((,)True) (remove x xs)

