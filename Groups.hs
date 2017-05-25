import System.Environment (getArgs, getProgName)
import Data.List (nub, sortBy, groupBy)
import Words.Join.Opts   (processOptsL, readDict)
import Words.Join.Joined (Joined(joined)) 

groups :: Joined a => [a] -> [[a]]
groups = groups' []
  where groups' :: Joined a => [[a]] -> [a] -> [[a]]
        groups' xss [] = xss
        groups' xss (x:xs) = 
                let (yss,zss)= regroup x (groups' xss xs) in
                (x:concat yss):zss

        regroup :: Joined a => a -> [[a]] -> ([[a]],[[a]])
        regroup x = split (any (joined x))
	
        split :: (a -> Bool) -> [a] -> ([a],[a])
        split _ [] = ([],[])
        split p (x:xs)  | p x       = (x:ys,zs)
                        | otherwise = (ys,x:zs) 
                                where (ys,zs) = split p xs 
	
sortWith :: Ord b => (a -> b) -> [a] -> [a]
sortWith f = sortBy (\x y -> compare (f x) (f y))

groupWith :: Eq b => (a -> b) -> [a] -> [[a]]
groupWith f = groupBy (\x y -> (f x) == (f y))

main :: IO()
main = do
        args <- getArgs
        prog <- getProgName
        (d,l) <- processOptsL prog args
        zs <- readDict d
        sequence_ (map (\lxss -> do {   print (fst (head lxss));
                                        print (map snd lxss) } )
                (groupWith fst (sortWith fst 
                        (map (\xs -> (length xs,xs)) 
                                (groups (nub (filter ((== l).length) zs)))))))
