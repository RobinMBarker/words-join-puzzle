import System.Environment (getArgs, getProgName)
import Data.List (nub)
import Words.Join.Opts   (processOpts, readDict)
import Words.Join.Joined (Joined(joined), removed)

shell :: Joined a => ([a],[a]) -> ([a],[a])
shell (_, []) = ([], [])
shell (xs, z:zs) = 
        let (xs', zs') = shell (xs, zs) in
        if any (joined z) xs then (z:xs', zs') else (xs',z:zs')

layers :: Joined a => a -> [a] -> [[a]]
layers x zs = takeWhile (not.null) (map fst (iterate shell ([x], zs)))
        
nearwords :: (Eq a, Show a) => [a] -> [[a]] -> (String, [[[a]]])
nearwords x zs = 
        let l = length x in 
        let zs' = nub (filter ((== l).length) zs) in
        let (xok,zs'') = removed x zs' in
        ((if xok then "" else (show x)++" not a word\n"), layers x zs'')

main = do
        prog <- getProgName
        args <- getArgs
        (d,x) <- processOpts prog args
        dictWords <- readDict d
        let (warning, ans) = nearwords x dictWords
        putStr warning
        sequence_ (map print ans)
        print (length (concat ans))
        print (length ans)
