import System.Environment (getArgs, getProgName)
import Data.List (nub, sort, intersperse)
import Words.Join.Opts   (processOpts2, readDict)
import Words.Join.Joined (Joined(joined), removed)

joinwords:: (Eq a, Show a) => [a] -> [a] -> [[a]] -> (String,[[[a]]])
joinwords x y zs = 
        if x == y then ("Equal", [[x]])
        else let l = length x in 
                if l == length y
                then    let zs' = nub (filter ((== l).length) zs) in
                        let (xok,zs'') = removed x zs' in
                        let (yok,zs''') = removed y zs'' in
                        ((if xok then "" else (show x)++" not a word\n")++
                         (if yok then "" else (show y)++" not a word\n"),
                         joinwords' [[x]] [[y]] zs''')
                else ("Unequal lengths", [])

  where joinwords' :: Joined a => [[a]] -> [[a]] -> [a] -> [[a]]
        joinwords' xss yss zs =
                if null xss || null yss then [] -- ran out of neighbours        
                else case [ (reverse xs)++ys |  xs@(x:_) <- xss, 
                                                ys@(y:_) <- yss, 
                                                joined x y ] of
                        []      -> joinwords'' xss yss zs
                        ans     -> ans

          where joinwords'' :: Joined a => [[a]] -> [[a]] -> [a] -> [[a]]
                joinwords'' _   _   []  = []    -- ran out of words
                joinwords'' xss yss zs  = 
                    case searchwords xss yss zs of
                        Left (xss', yss', zs')  -> joinwords' xss' yss' zs'
                        Right ans               -> ans
        
searchwords :: Joined a => [[a]] -> [[a]] -> [a] 
                                -> Either ([[a]], [[a]], [a]) [[a]]
searchwords xss yss = searchwords' xss yss ([], [], [])
  where searchwords':: Joined a => [[a]] -> [[a]] -> ([[a]], [[a]], [a]) 
                                -> [a] -> Either ([[a]], [[a]], [a]) [[a]]
        searchwords' _   _   xyz [] = Left xyz 
        searchwords' xss yss (xss', yss', zs') (z:zs) =
            case ( joinedto z xss, joinedto z yss ) of
                ([], [])    -> searchwords' xss yss (xss', yss', z:zs') zs
                ([], yss'') -> searchwords' xss yss 
                                (xss', (map (z:) yss'')++yss', zs') zs
                (xss'', []) -> searchwords' xss yss  
                                ((map (z:) xss'')++xss', yss', zs') zs
                (xss'', yss'') -> Right (foldl (searchwords'' xss yss) 
                                            (joinedup xss'' z yss'') zs)
 
        searchwords'':: Joined a => [[a]] -> [[a]] -> [[a]] -> a -> [[a]]
        searchwords'' xss yss ans z =
            case (joinedto z xss, joinedto z yss) of 
                (xss'@(_:_), yss'@(_:_)) -> (joinedup xss' z yss')++ans
                _                        -> ans

        joinedto :: Joined a => a -> [[a]] -> [[a]]
        joinedto z = filter ((joined z).head)

        joinedup :: [[a]] -> a -> [[a]] -> [[a]]
        joinedup xss z yss = [ (reverse xs)++(z:ys)| xs <- xss, ys <- yss ]
        
main :: IO()
main = do
        args <- getArgs
        prog <- getProgName
        (d,x,y) <- processOpts2 prog args
        dictWords <- readDict d
        let (warning, ans) = joinwords x y dictWords 
        putStr warning
        case ans of 
            []  -> putStrLn "No solutions"
            _   -> do
                putStr (show (length ans))
                putStr " solution(s): of length "
                print (length (head ans))
                sequence_ (map (putStrLn.concat.(intersperse " > ")) (sort ans))
