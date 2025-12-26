import System.Environment (getArgs, getProgName)
import Words.Join.Opts (processOptsL, readDict)
import Data.List.Ordered (isSortedBy)

main :: IO()
main = do
        args <- getArgs
        prog <- getProgName
        (d,l) <- processOptsL prog args
        zs <- readDict d
        let ok xs = (l <= length xs) && (isSortedBy (<) xs)
        sequence_ $ map putStrLn $ filter ok zs
