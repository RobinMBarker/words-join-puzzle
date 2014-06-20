module Words.Join.Opts where

    import System.Console.GetOpt
    import Data.Char (isAscii, isLower, isDigit)
    import Words.Join.Dict (getDictFile)

    data OptData = Dictionary FilePath deriving Show;

    options :: [OptDescr OptData]
    options = [ Option ['d'] ["dict"]
		(ReqArg Dictionary "FILE") "list of words" ]

    processOpts :: String -> [String] -> IO (Maybe FilePath, String)
    processOpts prog argv =
       	case getOpt RequireOrder options argv of
    	    (o,[s],[])  -> return (readOpts o,s)
            (_,_,errs)	->
		ioError (userError (concat errs ++ usageInfo header options))
      where header = "Usage:\n "++prog++" [OPTION...] word"

    processOpts2 :: String -> [String] -> IO (Maybe FilePath, String, String)
    processOpts2 prog argv =
       	case getOpt RequireOrder options argv of
    	    (o,[s,t],[])-> return (readOpts o,s,t)
            (_,_,errs)	->
		ioError (userError (concat errs ++ usageInfo header options))
      where header = "Usage:\n "++prog++" [OPTION...] word word'"

    processOptsL :: String -> [String] -> IO (Maybe FilePath, Int)
    processOptsL prog argv =
	case getOpt RequireOrder options argv of
	    (o,[n],[]) | all isDigit n -> return (readOpts o, read n)
	    (_,_,errs) ->
		ioError (userError (concat errs ++ usageInfo header options))
      where header = "Usage:\n "++prog++" [OPTION...] number"

    readOpts :: [OptData] -> Maybe FilePath
    readOpts = foldl (\Nothing (Dictionary p) -> Just p) Nothing

    readDict :: Maybe FilePath -> IO([String])
    readDict optD = do	d <- getDictFile optD
			contents <- readFile d
			return (filter (all isAsciiLower) (words contents))
		where 	isAsciiLower c = isAscii c && isLower c

