module Words.Join.Dict where

    import Data.Maybe (fromMaybe)
    import System.Environment (getEnv)
    import System.IO.Error (isDoesNotExistError)
    import Control.Exception (catch)

    defaultDict :: FilePath
    defaultDict = "/usr/share/dict/words"
	
    dictEnvVar :: String
    dictEnvVar = "DICTWORDS"

    readEnvVar :: String -> IO(Maybe String)
    readEnvVar var = catch (getEnv var >>= return . Just) 
			(\e -> if isDoesNotExistError e 
				then return Nothing
				else ioError e)

    getDictFile :: Maybe FilePath -> IO(FilePath)
    getDictFile (Just d) = return d
    getDictFile Nothing = readEnvVar dictEnvVar
			>>= return . (fromMaybe defaultDict) 

-- SET DICTWORDS=C:\Users\Robin\Documents\12dict\5desk.txt

