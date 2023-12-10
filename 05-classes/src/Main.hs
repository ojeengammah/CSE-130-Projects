import qualified Language.Nano.Types  as Nano
import qualified Language.Nano.Eval   as Nano
import           Language.Nano.Repl
import           Text.Printf
import           GHC.IO.Encoding

-- https://util.unicode.org/UnicodeJsps/character.jsp?a=03BB
lambda :: String
lambda = "\x03BB"

main :: IO ()
main = do
    setLocaleEncoding utf8
    putStrLn welcome
    startLoop [] 0

startLoop :: Nano.Env -> Int -> IO ()
startLoop env index = do
    putStrFlush ("λ [" ++ show index ++ "] ")
    cmd <- getLine
    case strCmd cmd of
        CQuit        -> doQuit
        (CRun file)  -> doRun file
        (CEval eval) -> doEval env eval
        (CLoad file) -> do
                          newEnv <- doLoad file
                          putStrFlush "definitions:"
                          listIdentifiers newEnv
                          putStrLn ""
                          startLoop newEnv (index + 1)
        CUnknown     -> doUnknown
    startLoop env (index + 1)

listIdentifiers :: Nano.Env -> IO ()
listIdentifiers [] = putStrFlush ""
listIdentifiers ((id, _) : rest) = do
                                     putStrFlush (" " ++ id)
                                     listIdentifiers rest

--------------------------------------------------------------------------------
-- | Some useful functions 
--------------------------------------------------------------------------------
-- putStr   :: String -> IO ()
-- hFlush   :: 
-- putStrLn :: String -> IO ()
-- getLine  :: IO String 

