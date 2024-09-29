module Log where

import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger
import qualified Control.Monad.Logger as L
import qualified Data.ByteString.Char8 as S8
import System.Console.ANSI

withSGR :: [SGR] -> IO () -> IO ()
withSGR style action = setSGR style >> action >> setSGR [Reset]

printLevel :: LogLevel -> IO ()
printLevel level = do
    putStr "["
    case level of
        L.LevelError -> withSGR [SetColor Foreground Dull Red] (putStr "WARN")
        L.LevelWarn -> withSGR [SetColor Foreground Dull Yellow] (putStr "WARN")
        L.LevelInfo -> withSGR [SetColor Foreground Dull Green] (putStr "INFO")
        L.LevelDebug -> withSGR [SetColor Foreground Dull Blue] (putStr "DEBUG")
        _ -> withSGR [SetColor Foreground Vivid Black] (putStr "OTHER")
    putStr "]"

printLoc :: Loc -> IO ()
printLoc loc = do
    let (line, _) = loc.loc_start
    let unknown = loc.loc_filename == "<unknown>"
    unless
        unknown
        $ do
            setSGR [SetColor Foreground Vivid Black]
            putStr " "
            putStr loc.loc_filename
            putStr ":"
            putStr $ show line
            setSGR [Reset]

outputFor ::
    Loc ->
    LogSource ->
    LogLevel ->
    LogStr ->
    IO ()
outputFor loc source level s = do
    printLevel level
    printLoc loc
    putStr " "
    S8.putStrLn (fromLogStr s)

runLog :: (MonadIO m) => LoggingT m a -> m a
runLog action =
    runLoggingT action outputFor
