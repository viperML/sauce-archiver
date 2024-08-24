module Log where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger
import qualified Control.Monad.Logger as L
import qualified Data.ByteString.Char8 as S8

newtype Style = Style S8.ByteString

ansiBlue :: Style
ansiBlue = Style "34"

ansiGreen :: Style
ansiGreen = Style "32"

ansiRed :: Style
ansiRed = Style "31"

withStyle :: Style -> S8.ByteString -> S8.ByteString
withStyle style input = "\x001B" <> "[" <> s <> "m" <> input <> "\x001B[0m"
  where
    Style s = style

formatLevel :: LogLevel -> S8.ByteString
formatLevel level =
    "["
        <> ( case level of
                L.LevelWarn -> withStyle ansiRed "WARN"
                L.LevelDebug -> withStyle ansiBlue "DEBUG"
                L.LevelInfo -> withStyle ansiGreen "INFO"
                L.LevelError -> withStyle ansiRed "ERROR"
                L.LevelOther _ -> withStyle ansiBlue "OTHER"
           )
        <> "]"

outputFor ::
    Loc ->
    LogSource ->
    LogLevel ->
    LogStr ->
    IO ()
outputFor loc source level s = S8.putStrLn $ formatLevel level <> " " <> fromLogStr s

runLog :: (MonadIO m) => LoggingT m a -> m a
runLog action =
    runLoggingT action outputFor
