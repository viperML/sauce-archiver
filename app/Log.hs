module Log where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger
import qualified Control.Monad.Logger as L
import qualified Data.ByteString.Char8 as S8
import qualified Data.List as List
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified GHC.Show as S8

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
outputFor loc source level s =
    let
        (line, _) = loc.loc_start
        unknown = loc.loc_filename == "<unknown>"
     in
        S8.putStrLn
            $ mconcat
            $ List.intersperse
                " "
            $ catMaybes
                [ Just (formatLevel level)
                , if unknown then Nothing else Just (S8.pack (loc.loc_filename <> ":" <> show line))
                , Just (fromLogStr s)
                ]

runLog :: (MonadIO m) => LoggingT m a -> m a
runLog action =
    runLoggingT action outputFor
