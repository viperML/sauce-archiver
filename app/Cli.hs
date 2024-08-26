module Cli (CliOptions, options) where

import Options.Applicative

data CliOptions = CliOptions
    { verbose :: Bool
    , dir :: String
    }
    deriving (Show)

parser :: Parser CliOptions
parser =
    CliOptions
        <$> switch
            ( long "verbose"
                <> short 'v'
            )
        <*> strOption
            ( long "dir"
                <> short 'd'
            )

options :: IO CliOptions
options =
    execParser $
        info
            (parser <**> helper)
            ( fullDesc
                <> progDesc "Short desc"
                <> header "header"
            )

x :: CliOptions
x = CliOptions False ""

y = x.verbose
