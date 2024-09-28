module Cli (CliOptions(..), options) where

import Options.Applicative

data CliOptions = CliOptions
    { file :: FilePath
    }
    deriving (Show)

parser :: Parser CliOptions
parser =
    CliOptions
        <$> strOption
            ( long "file"
                <> short 'f'
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
