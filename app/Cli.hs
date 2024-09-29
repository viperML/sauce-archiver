module Cli (CliOptions (..), getCliOptions) where

import Options.Applicative

data CliOptions = CliOptions
    { inputFolder :: Maybe FilePath
    , sauceFolder :: Maybe FilePath
    , noSauceFolder :: Maybe FilePath
    }
    deriving (Show)

parser :: Parser CliOptions
parser =
    CliOptions
        <$> optional
            ( strOption
                ( long "input_folder"
                    <> short 'i'
                )
            )
        <*> optional
            ( strOption
                (long "sauce_folder" <> short 's')
            )
        <*> optional
            ( strOption
                (long "no_sauce_folder" <> short 'S')
            )

getCliOptions :: IO CliOptions
getCliOptions =
    execParser $
        info
            (parser <**> helper)
            ( fullDesc
                <> progDesc "Short desc"
                <> header "header"
            )
