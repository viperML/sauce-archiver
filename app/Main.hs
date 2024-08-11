{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where


import Control.Monad.Logger
import Control.Concurrent.Chan
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)

import qualified Data.Text as T

action :: Chan T.Text -> IO ()
action chan = forever $ do
    newVal <- readChan chan
    print newVal

main :: IO ()
main = do 
    -- myChan :: Chan LogLine <- newChan
    myChan <- newChan

    thread <- forkIO $ action myChan

    print $ T.concat ["[thread:]", (T.pack $ show thread)] 

    _ <- forever $ do
        line <- getLine
        writeChan myChan (T.pack line)

    return ()

