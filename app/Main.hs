{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where


import Control.Monad.Logger
import Control.Concurrent.Chan


main :: IO ()
main = do 
    myChan :: Chan LogLine <- newChan

    return ()

