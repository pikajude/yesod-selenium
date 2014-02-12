{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class
import TestApp
import Test.Hspec
import Yesod.Test.Selenium

main :: IO ()
main = withApp TestApp $ hspec $
    describe "echo page" $
        it "should echo whatever is given to it" $
            onBrowsers [firefox] $ do
                let msg1 = "Hello, world!"
                visit (EchoR msg1)
                t <- getTitle
                liftIO $ t `shouldBe` msg1
