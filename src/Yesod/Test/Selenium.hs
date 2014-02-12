{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Yesod.Test.Selenium (
    module Test.WebDriver,
    module Test.WebDriver.Server,

    withApp,
    onBrowsers,
    visit
) where

import Blaze.ByteString.Builder
import Control.Concurrent
import Control.Exception
import qualified Data.ByteString as B
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Types.URI
import Network.Wai.Handler.Warp
import Test.WebDriver
import Test.WebDriver.Server
import Yesod.Core.Dispatch
import Yesod.Routes.Class

onBrowsers bs act = mapM_ (\b -> runSession defaultSession defaultCaps { browser = b } act) bs

visit r = openPage . T.unpack $
    "http://localhost:12345" <> (T.decodeUtf8 . toByteString . encodePathSegments . fst $ renderRoute r)

withApp app act = bracket
    (do wai <- toWaiAppPlain app
        forkIO $ runSettings Network.Wai.Handler.Warp.defaultSettings
            { settingsPort = 12345
            , settingsHost = "127.0.0.1"
            } wai
        )
    killThread
    (const $ withServer Test.WebDriver.Server.defaultSettings act)
