{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module TestApp where

import Data.Text (Text)
import Yesod

data TestApp = TestApp
instance Yesod TestApp

mkYesod "TestApp" [parseRoutes|
/echo/#Text EchoR GET
|]

getEchoR :: Text -> Handler Html
getEchoR str = defaultLayout $ do
    setTitle "Hello, world!"
    [whamlet|<h1>#{str}|]
