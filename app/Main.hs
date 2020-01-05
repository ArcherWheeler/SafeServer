{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Policy

import Control.Monad.Except
import Control.Monad.Reader
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Aeson.Types
import Network.Wai
import Servant
import System.Directory
import Data.Aeson
import qualified Data.Aeson.Parser

import GHC.Generics
import Network.Wai.Handler.Warp
import PrivateServer
import Network.HTTP.Types.Status
import Data.Text.IO

import qualified Data.ByteString.Lazy as BS

import Control.Monad
import Data.Monoid
import qualified Data.Text as T

type SR = Safe MyPolicy

loadPi :: FilePath -> IO (PI MyPolicy T.Text)
loadPi = load Proxy

reloadPi :: Maybe String -> PI MyPolicy FilePath -> IO (PI MyPolicy T.Text)
reloadPi tok fp = 
  case tok of
    Just key -> reload Proxy (T.pack key) fp
    Nothing -> return noone

server :: Server API
server tok = event tok :<|> count tok :<|> important tok
  where event :: Maybe String -> String -> Handler (SR Resp)
        event tok file = withPi tok $ do
          pi <- loadPi ("./data/" <> file <> ".txt")
          let result = pmap pBlob pi
          return result
        
        count :: Maybe String -> Handler (SR Int)
        count tok = withPi tok $ do
          files <- listDirectory "./data"
          listOfPrivateInfo <- liftIO $ forM files (\fileName -> loadPi $ "./data/" <> fileName)
          let privateList = viewCollection listOfPrivateInfo
          let result = pmap pLength privateList
          return result 
        
        important :: Maybe String -> Handler (SR Resp)
        important tok = withPi tok $ do
          pi1 <- loadPi "./metadata/most_important.txt"
          let file = fmap (\str -> "./data/" <> T.unpack str <> ".txt") pi1
          pi2 <- reloadPi tok file
          let result = fmap Blob pi2
          return result

userAPI :: Proxy API
userAPI = Proxy

app :: Application
app = serve userAPI server

main :: IO ()
main = run 8080 app