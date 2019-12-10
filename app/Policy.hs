{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Policy 
( API
, MyPolicy
, Resp(..)
) where 

import Servant
import GHC.Generics
import Data.Aeson
import qualified Data.Text as T

import PrivateServer

-- Policy Definition --

parseData :: T.Text -> (T.Text -> Maybe T.Text)
parseData fileContent = 
    let (l1:l2:ls) = T.lines fileContent
        names = T.splitOn " " l1
        (secretNames, secretData) = 
            if length ls > 1
                then (T.splitOn " " (head ls), ls !! 1)
                else ([], "")
    in \name -> case (name `elem` names, name `elem` secretNames) of
                        (False, True) -> Just secretData
                        (True, False) -> Just l2
                        (True, True) -> Just (l2 <> " " <> secretData)
                        _ -> Nothing

data MyPolicy
instance Policy MyPolicy where
    policy _ = parseData

-- API Definition --

newtype Resp = Blob { body :: T.Text } deriving (Eq, Show, Generic)
instance ToJSON Resp

type API = QueryParam "tok" String :>
    (
            "event" :> Capture "file" String :> GetJSON MyPolicy Resp
        :<|> "count" :> GetPlainText MyPolicy Int
        :<|> "important" :> GetJSON MyPolicy Resp
    )