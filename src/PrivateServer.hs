{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module PrivateServer 
( PI
, Safe
, GetJSON
, GetPlainText
, Policy
, withPi
, load
, reload
, policy
, viewCollection
, view
, noone
) where
import Network.HTTP.Types.Status

import Control.Monad
import Control.Monad.Trans
import Data.Witherable
import qualified Data.Text as T
import Data.Text.IO
import Data.List
import Data.Monoid
import qualified Data.Set as S
import Control.Applicative (Applicative(..))
import Data.Aeson.Types
import Network.Wai
import Servant
import System.Directory
import Data.Aeson
import qualified Data.Aeson.Parser
import Data.Maybe
import Data.Coerce

import GHC.Generics
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as CH

{-
    This module is intended to eventually be it's own library.
    For now it's just a module of simplicity.
-}

-- PI, Private Information Definitions --
newtype PI p a = Box (T.Text -> Maybe a)

debug :: T.Text -> PI p a -> a
debug tok (Box eval) = fromJust $ eval tok

instance Functor (PI p) where
    fmap fnc (Box eval) = 
        let newEval tok = case eval tok of 
                Just x -> Just (fnc x)
                Nothing -> Nothing
        in Box newEval

instance Applicative (PI p) where
    pure x = Box (\k -> Just x)
    (<*>) = ap

instance Monad (PI p) where 
    (Box eval) >>= fnc =
        let newEval tok = case eval tok of
                        Just x -> let (Box f) = fnc x in f tok
                        Nothing -> Nothing
        in Box newEval

noone :: PI p a
noone = Box (const Nothing)

mfromMaybe :: MonadPlus m => Maybe a -> m a
mfromMaybe = maybe mzero return

mcatMaybes :: MonadPlus m => m (Maybe a) -> m a
mcatMaybes = (>>= mfromMaybe)

mmapMaybe :: MonadPlus m => (a -> Maybe b) -> m a -> m b
mmapMaybe f = mcatMaybes . liftM f

viewCollection :: MonadPlus c => c (PI p a) -> PI p (c a)
viewCollection collection = Box (\key -> Just $ mmapMaybe (\(Box eval) -> eval key) collection)   

class Policy p where
    policy :: Proxy p -> T.Text -> (T.Text -> Maybe T.Text)

    load :: Proxy p -> FilePath -> IO (PI p T.Text)
    load prx file = do
        rawFile <- Data.Text.IO.readFile file
        return $ Box (policy prx rawFile)

    reload :: Proxy p -> T.Text -> PI p FilePath -> IO (PI p T.Text)
    reload prx key (Box eval) = 
        case eval key of 
            Just file -> load prx file
            Nothing -> return $ Box (const Nothing)

-- Servant Server Code --

data Safe p a = SafeBox a | Unsafe
view :: T.Text -> PI p a -> Safe p a
view key (Box eval) = case eval key of
                        Just x -> SafeBox x
                        Nothing -> Unsafe

data PJSON 
instance Accept PJSON where
    contentType _ = "application/json"

instance ToJSON a => MimeRender PJSON (Safe p a) where
    mimeRender _ (SafeBox a) = encode a
    mimeRender _ Unsafe = ""

data PTEXT
instance Accept PTEXT where
    contentType _ = "text/plain"

instance Show a => MimeRender PTEXT (Safe p a) where
    mimeRender _ (SafeBox a) = CH.pack (show a) <> "\n"
    mimeRender _ Unsafe = ""

type GetJSON p a = Get '[PJSON] (Safe p a)
type GetPlainText p a = Get '[PTEXT] (Safe p a)

withPi :: Maybe String -> IO (PI p a) -> Handler (Safe p a)
withPi tok action = do
    let key = T.pack $ fromMaybe "" tok
    pi <- liftIO action
    return $ view key pi