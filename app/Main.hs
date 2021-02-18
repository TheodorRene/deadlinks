{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Req
import qualified Data.ByteString.Char8 as B
import qualified Text.URI as URI

main = doReq (Url' "vg.no") >>= print

newtype Url' = Url' Text

doReq :: Url' -> IO Int
doReq (Url' url) = runReq defaultHttpConfig $ do 
    bs <- req GET (https url) NoReqBody bsResponse mempty
    return (responseStatusCode bs)
