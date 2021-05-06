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
import Text.ParserCombinators.ReadP
import Control.Applicative((<|>))

main = getStatusCode (Url' "vg.no") >>= print

newtype Url' = Url' Text

getStatusCode :: Url' -> IO Int
getStatusCode (Url' url) = runReq defaultHttpConfig $ do 
    bs <- req GET (https url) NoReqBody bsResponse mempty
    return (responseStatusCode bs)

isStartKlamme :: Char -> Bool
isStartKlamme char = char == '['

isSluttKlamme :: Char -> Bool
isSluttKlamme char = char == ']'

startklamme :: ReadP Char
startklamme = satisfy isStartKlamme

stopklamme :: ReadP Char
stopklamme = satisfy isSluttKlamme

parseBetween :: ReadP String
parseBetween = between (char '[') (char ']') (many get)

