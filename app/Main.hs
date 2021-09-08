{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Maybe (fromJust,mapMaybe)
import Data.Monoid ((<>))
import Data.Text (Text, pack, stripPrefix)
import GHC.Generics
import Network.HTTP.Req
import qualified Data.ByteString.Char8 as B
import qualified Text.URI as URI
import Text.ParserCombinators.ReadP
import Control.Applicative((<|>))
import CMark(commonmarkToNode,Node(..), NodeType(..))
import Control.Monad
import qualified Text.URI as U

main = do
    content <- readFile "markdown_test.md" 
    let links2 = getLinks $ pack content
    let links = getURIs (getLinks s)
    x <- mapM getStatusCode links
    print x 

links = getURIs (getLinks s)

stuff = readFile "markdown_test.md"

stripHTTP = stripPrefix "http://"
stripHTTPS = stripPrefix "https://"

stripScheme z = case f z of
                    (Just x, _) -> x
                    (_ , Just x) -> x
                    (_ , _) -> z
                where f = \x -> (stripHTTPS x, stripHTTP x)
    

getURIs x =  mapMaybe useHttpsURI (mapMaybe (U.mkURI::Text -> Maybe U.URI) x)


getStatusCode (url,options) = runReq defaultHttpConfig{ httpConfigCheckResponse = \_ _ _ -> Nothing } $ do 
    bs <- req GET url NoReqBody bsResponse options
    return (responseStatusCode bs)


getLinks :: Text -> [Text]
getLinks content = parseNodeTree [] $ commonmarkToNode [] content

parseNodeTree :: [Text] -> Node -> [Text]
parseNodeTree currentUrls (Node _ (LINK url _) children) = 
    case children of
      [] -> currentUrls ++ [url]
      xs -> concatMap (parseNodeTree (currentUrls++[url])) children
parseNodeTree currentUrls (Node _ _ children) =
    case children of 
      [] -> currentUrls
      xs -> concatMap (parseNodeTree currentUrls) children



singleLink = "https://vg.no/hei?gurba=hei"::Text
s :: Text
s = " [A History of Haskell: being lazy with class](https://vg.no)\
\ The Beginning: The Eugenio Moggi Papers\
\- [Notions of Computations over Monads](https://hvorlengeerdettil.uka.no/asdasd)\
\- [Computational Lambda Calculus and Monads](https://www.disi.unige.it/person/MoggiE/ftp/lics89.pdf)\
\- [Monads for functional programming](http://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf)\
\- [The essence of functional programming](https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.41.9361&rep=rep1&type=pdf)\\"
