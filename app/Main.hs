{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeOperators #-}

import Data.Aeson
import GHC.Generics
import qualified Network.Wai.Handler.Warp as Warp
import Servant
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Media ((//), (/:))

import Debug.Trace

import Knot
import Polynomial

data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML ByteString where
  mimeRender _ bs = bs

data JsonLink = JsonLink
  { intersection_id :: Int
  , front_edge_ids  :: [Int]
  , back_edge_ids   :: [Int]
  } deriving (Generic, FromJSON, ToJSON)

json_to_link :: [JsonLink] -> Knot.Link
json_to_link [] = [Trivial]
json_to_link [JsonLink iid fids bids] = [InterSection (Front (fids !! 0),Front (fids !! 1),Back (bids !! 0),Back (bids !! 1))]
json_to_link (x:xs) = (json_to_link [x])++(json_to_link xs)

type API = "jones" :> Get '[HTML] ByteString
          :<|> "js" :> Raw
          :<|> "css" :> Raw
          :<|> "jones" :> "post" :> ReqBody '[JSON] [JsonLink] :> Post '[JSON] String


api :: Proxy API
api = Proxy

server :: ByteString -> Server API
server indexHtml = index
              :<|> serveDirectoryWebApp "app/js"
              :<|> serveDirectoryWebApp "app/css"
              :<|> post
              where
                index = pure indexHtml
                post :: [JsonLink] -> Handler String
                post js = return $ lp_to_string $ kauffman_bracket $ json_to_link js


main :: IO ()
main = do
      indexHtml <- B.readFile "app/view/index.html"
      putStrLn "Listening on port 80"
      Warp.run 80 $ serve api $ server indexHtml
