{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import qualified Data.ByteString.Char8 as S8
import qualified Data.Map              as M
import           Data.Proxy            (Proxy (..))
import           Data.Text             (Text)
import           Network.HTTP.Types
import           Network.Wai
import           Servant               ((:>), Get, JSON, Server)
import           Servant.Server        (serve)
import           Snap.Test
import           Snap.Wai
import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "plain wai" $ do
    it "handles requests" $ do
      let sh = serveWai waiApp
      res <- runHandler (get "foo/bar" M.empty) sh

      body <- getResponseBody res
      S8.putStrLn "body is:"
      S8.putStrLn body

      assertSuccess res
      assertBodyContains "Hello" res

  describe "servant api" $ do
    it "handles requests" $ do
      let sh = serveWai servantApp
      res <- runHandler (get "api" M.empty) sh

      body <- getResponseBody res
      S8.putStrLn "body is:"
      S8.putStrLn body

      assertSuccess res
      assertBodyContains "Hello" res

waiApp :: Application
waiApp _req respond = do
  putStrLn "responding"
  respond $ responseLBS status200 [] "Hello Wai"

type Api = "api" :> Get '[JSON] Text

servantApp :: Application
servantApp = serve (Proxy :: Proxy Api) server

server :: Server Api
server = pure "Hello Servant"
