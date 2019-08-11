{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Test.Hspec
import Snap.Test
import Snap.Wai
import qualified Data.Map as M
import Network.Wai
import Network.HTTP.Types
import qualified Data.ByteString.Char8 as S8
import Servant ((:>), Get , JSON, Server)
import Servant.Server (serve)
import Data.Proxy (Proxy(..))
import           Data.Vault.Lazy (empty)
import Data.Text (Text)

main :: IO ()
main = hspec $ do
  describe "plain wai" $ do
    it "handles requests" $ do
      let sh = serveWai empty waiApp
      res <- runHandler (get "foo/bar" M.empty) sh

      body <- getResponseBody res
      S8.putStrLn "body is:"
      S8.putStrLn body

      assertSuccess res
      assertBodyContains "Hello" res

  describe "servant api" $ do
    it "handles requests" $ do
      let sh = serveWai empty servantApp
      res <- runHandler (get "api" M.empty) sh

      body <- getResponseBody res
      S8.putStrLn "body is:"
      S8.putStrLn body

      assertSuccess res
      assertBodyContains "Hello" res

waiApp :: Application
waiApp req respond = do
  putStrLn "responding"
  respond $ responseLBS status200 [] "Hello Wai"

type Api = "api" :> Get '[JSON] Text

servantApp :: Application
servantApp = serve (Proxy :: Proxy Api) server

server :: Server Api
server = pure "Hello Servant"
