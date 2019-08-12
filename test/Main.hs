{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import           Control.Concurrent.Async (race_)
import           Control.Concurrent.MVar  (newEmptyMVar, putMVar, takeMVar)
import           Control.Monad            (void)
import           Data.Aeson               (Value)
import qualified Data.ByteString.Char8    as S8
import qualified Data.ByteString.Lazy     as BL
import qualified Data.Map                 as M
import           Data.Proxy               (Proxy (..))
import           Data.Text                (Text)
import qualified Network.HTTP.Simple      as Http
import           Network.HTTP.Types
import           Network.Wai
import           Servant.API              ((:<|>) (..), (:>), Get, JSON, Post,
                                           ReqBody)
import           Servant.Server           (Server, serve)
import           Snap.Http.Server         (httpServe, setPort, setStartupHook)
import           Snap.Test
import           Snap.Wai
import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "plain wai" $ do
    let sh = serveWai waiApp
    it "handles requests" $ do
      res <- runHandler (get "foo/bar" M.empty) sh
      assertSuccess res
      assertBodyContains "Hello" res

  describe "servant api" $ do
    let sh = serveWai servantApp
    it "handles get requests" $ do
      res <- runHandler (get "api/hello" M.empty) sh
      assertSuccess res
      assertBodyContains "Hello" res

    it "handles post requests" $ do
      res <- runHandler (postRaw "api/echo" "application/json" "{ \"yolo\": true }") sh
      assertSuccess res
      assertBodyContains "yolo" res

  describe "integration test" $ do
    let sh = serveWai servantApp
    it "works with a server" $ do
      started <- newEmptyMVar
      let cfg = setPort 12345 $ setStartupHook (putMVar started) mempty
      race_ (httpServe cfg sh) $ do
        void $ takeMVar started
        res <- Http.httpBS "http://localhost:1234/api/hello"
        Http.getResponseStatusCode res `shouldBe` 200
        Http.getResponseBody res `shouldSatisfy` S8.isInfixOf "Hello"

waiApp :: Application
waiApp req respond = do
  body <- BL.toStrict <$> strictRequestBody req
  S8.putStrLn $ "the request body is " <> body
  respond $ responseLBS status200 [] "Hello Wai"

type Api = "api" :>
  ( "hello" :> Get '[JSON] Text
  :<|> "echo" :> ReqBody '[JSON] Value :> Post '[JSON] Value )

servantApp :: Application
servantApp = serve (Proxy :: Proxy Api) server

server :: Server Api
server = pure "Hello Servant" :<|> pure
