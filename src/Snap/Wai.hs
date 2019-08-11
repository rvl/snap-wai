{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Snap.Wai ( serveWai ) where

import           Control.Monad               (void)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Trans.Control (liftBaseWith, restoreM, control)
import           Data.Binary.Builder         (Builder, toLazyByteString)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8       as B8
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.CaseInsensitive        (CI (..))
import           Data.List                   (foldl')
import qualified Data.Map.Strict             as Map
import           Data.Maybe                  (fromMaybe)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import           Data.Vault.Lazy             (Vault)
import qualified Network.HTTP.Types          as H
import           Network.Wai                 (Application, Request (..),
                                              defaultRequest)
import           Network.Wai.Internal        (Response (..),
                                              ResponseReceived (..))
import qualified Network.Wai.Internal        as Wai
import           Snap.Core                   (MonadSnap (..), putResponse,
                                              setResponseBody, writeBuilder,
                                              setResponseStatus, sendFile, sendFilePartial)
import qualified Snap.Core                   as Snap
import           Snap.Internal.Http.Types    (Request (..))
import           System.IO.Streams           (InputStream, OutputStream)
import qualified System.IO.Streams           as Stream (read, write)

-- | Converts a 'Network.Wai.Application' to a 'Snap' handler.
serveWai :: MonadSnap m
         => Vault
         -- ^ Store for persistent state between requests.
         -> Application
         -- ^ The Wai app to run in the handler.
         -> m ()
serveWai vault app = do
  req <- convertRequest vault <$> Snap.getRequest
  liftIO $ putStrLn "Running app handler"
  void $ liftBaseWith $ \runInBase -> app req $ \res -> do
    runInBase $ case res of
      ResponseBuilder st hdrs builder -> do
        putResponse $ responseNoBody st hdrs
        liftIO $ L8.putStrLn $ "builder is: " <> toLazyByteString builder
        writeBuilder builder
      ResponseStream  st rsphdr strmbdy -> do
        liftIO $ putStrLn "streaming response"
        putResponse $ responseStream st rsphdr (isHead req) strmbdy
      ResponseFile st hdrs path mpart -> do
        putResponse $ responseNoBody st hdrs
        case mpart of
          Nothing ->
            sendFile path
          Just (Wai.FilePart offset count _) ->
            sendFilePartial path (fromIntegral offset, fromIntegral count)
      ResponseRaw{} -> do
        error "ResponseRaw is not supported"

    pure ResponseReceived

----------------------------------------------------------------------------
-- Request

convertRequest :: Vault -> Snap.Request -> Wai.Request
convertRequest vault req@Request{..} = defaultRequest
  { requestMethod = B8.pack . show $ rqMethod
  , httpVersion = uncurry H.HttpVersion rqVersion
  , rawPathInfo = rqPathInfo
  , rawQueryString = rqQueryString
  , requestHeaders = convertRequestHeaders $ Snap.listHeaders req
  , isSecure = rqIsSecure
  -- , remoteHost = SockAddrInet rqClientPort rqClientAddr
  , pathInfo = T.splitOn "/" . T.decodeUtf8 $ rqPathInfo
  , queryString = paramsToQuery rqParams
  , requestBody = readSnapRequest rqBody
  , vault = vault
  , requestBodyLength = Wai.ChunkedBody
  , requestHeaderHost = Just rqHostName
  -- , requestHeaderRange = _
  -- , requestHeaderReferer = _
  -- , requestHeaderUserAgent = _
  }

readSnapRequest :: InputStream ByteString -> IO ByteString
readSnapRequest = fmap (fromMaybe B8.empty) . Stream.read

convertRequestHeaders :: [(CI ByteString, ByteString)] -> H.RequestHeaders
convertRequestHeaders = id

paramsToQuery :: Snap.Params -> H.Query
paramsToQuery = dist . Map.toList
  where
    dist xs = concat [ [(k, Just v) | v <- vs] ++ [(k, Nothing) | null vs] | (k,vs) <- xs]

----------------------------------------------------------------------------
-- Response

responseStream :: H.Status -> H.ResponseHeaders -> Bool
               -> Wai.StreamingBody
               -> Snap.Response
responseStream st rsphdr reqIsHead strmbdy
  | noBody st = responseNoBody st rsphdr
  | reqIsHead = responseNoBody st rsphdr
  | otherwise = responseStreaming st rsphdr strmbdy

responseNoBody :: H.Status -> H.ResponseHeaders -> Snap.Response
responseNoBody (H.Status code reason) hdrs =
  setResponseStatus code reason $
  addHeaders (convertResponseHeaders hdrs)
  Snap.emptyResponse

responseStreaming :: H.Status -> H.ResponseHeaders -> Wai.StreamingBody -> Snap.Response
responseStreaming status hdrs getBody = setResponseBody body $ responseNoBody status hdrs
  where
    body :: OutputStream Builder -> IO (OutputStream Builder)
    body out = do
      getBody (\b -> Stream.write (Just b) out) (pure ())
      pure out

convertResponseHeaders :: H.ResponseHeaders -> [(CI ByteString, ByteString)]
convertResponseHeaders = id

addHeaders :: Snap.HasHeaders a => [(CI ByteString, ByteString)] -> a -> a
addHeaders hs a = foldl' (\xs (k,v) -> Snap.addHeader k v xs) a hs

noBody :: H.Status -> Bool
noBody = not . hasBody

hasBody :: H.Status -> Bool
hasBody s = sc /= 204
         && sc /= 304
         && sc >= 200
  where
    sc = H.statusCode s

isHead :: Wai.Request -> Bool
isHead = (== H.methodHead) . requestMethod
