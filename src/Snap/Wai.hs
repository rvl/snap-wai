{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Snap.Wai ( serveWai ) where

import           Control.Monad.IO.Class   (liftIO)
import           Data.Binary.Builder      (Builder)
import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Char8    as B8
import           Data.CaseInsensitive     (CI (..))
import           Data.IORef               (IORef, newIORef, readIORef,
                                           writeIORef)
import           Data.List                (foldl')
import qualified Data.Map.Strict          as Map
import           Data.Maybe               (fromMaybe)
import           Data.Word                (Word64)
import qualified Network.HTTP.Types       as H
import           Network.Wai              (Application, Request (..),
                                           defaultRequest)
import           Network.Wai.Internal     (Response (..), ResponseReceived (..))
import qualified Network.Wai.Internal     as Wai
import           Snap.Core                (MonadSnap (..), modifyResponse,
                                           setResponseBody, setResponseStatus)
import qualified Snap.Core                as Snap
import           Snap.Internal.Http.Types as Snap
import           Snap.Internal.Http.Types (Request (..))
import           System.IO.Streams        (InputStream)
import qualified System.IO.Streams        as Stream (read, write)

-- | Converts a 'Network.Wai.Application' to a 'Snap' handler.
serveWai :: MonadSnap m => Application -> m ()
serveWai app = do
  liftIO $ putStrLn "serveWai start"
  req <- convertRequest <$> Snap.getRequest
  liftIO $ putStrLn $ "converted request: " ++ show req
  rvar <- liftIO (newIORef id :: IO (IORef (Snap.Response -> Snap.Response)))
  ResponseReceived <- liftIO $ app req $ \res -> do
    putStrLn "Response received from app"
    writeIORef rvar $ case res of
      ResponseBuilder st hdrs builder ->
        responseBuilder builder . responseBase st hdrs
      ResponseStream st hdrs strmbdy ->
        responseStream st (isHead req) strmbdy . responseBase st hdrs
      ResponseFile st hdrs path mpart ->
        responseFile path mpart . responseBase st hdrs
      ResponseRaw{} ->
        error "ResponseRaw is not supported"
    pure ResponseReceived
  liftIO (readIORef rvar) >>= modifyResponse

----------------------------------------------------------------------------
-- Request

convertRequest :: Snap.Request -> Wai.Request
convertRequest req@Request{..} = defaultRequest
  { requestMethod = B8.pack . show $ rqMethod
  , httpVersion = uncurry H.HttpVersion rqVersion
  , rawPathInfo = rqPathInfo
  , rawQueryString = rqQueryString
  , requestHeaders = convertRequestHeaders $ Snap.listHeaders req
  , isSecure = rqIsSecure
  -- , remoteHost = SockAddrInet rqClientPort rqClientAddr
  , pathInfo = H.decodePathSegments rqPathInfo
  , queryString = paramsToQuery rqParams
  , requestBody = readSnapRequest rqBody
  , vault = mempty
  , requestBodyLength = maybe Wai.ChunkedBody Wai.KnownLength rqContentLength
  , requestHeaderHost = Just rqHostName
  , requestHeaderRange = getHeader "Range" req
  , requestHeaderReferer = getHeader "Referer" req
  , requestHeaderUserAgent = getHeader "User-Agent" req
  }

readSnapRequest :: InputStream ByteString -> IO ByteString
-- readSnapRequest = fmap (fromMaybe B8.empty) . Stream.read
readSnapRequest s = do
  putStrLn "readSnapRequest"
  chunk <- Stream.read s
  putStrLn $ "chunk is " ++ show chunk
  pure (fromMaybe B8.empty chunk)

convertRequestHeaders :: [(CI ByteString, ByteString)] -> H.RequestHeaders
convertRequestHeaders = id

paramsToQuery :: Snap.Params -> H.Query
paramsToQuery = dist . Map.toList
  where
    dist xs = concat [ [(k, Just v) | v <- vs] ++ [(k, Nothing) | null vs] | (k,vs) <- xs]

----------------------------------------------------------------------------
-- Response

-- Converts Wai response status and headers to Snap
responseBase :: H.Status -> H.ResponseHeaders -> Snap.Response -> Snap.Response
responseBase (H.Status code reason) hdrs =
  setResponseStatus code reason .
  addHeaders (convertResponseHeaders hdrs)

addHeaders :: Snap.HasHeaders a => [(CI ByteString, ByteString)] -> a -> a
addHeaders hs a = foldl' (\xs (k,v) -> Snap.addHeader k v xs) a hs

convertResponseHeaders :: H.ResponseHeaders -> [(CI ByteString, ByteString)]
convertResponseHeaders = id

-- | Convert a 'ResponseBuilder'
responseBuilder :: Builder -> Snap.Response -> Snap.Response
responseBuilder builder = setResponseBody $ \out -> do
  putStrLn "ResponseBuilder writing"
  Stream.write (Just builder) out
  pure out

-- | Convert a 'ResponseStream'
responseStream :: H.Status -> Bool -> Wai.StreamingBody -> Snap.Response -> Snap.Response
responseStream st reqIsHead strmbdy
  | noBody st = id
  | reqIsHead = id
  | otherwise = responseStreaming strmbdy

responseStreaming :: Wai.StreamingBody -> Snap.Response -> Snap.Response
responseStreaming getBody = setResponseBody $ \out -> do
  putStrLn "ResponseStreaming"
  getBody (\b -> Stream.write (Just b) out) (pure ())
  pure out

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

-- | Convert a 'ResponseFile'
responseFile :: FilePath -> Maybe Wai.FilePart -> Snap.Response -> Snap.Response
responseFile path mpart r = r { rspBody = Snap.SendFile path (convertRange <$> mpart) }

convertRange :: Wai.FilePart -> (Word64, Word64)
convertRange (Wai.FilePart offset count _) = (fromIntegral offset, fromIntegral count)
