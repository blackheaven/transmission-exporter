{-# LANGUAGE QuasiQuotes #-}

module Data.Prometheus.Exporter.Transmission
  ( -- * Entrypooints
    getMetrics,
    Torrent (..),
    Session (..),
    SessionStats (..),
    SessionStatsSub (..),

    -- * Plumbing
    Endpoint (..),
    mkEndpointFromAddress,
    Auth (..),
    SessionId (..),
  )
where

import Control.Exception (catch, throw)
import Control.Lens
import Control.Monad
import Data.Aeson
import Data.Aeson.QQ.Simple
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as B
import Data.Maybe (fromJust)
import Data.Text (Text)
import GHC.Generics
import qualified Network.Wreq as Wreq
import System.IO
import Prelude

getMetrics :: Endpoint -> Auth -> Maybe SessionId -> IO ([Torrent], Session, SessionStats, SessionId)
getMetrics rpcEndpoint auth mSessionId = do
  (torrentsResponse, sessionId0) <-
    rpc
      rpcEndpoint
      auth
      mSessionId
      [aesonQQ|
        {
           "arguments": {
             "fields": [
              "id",
              "name",
              "hashString",
              "status",
              "addedDate",
              "leftUntilDone",
              "eta",
              "uploadRatio",
              "rateDownload",
              "rateUpload",
              "downloadDir",
              "downloadedEver",
              "isFinished",
              "percentDone",
              "seedRatioMode",
              "error",
              "errorString",
              "files",
              "fileStats",
              "peers",
              "trackers",
              "trackerStats",
              "peersConnected",
              "peersGettingFromUs",
              "peersSendingToUs",
              "totalSize",
              "uploadedEver"
             ]
           },
           "method": "torrent-get",
           "tag": 42
        }
      |]

  (sessionResponse, sessionId1) <-
    rpc
      rpcEndpoint
      auth
      (Just sessionId0)
      [aesonQQ|
        {
           "method": "session-get",
           "tag": 43
        }
      |]

  (sessionStatsResponse, sessionId2) <-
    rpc
      rpcEndpoint
      auth
      (Just sessionId1)
      [aesonQQ|
        {
           "method": "session-stats",
           "tag": 44
        }
      |]

  let Torrents torrents = torrentsResponse ^. Wreq.responseBody
      session = sessionResponse ^. Wreq.responseBody
      sessionStats = sessionStatsResponse ^. Wreq.responseBody

  return (torrents, session, sessionStats, sessionId2)

newtype Torrents = Torrents {unTorrents :: [Torrent]}
  deriving stock (Eq, Ord, Show)

instance FromJSON Torrents where
  parseJSON =
    withObject "TorrentsResponse" $ \response -> do
      ensureResponse response 42
      arguments <- response .: "arguments"
      Torrents <$> arguments .: "torrents"

ensureResponse :: Object -> Int -> Aeson.Parser ()
ensureResponse response expectedTag = do
  result :: Text <- response .: "result"
  unless (result == "success") $
    error $
      "result: "
        <> show result

  tag :: Int <- response .: "tag"
  unless (tag == expectedTag) $
    error $
      "tag: "
        <> show tag

data Torrent = Torrent
  { id :: Int,
    name :: Text,
    addedDate :: Double,
    percentDone :: Double,
    rateDownload :: Double,
    downloadedEver :: Double,
    rateUpload :: Double,
    uploadedEver :: Double,
    peersConnected :: Int,
    peersGettingFromUs :: Double,
    peersSendingToUs :: Double,
    uploadRatio :: Double,
    status :: Int,
    isFinished :: Bool
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON)

data Session = Session
  { altSpeedDownBytes :: Int,
    altSpeedDownEnabled :: Bool,
    altSpeedUpBytes :: Int,
    altSpeedUpEnabled :: Bool,
    cacheSizeBytes :: Double,
    freeSpace :: Double,
    downloadDir :: Text,
    incompleteDir :: Text,
    globalPeerLimit :: Int,
    queueDownBytes :: Int,
    queueDownEnabled :: Bool,
    queueUpBytes :: Int,
    queueUpEnabled :: Bool,
    seedRatioLimit :: Double,
    seedRatioLimitEnabled :: Bool,
    speedLimitDownBytes :: Double,
    speedLimitDownEnabled :: Bool,
    speedLimitUpBytes :: Double,
    speedLimitUpEnabled :: Bool,
    version :: Text
  }
  deriving stock (Eq, Ord, Show)

instance FromJSON Session where
  parseJSON =
    withObject "Session" $ \response -> do
      ensureResponse response 43
      arguments <- response .: "arguments"
      Session
        <$> arguments .: "alt-speed-down"
        <*> arguments .: "alt-speed-enabled"
        <*> arguments .: "alt-speed-up"
        <*> arguments .: "alt-speed-enabled"
        <*> arguments .: "cache-size-mb"
        <*> arguments .: "download-dir-free-space"
        <*> arguments .: "download-dir"
        <*> arguments .: "incomplete-dir"
        <*> arguments .: "peer-limit-global"
        <*> arguments .: "download-queue-size"
        <*> arguments .: "download-queue-enabled"
        <*> arguments .: "seed-queue-size"
        <*> arguments .: "seed-queue-enabled"
        <*> arguments .: "seedRatioLimit"
        <*> arguments .: "seedRatioLimited"
        <*> arguments .: "speed-limit-down"
        <*> arguments .: "speed-limit-down-enabled"
        <*> arguments .: "speed-limit-up"
        <*> arguments .: "speed-limit-up-enabled"
        <*> arguments .: "version"

data SessionStats = SessionStats
  { active :: Double,
    downloadSpeedBytes :: Double,
    torrentsActive :: Int,
    torrentsPaused :: Int,
    torrentsTotal :: Int,
    uploadSpeedBytes :: Double,
    subCurrent :: SessionStatsSub,
    subCumulative :: SessionStatsSub
  }
  deriving stock (Eq, Ord, Show)

instance FromJSON SessionStats where
  parseJSON =
    withObject "SessionStats" $ \response -> do
      ensureResponse response 44
      arguments <- response .: "arguments"
      SessionStats
        <$> arguments .: "activeTorrentCount"
        <*> arguments .: "downloadSpeed"
        <*> arguments .: "activeTorrentCount"
        <*> arguments .: "pausedTorrentCount"
        <*> arguments .: "torrentCount"
        <*> arguments .: "uploadSpeed"
        <*> arguments .: "current-stats"
        <*> arguments .: "cumulative-stats"

data SessionStatsSub = SessionStatsSub
  { secondsActive :: Double,
    downloadedBytes :: Double,
    filesAdded :: Int,
    sessions :: Int,
    uploadedBytes :: Double
  }
  deriving stock (Eq, Ord, Show)

instance FromJSON SessionStatsSub where
  parseJSON =
    withObject "SessionStatsSub" $ \s ->
      SessionStatsSub
        <$> s .: "secondsActive"
        <*> s .: "downloadedBytes"
        <*> s .: "filesAdded"
        <*> s .: "sessionCount"
        <*> s .: "uploadedBytes"

-- | RPC
rpc ::
  (ToJSON payload, FromJSON responseBody) =>
  Endpoint ->
  Auth ->
  Maybe SessionId ->
  payload ->
  IO (Wreq.Response responseBody, SessionId)
rpc (Endpoint rpcEndpoint) auth mSessionId payload = do
  let authHeader =
        case auth of
          NoAuth -> Prelude.id
          BasicAuth user password -> Wreq.auth ?~ Wreq.basicAuth user password
      neverThrow = Wreq.checkResponse ?~ (\_ _ -> return ())
      sessionIdHeadrName = "X-Transmission-Session-Id"
      mkSessionIdHeader (SessionId sessionId) = Wreq.header sessionIdHeadrName .~ [sessionId]

  firstResponse <-
    Wreq.postWith
      (Wreq.defaults & authHeader & neverThrow & maybe Prelude.id mkSessionIdHeader mSessionId)
      rpcEndpoint
      (toJSON payload)

  let newSessionId = SessionId $ firstResponse ^. Wreq.responseHeader sessionIdHeadrName
  (lastResponse, sessionId) <-
    if firstResponse ^. Wreq.responseStatus . Wreq.statusCode /= 409
      then return (firstResponse, fromJust mSessionId)
      else do
        retryResponse <-
          Wreq.postWith
            (Wreq.defaults & authHeader & mkSessionIdHeader newSessionId)
            rpcEndpoint
            (toJSON payload)
        return (retryResponse, newSessionId)

  parsedResponse <-
    Wreq.asJSON lastResponse
      `catch` \ex@(Wreq.JSONError _) -> print lastResponse >> print (lastResponse ^. Wreq.responseBody) >> throw ex
  return (parsedResponse, sessionId)

newtype Endpoint = Endpoint String

mkEndpointFromAddress :: String -> Endpoint
mkEndpointFromAddress address = Endpoint $ address <> "/transmission/rpc"

data Auth = NoAuth | BasicAuth B.ByteString B.ByteString

newtype SessionId = SessionId B.ByteString
