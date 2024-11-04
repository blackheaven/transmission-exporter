{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Control.Lens
import Control.Monad
import Data.Aeson
import Data.Aeson.QQ.Simple
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as Build
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Env
import GHC.Generics
import Network.HTTP.Types (status200, status404)
import Network.Wai (rawPathInfo, responseLBS)
import Network.Wai.Handler.Warp (HostPreference, defaultSettings, runSettings, setHost, setPort)
import qualified Network.Wreq as Wreq
import Prometheus
import System.IO
import Text.Read (readMaybe)
import Prelude

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  settings <- Env.parse (Env.header "Transmission Prometheus Exporter") settingsP

  let warpSettings = setHost (fst settings.listenOn) $ setPort (snd settings.listenOn) defaultSettings
  putStrLn $ "Starting server on " <> show (fst settings.listenOn) <> ":" <> show (snd settings.listenOn) <> " with endpoint " <> show settings.endpoint
  runSettings warpSettings $ \req respond -> do
    let path = rawPathInfo req
    if path == settings.endpoint
      then getMetrics settings >>= respond . responseLBS status200 [("Content-Type", "text/plain")]
      else respond $ responseLBS status404 [("Content-Type", "text/plain")] "Not Found"

getMetrics :: Settings -> IO BL.ByteString
getMetrics settings = do
  let authHeader = Wreq.auth ?~ Wreq.basicAuth settings.transmissionUsername settings.transmissionPassword
      rpcEndpoint = settings.transmissionAddr <> "/transmission/rpc"
      neverThrow = Wreq.checkResponse ?~ (\_ _ -> return ())

  initResponse <-
    Wreq.postWith (Wreq.defaults & authHeader & neverThrow) rpcEndpoint [aesonQQ|{}|]

  let sessionIdHeader =
        Wreq.header "X-Transmission-Session-Id"
          .~ [initResponse ^. Wreq.responseHeader "X-Transmission-Session-Id"]

  torrentsResponse <-
    Wreq.postWith
      (Wreq.defaults & authHeader & sessionIdHeader)
      rpcEndpoint
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
      >>= Wreq.asJSON

  sessionResponse <-
    Wreq.postWith
      (Wreq.defaults & authHeader & sessionIdHeader)
      rpcEndpoint
      [aesonQQ|
        {
           "method": "session-get",
           "tag": 43
        }
      |]
      >>= Wreq.asJSON

  sessionStatsResponse <-
    Wreq.postWith
      (Wreq.defaults & authHeader & sessionIdHeader)
      rpcEndpoint
      [aesonQQ|
        {
           "method": "session-stats",
           "tag": 44
        }
      |]
      >>= Wreq.asJSON

  let Torrents torrents = torrentsResponse ^. Wreq.responseBody
      session = sessionResponse ^. Wreq.responseBody
      sessionStats = sessionStatsResponse ^. Wreq.responseBody
      singletonSG :: Text -> Text -> LabelPairs -> B.ByteString -> SampleGroup
      singletonSG name desc labels value = SampleGroup (Info name desc) GaugeType [Sample name labels value]
      sessionSG :: Text -> Text -> (Session -> B.ByteString) -> SampleGroup
      sessionSG name desc f = singletonSG name desc [] $ f session
      sessionStatsSG :: Text -> Text -> (SessionStats -> B.ByteString) -> SampleGroup
      sessionStatsSG name desc f = singletonSG name desc [] $ f sessionStats
      sessionStatsSubSG :: Text -> Text -> (SessionStatsSub -> B.ByteString) -> SampleGroup
      sessionStatsSubSG name desc f =
        SampleGroup
          (Info name desc)
          GaugeType
          [ Sample name [("type", "cumulative")] (f sessionStats.subCumulative),
            Sample name [("type", "current")] (f sessionStats.subCurrent)
          ]
      torrentsSG :: Text -> Text -> (Torrent -> B.ByteString) -> SampleGroup
      torrentsSG name desc f =
        SampleGroup
          (Info name desc)
          GaugeType
          (mkSample name f (\t -> [("id", T.pack (show t.id)), ("name", t.name)]) <$> torrents)
      mkSample :: Text -> (b -> B.ByteString) -> (b -> LabelPairs) -> b -> Sample
      mkSample name f mkPairs t = Sample name (mkPairs t) $ f t
      valNum :: (Num a, Show a) => (b -> a) -> b -> B.ByteString
      valNum f = T.encodeUtf8 . T.pack . show . f
      valBool :: (b -> Bool) -> b -> B.ByteString
      valBool f = valNum $ \t -> if f t then 1 :: Int else 0
      boolLabel name x = (name, if x then "1" else "0")

  return $
    Build.toLazyByteString $
      foldMap
        exportSampleGroup
        [ singletonSG "transmission_alt_speed_down" "Alternative max global download speed" [boolLabel "enabled" session.altSpeedDownEnabled] (valNum (.altSpeedDownBytes) session),
          singletonSG "transmission_alt_speed_up" "Alternative max global upload speed" [boolLabel "enabled" session.altSpeedUpEnabled] (valNum (.altSpeedUpBytes) session),
          sessionSG "transmission_cache_size_bytes" "Maximum size of the disk cache" (valNum (.cacheSizeBytes)),
          singletonSG "transmission_free_space" "Free space left on device to download to" [("download_dir", session.downloadDir), ("incomplete_dir", session.incompleteDir)] (valNum (.freeSpace) session),
          sessionSG "transmission_global_peer_limit" "Maximum global number of peers" (valNum (.globalPeerLimit)),
          singletonSG "transmission_queue_down" "Max number of torrents to download at once" [boolLabel "enabled" session.queueDownEnabled] (valNum (.queueDownBytes) session),
          singletonSG "transmission_queue_up" "Max number of torrents to upload at once" [boolLabel "enabled" session.queueUpEnabled] (valNum (.queueUpBytes) session),
          singletonSG "transmission_seed_ratio_limit" "The default seed ratio for torrents to use" [boolLabel "enabled" session.seedRatioLimitEnabled] (valNum (.seedRatioLimit) session),
          sessionStatsSubSG "transmission_session_stats_active" "The time transmission is active since" (valNum (.secondsActive)),
          sessionStatsSG "transmission_session_stats_download_speed_bytes" "Current download speed in bytes" (valNum (.downloadSpeedBytes)),
          sessionStatsSubSG "transmission_session_stats_downloaded_bytes" "The number of downloaded bytes" (valNum (.downloadedBytes)),
          sessionStatsSubSG "transmission_session_stats_files_added" "The number of files added" (valNum (.filesAdded)),
          sessionStatsSubSG "transmission_session_stats_sessions" "Count of the times transmission started" (valNum (.sessions)),
          sessionStatsSG "transmission_session_stats_torrents_active" "The number of active torrents" (valNum (.torrentsActive)),
          sessionStatsSG "transmission_session_stats_torrents_paused" "The number of paused torrents" (valNum (.torrentsPaused)),
          sessionStatsSG "transmission_session_stats_torrents_total" "The total number of torrents" (valNum (.torrentsTotal)),
          sessionStatsSG "transmission_session_stats_upload_speed_bytes" "Current download speed in bytes" (valNum (.uploadSpeedBytes)),
          sessionStatsSubSG "transmission_session_stats_uploaded_bytes" "The number of uploaded bytes" (valNum (.uploadedBytes)),
          singletonSG "transmission_speed_limit_down_bytes" "Max global download speed" [boolLabel "enabled" session.speedLimitDownEnabled] (valNum (.speedLimitDownBytes) session),
          singletonSG "transmission_speed_limit_up_bytes" "Max global upload speed" [boolLabel "enabled" session.speedLimitUpEnabled] (valNum (.speedLimitUpBytes) session),
          singletonSG "transmission_version" "Transmission version as label" [("version", session.version)] "1",
          torrentsSG "transmission_torrent_added" "The unixtime time a torrent was added" (valNum (.addedDate)),
          torrentsSG "transmission_torrent_done" "The percent of a torrent being done" (valNum @Int (floor . (.percentDone))),
          torrentsSG "transmission_torrent_download_bytes" "The current download rate of a torrent in bytes" (valNum (.rateDownload)),
          torrentsSG "transmission_torrent_downloaded_ever_bytes" "The amount of bytes that have been downloaded from a torrent ever" (valNum (.downloadedEver)),
          torrentsSG "transmission_torrent_finished" "Indicates if a torrent is finished (1) or not (0)" (valBool (.isFinished)),
          -- torrentsSG "transmission_torrent_peer_limit" "Maximum number of peers for a single torrent" (valNum (.peersSendingToUs)),
          torrentsSG "transmission_torrent_peers_sending_to_us" "The quantity of peers sending pieces of a torrent to us" (valNum (.peersSendingToUs)),
          torrentsSG "transmission_torrent_peers_connected" "The quantity of peers connected on a torrent" (valNum (.peersConnected)),
          torrentsSG "transmission_torrent_peers_getting_from_us" "The quantity of peers getting pieces of a torrent from us" (valNum (.peersGettingFromUs)),
          torrentsSG "transmission_torrent_ratio" "The upload ratio of a torrent" (valNum (.uploadRatio)),
          torrentsSG "transmission_torrent_status" "Status of a torrent" (valNum (.status)),
          torrentsSG "transmission_torrent_upload_bytes" "The current upload rate of a torrent in bytes" (valNum (.rateUpload)),
          torrentsSG "transmission_torrent_uploaded_ever_bytes" "The amount of bytes that have been uploaded from a torrent ever" (valNum (.uploadedEver))
        ]

data Settings = Settings
  { endpoint :: B.ByteString,
    listenOn :: (HostPreference, Int),
    transmissionAddr :: String,
    transmissionUsername :: B.ByteString,
    transmissionPassword :: B.ByteString
  }
  deriving stock (Show)

settingsP :: (Env.AsUnset e, Env.AsUnread e) => Env.Parser e Settings
settingsP =
  Settings
    <$> Env.var Env.str "WEB_PATH" (Env.def "/metrics" <> Env.helpDef (T.unpack . T.decodeUtf8) <> Env.help "Path for metrics")
    <*> Env.var listenR "WEB_ADDR" (Env.def ("0.0.0.0", 19091) <> Env.helpDef (\(h, p) -> show h <> ":" <> show p) <> Env.help "Address for this exporter to run")
    <*> Env.var Env.str "TRANSMISSION_ADDR" (Env.def "http://localhost:9091" <> Env.helpDef Prelude.id <> Env.help "Transmission address to connect with")
    <*> Env.var Env.str "TRANSMISSION_USERNAME" (Env.help "Transmission username")
    <*> Env.var Env.str "TRANSMISSION_PASSWORD" (Env.help "Transmission password")
  where
    listenR :: (Env.AsUnread e) => Env.Reader e (HostPreference, Int)
    listenR x =
      case (takeWhile (/= ':') x, drop 1 $ dropWhile (/= ':') x) of
        (rawHost, rawPort) ->
          case readMaybe rawPort of
            Nothing -> Left $ Env.unread $ "Unable to read port in " <> show x
            Just port -> Right (fromString $ if null rawHost then "*" else rawHost, port)

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
    withObject "SessionResponse" $ \response -> do
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
    withObject "SessionStatsResponse" $ \response -> do
      ensureResponse response 44
      arguments <- response .: "arguments"
      SessionStats
        <$> arguments .: "activeTorrentCount"
        <*> arguments .: "downloadSpeed"
        <*> arguments .: "activeTorrentCount"
        <*> arguments .: "pausedTorrentCount"
        <*> arguments .: "torrentCount"
        <*> arguments .: "uploadSpeed"
        <*> arguments .: "cumulative-stats"
        <*> arguments .: "current-stats"

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

exportSampleGroup :: SampleGroup -> Build.Builder
exportSampleGroup (SampleGroup info ty samples) =
  if null samples
    then mempty
    else prefix <> exportedSamples
  where
    exportedSamples = exportSamples samples
    name = metricName info
    help = metricHelp info
    prefix =
      Build.byteString $
        T.encodeUtf8 $
          T.unlines
            [ "# HELP " <> name <> " " <> T.concatMap escape help,
              "# TYPE " <> name <> " " <> T.pack (show ty)
            ]
    escape '\n' = "\\n"
    escape '\\' = "\\\\"
    escape other = T.pack [other]

exportSamples :: [Sample] -> Build.Builder
exportSamples samples =
  mconcat [exportSample s <> Build.charUtf8 '\n' | s <- samples]

exportSample :: Sample -> Build.Builder
exportSample (Sample name labels value) =
  Build.byteString (T.encodeUtf8 name)
    <> ( case labels of
           [] -> mempty
           l : ls ->
             Build.charUtf8 '{'
               <> exportLabel l
               <> mconcat [Build.charUtf8 ',' <> exportLabel l' | l' <- ls]
               <> Build.charUtf8 '}'
       )
    <> Build.charUtf8 ' '
    <> Build.byteString value

exportLabel :: (Text, Text) -> Build.Builder
exportLabel (key, value) =
  Build.byteString (T.encodeUtf8 key)
    <> Build.charUtf8 '='
    <> Build.stringUtf8 (show value)
