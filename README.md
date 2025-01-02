# transmission-exporter

Drop-in replacement for [transmission-exporter](https://github.com/leijurv/transmission-exporter).

### Configuration

ENV Variable | Description
|----------|-----|
| WEB_PATH | Path for metrics, default: `/metrics` |
| WEB_ADDR | Address for this exporter to run, default: `0.0.0.0` |
| WEB_PORT | Port for this exporter to run, default: `19091` |
| TRANSMISSION_ADDR | Transmission address to connect with, default: `http://localhost:9091` |
| TRANSMISSION_USERNAME | Transmission username, no default (optional) |
| TRANSMISSION_PASSWORD | Transmission password, no default (optional) |
| TRANSMISSION_NO_AUTH | Transmission don't use any basic auth, default |

## Exposed metrics

```
# HELP transmission_alt_speed_down Alternative max global download speed
# HELP transmission_alt_speed_up Alternative max global upload speed
# HELP transmission_cache_size_bytes Maximum size of the disk cache
# HELP transmission_free_space Free space left on device to download to
# HELP transmission_global_peer_limit Maximum global number of peers
# HELP transmission_queue_down Max number of torrents to download at once
# HELP transmission_queue_up Max number of torrents to upload at once
# HELP transmission_seed_ratio_limit The default seed ratio for torrents to use
# HELP transmission_session_stats_active The time transmission is active since
# HELP transmission_session_stats_download_speed_bytes Current download speed in bytes
# HELP transmission_session_stats_downloaded_bytes The number of downloaded bytes
# HELP transmission_session_stats_files_added The number of files added
# HELP transmission_session_stats_sessions Count of the times transmission started
# HELP transmission_session_stats_torrents_active The number of active torrents
# HELP transmission_session_stats_torrents_paused The number of paused torrents
# HELP transmission_session_stats_torrents_total The total number of torrents
# HELP transmission_session_stats_upload_speed_bytes Current download speed in bytes
# HELP transmission_session_stats_uploaded_bytes The number of uploaded bytes
# HELP transmission_speed_limit_down_bytes Max global download speed
# HELP transmission_speed_limit_up_bytes Max global upload speed
# HELP transmission_version Transmission version as label
# HELP transmission_torrent_added The unixtime time a torrent was added
# HELP transmission_torrent_done The percent of a torrent being done
# HELP transmission_torrent_download_bytes The current download rate of a torrent in bytes
# HELP transmission_torrent_downloaded_ever_bytes The amount of bytes that have been downloaded from a torrent ever
# HELP transmission_torrent_finished Indicates if a torrent is finished (1) or not (0)
# HELP transmission_torrent_peer_limit Maximum number of peers for a single torrent
# HELP transmission_torrent_peers_connected The quantity of peers connected on a torrent
# HELP transmission_torrent_peers_getting_from_us The quantity of peers getting pieces of a torrent from us
# HELP transmission_torrent_peers_sending_to_us The quantity of peers sending pieces of a torrent to us
# HELP transmission_torrent_ratio The upload ratio of a torrent
# HELP transmission_torrent_status Status of a torrent
# HELP transmission_torrent_upload_bytes The current upload rate of a torrent in bytes
# HELP transmission_torrent_uploaded_ever_bytes The amount of bytes that have been uploaded from a torrent ever
```
