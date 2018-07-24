#As someone who has monthly playlists, 
#I always thought it'd be awesome to be able to have a master cumulative playlist, divided by moods of the songs.
#And this is an attempt to create something like that, with the help from Spotify API.

#loading package
library(httr)
library(dplyr)

#my own client ID and secret for the sake of the operation of the code.
clientID <- 'e68088a14ff04e21b372d6d6a71d1a6b'
secret <- '1bb980ba6b5d436ba6a8ebe915071daf'

response_1 <- POST(
  'https://accounts.spotify.com/api/token',
  accept_json(),
  authenticate(clientID, secret),
  body = list(grant_type = 'client_credentials'),
  encode = 'form',
  verbose()
)

mytoken <- content(response_1)$access_token
## creating ID's. For your own playlist, use your own values for these!
playlistID <- "2zrVrilxpmlq1Z1iPMvfCN"
#my own userID here for the sake of the code running.
userID <- 'spider3003'
#token
hv <- paste0('Bearer ', mytoken)
#different token used for authorization for POST. You have to go to 
#https://beta.developer.spotify.com/console/post-playlist-tracks/?user_id=3&playlist_id=&position=&uris=
#to get the token each time, because the token expires after certain time.
hvmodify <- paste0('Bearer ', "BQBS5TVnLyGDLJ82M1XOopZHd-SK-F1RFrr1bMoCb6nMM3oCltYnjNrZCLZJzxFsRe0BLKbCCSToaf0KTQVQN9GV2dbvk2WmMGI1KnsTWAZ3608c9xbjoUUgXUrDz7kH3I6rHLeobIaspwEFsiprxe2xgfqDp8NFgAVphe2nazdgcGE")
#setting url for playlist
URL_playlist <- paste0('https://api.spotify.com/v1/users/', userID, 
                       '/playlists/', playlistID, '/tracks')
#fetching the info3
response_playlist <- GET(url = URL_playlist, add_headers(Authorization = hvmodify))
playlist <- content(response_playlist)
#now let's run the loops for each song
#first creating a dataframe with the first song in the playlist.
#from there we can use rbind feature with for loops
songID <- playlist$items[[1]]$track$id
URL_song <- paste0('https://api.spotify.com/v1/audio-features/', songID)
response_song <- GET(url = URL_song, add_headers(Authorization = hv))
#creating a new data frame songs. This is differently named to the object used in for loop below.
songs <- as.data.frame(content(response_song))
for(i in 1:(playlist$total-1)) {
  songID <- playlist$items[[i+1]]$track$id
  URL_song <- paste0('https://api.spotify.com/v1/audio-features/', songID)
  response_song <- GET(url = URL_song, add_headers(Authorization = hv))
  song <- as.data.frame(content(response_song))
  #merging it to create a mastersheet
  songs <- rbind(songs, song)
}

#now, let's divide into two different playlists, depending on the valence (happiness).
#dividing by valence (point of division = 0.5)
happier_songs <- songs %>%
  filter(valence >= 0.5)
notashappy_songs <- songs %>%
  filter(valence < 0.5)

#loop for happy songs
#have to start with an initial vector I can rbind to!
URL_track <- as.character(happier_songs$track_href[1])
response_track <- GET(url = URL_track, add_headers(Authorization = hv))
track <- content(response_track)
songs_happy <- track$name
#now the loop
for(i in 2:nrow(happier_songs)) {
  URL_track <- as.character(happier_songs$track_href[i])
  response_track <- GET(url = URL_track, add_headers(Authorization = hv))
  track <- content(response_track)
  name <- track$name
  songs_happy <- c(songs_happy, name)
}

#creating urls for the playlists I'll be adding these songs into.
playlistID_happy <- '4VOmosPFwJUMUnY9cLg38F'
playlistID_sad <- '4Vn8m6IDUXzrPVwA7GQT2G'

vec_happy <- as.vector(happier_songs$id)

for(i in 1:length(vec_happy)) {
  URL_temp <- paste0('https://api.spotify.com/v1/users/', userID, 
                     '/playlists/', playlistID_happy, '/tracks?uris=spotify%3Atrack%3A', vec_happy[i])
  POST(url = URL_temp, add_headers(Authorization = hvmodify, scope = 'playlist-modify-public'
  ))
}


#now feelsy songs

vec_sad <- as.vector(notashappy_songs$id)

for(i in 1:length(vec_sad)) {
  URL_temp <- paste0('https://api.spotify.com/v1/users/', userID, 
                     '/playlists/', playlistID_sad, '/tracks?uris=spotify%3Atrack%3A', vec_sad[i])
  POST(url = URL_temp, add_headers(Authorization = hvmodify, scope = 'playlist-modify-public'
  ))
}



