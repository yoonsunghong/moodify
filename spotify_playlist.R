#As someone who has monthly playlists, 
#I always thought it'd be awesome to be able to have a master cumulative playlist, divided by moods of the songs.
#And this is an attempt to create something like that, with the help from Spotify API.

#loading package
library(httr)
library(dplyr)
library(NbClust)
library(ggplot2)
library(plotly)
#my own client ID and secret for the sake of the operation of the code.
clientID <- 'xxxxxxxxx'
secret <- 'xxxxxxxxx'

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
playlistID <- "xxxxxxxxx"
#my own userID here for the sake of the code running.
userID <- 'xxxxxxxxx'
#token
hv <- paste0('Bearer ', mytoken)
#different token used for authorization for POST. You have to go to 
#https://beta.developer.spotify.com/console/post-playlist-tracks/?user_id=3&playlist_id=&position=&uris=
#to get the token each time, because the token expires after certain time.
hvmodify <- paste0('Bearer ', "xxxxxxxxx")
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
track_url <- as.character(songs$track_href)
response_track <- GET(url = track_url, add_headers(Authorization = hv))
track <- content(response_track)
trackname <- track$name
trackartist <- track$artists[[1]]$name
songs$trackname <- trackname
songs$trackartist <- trackartist
for(i in 1:(playlist$total-1)) {
  songID <- playlist$items[[i+1]]$track$id
  URL_song <- paste0('https://api.spotify.com/v1/audio-features/', songID)
  response_song <- GET(url = URL_song, add_headers(Authorization = hv))
  song <- as.data.frame(content(response_song))
  #adding song title and artist name for the information 
  track_url <- as.character(song$track_href)
  response_track <- GET(url = track_url, add_headers(Authorization = hv))
  track <- content(response_track)
  trackname <- track$name
  trackartist <- track$artists[[1]]$name
  #binding the track name and artist name to the song vector before binding
  song$trackname <- trackname
  song$trackartist <- trackartist
  #merging it to create a mastersheet
  songs <- rbind(songs, song)
}
#clustering the songs
#amending the data frame
songs_numbers <- songs[,1:10]
#excluding key and mode as they are not relevant values in the cluster analysis (numeric values not actually numerically representative)
#also excluding loudness, instrumentalness and speechiness as it doesn't seem like a relevant enough variable
songs_numbers <- subset(songs_numbers, select = -c(key, mode, loudness, speechiness, instrumentalness))
nbclust <- NbClust(songs_numbers, distance = 'manhattan',
                   min.nc = 2, max.nc = 10,
                   method = 'ward.D', index = 'all')
#Conclusion - best number of clusters is 3
#subsetting the cluster assignments
cluster_group <- nbclust$Best.partition
pca <- prcomp(songs_numbers, scale = T)
summary(pca)
#up to pc3 accounts for 84.4% of the variance, up to pc2 accounts for 65.1%
pca_df <- as.data.frame(pca$x)
pca_df$cluster <- as.character(cluster_group)
ggplot(pca_df, aes(x = PC1, y = PC2, color = cluster)) + geom_point() + ggtitle("Clustered songs and their PC1, PC2 values")
#3d plot 

