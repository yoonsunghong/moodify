Clustering, Analyzing, and Curating Spotify Playlist Songs
================
Yoon Sung Hong
08/06/2017

1. Introduction
---------------

Personally, I create monthly playlists to get a chance to add as many new songs to my playlists as possible. Since I listen to music from different artists and genres, the monthly playlists often become a scramble of unhomogeneous collection of music. So I thought: what if I could find a way to group the songs and add them each into their own categorized playlists?

I used R to scrape the song details from the Spotify API, cluster the songs, verify that the clusters are valid, and add the songs into previously created playlists through Spotify API.

Let's load up some packages before starting. I used `httr` package to work with HTTP methods to utilize the Spotify API, and `dplyr` package to filter and clean the data. I used `NbClust` package the cluster the songs and finally, I used `ggplot2` and `scatterplot3d` to visualize the data and statistical analysis.

``` r
#loading packages
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(httr)
library(NbClust)
library(ggplot2)
library(scatterplot3d)
```

2. Importing and handling the song data using Spotify API
---------------------------------------------------------

To get the song data from your own playlist, you need 5 following things:
+ Client ID
+ Secret
+ Playlist ID
+ User ID
+ Access Token

Client ID and Secret can be obtained by registering your App through *Spotify for Developers.*
Playlist ID and User ID are your personal playlist ID and Spotify user ID. Those can be found on your Spotify application.
Access token is required to gain the access to the playlist songs' information as well as to add clustered songs into designated playlists at the end.

Creating different object names for each of the 5 variables listed above, I needed to set up a few more things such as the URL before importing any song data. I also used POST method with the client ID and secret to get access token for song information purposes.

``` r
hvmodify <- paste0('Bearer ', token)
#setting url for playlist
URL_playlist <- paste0('https://api.spotify.com/v1/users/', userID, 
                       '/playlists/', playlistID, '/tracks')
#POST method for access token
response_1 <- POST(
  'https://accounts.spotify.com/api/token',
  accept_json(),
  authenticate(clientID, secret),
  body = list(grant_type = 'client_credentials'),
  encode = 'form',
  verbose()
)
mytoken <- content(response_1)$access_token
#token
hv <- paste0('Bearer ', mytoken)
```

Now, we can use the URL of the playlist along with the token to gain data for each of the songs in the playlist.

``` r
#fetching the info
response_playlist <- GET(url = URL_playlist, add_headers(Authorization = hvmodify))
playlist <- content(response_playlist)
```

Now we have the data regarding our playlist under the object name `playlist`, including some data for each of the songs in the playlist. However, the data we obtained is in a format of nested list. After some exploration, I learned that each of the song and its information is contained in `playlist$items[[i]]`, where i is the index number of the track in the playlist.

Now that we know this, we can iterate through the playlist object to obtain audio feature data for each of the song. Let's first set up a data frame with the first song in the playlist, then bind to this setup data frame each time with the iteration.

``` r
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
```

Now we can use iteration, minus the first song, to get a full dataframe of audio features and information for every song in the playlist.

``` r
#iterating through the playlist data
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
```

Let's do some basic exploration to see what the data frame contains now.

``` r
str(songs)
```

    ## 'data.frame':    50 obs. of  20 variables:
    ##  $ danceability    : num  0.602 0.643 0.668 0.626 0.538 0.689 0.811 0.671 0.611 0.638 ...
    ##  $ energy          : num  0.601 0.657 0.515 0.836 0.626 0.338 0.57 0.195 0.351 0.708 ...
    ##  $ key             : int  9 10 7 7 5 11 5 9 3 1 ...
    ##  $ loudness        : num  -8.52 -4.22 -6.03 -5.96 -8.27 ...
    ##  $ mode            : int  1 0 0 0 1 0 1 0 0 0 ...
    ##  $ speechiness     : num  0.0411 0.0312 0.25 0.0406 0.0455 0.0629 0.129 0.0526 0.207 0.0754 ...
    ##  $ acousticness    : num  0.0367 0.064 0.046 0.103 0.115 0.349 0.253 0.972 0.245 0.0973 ...
    ##  $ instrumentalness: num  4.59e-03 4.51e-05 0.00 0.00 1.00e-05 5.70e-05 0.00 5.50e-01 1.11e-03 0.00 ...
    ##  $ liveness        : num  0.312 0.448 0.17 0.129 0.137 0.0649 0.148 0.0791 0.246 0.177 ...
    ##  $ valence         : num  0.858 0.48 0.64 0.683 0.241 0.568 0.814 0.639 0.184 0.606 ...
    ##  $ tempo           : num  122 105 86.5 92 132.9 ...
    ##  $ type            : Factor w/ 1 level "audio_features": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ id              : Factor w/ 50 levels "5EV4bGHxVN0kHpcAFvgnTt",..: 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ uri             : Factor w/ 50 levels "spotify:track:5EV4bGHxVN0kHpcAFvgnTt",..: 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ track_href      : Factor w/ 50 levels "https://api.spotify.com/v1/tracks/5EV4bGHxVN0kHpcAFvgnTt",..: 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ analysis_url    : Factor w/ 50 levels "https://api.spotify.com/v1/audio-analysis/5EV4bGHxVN0kHpcAFvgnTt",..: 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ duration_ms     : int  198000 223974 211712 192000 243235 320654 251250 165013 220362 201627 ...
    ##  $ time_signature  : int  4 4 4 4 4 4 4 4 5 4 ...
    ##  $ trackname       : chr  "Dance, Baby!" "Fucked Up (feat. Daniel Caesar)" "I Got You ◑" "Worry No More - Keys N Krates Remix" ...
    ##  $ trackartist     : chr  "boy pablo" "Jiin" "HONNE" "Diplo" ...

We can see that a lot of the variables we pulled are numerical variables, giving us an oppourtunity to do a statistical analysis with the data. A lot of these numerical variables also seem to be at a scale from 0 to 1. You can find more detailed description of each of the variables [here.](https://developer.spotify.com/documentation/web-api/reference/tracks/get-audio-features/)

3. Clustering and Principal Component Analysis
----------------------------------------------

Now that we have all the songs, let's try to see if we can group the songs by their audio features. For the clusters, I am making an assumption that if they have similar audio feature values, they are likely to be in the same musical color and genre.

I used `NbClust` package to determine the most appropriate number of clusters and to assign the cluster group for each of the songs. We can use the Ward.D method to minimize the total within-cluster variance. The Best.partition column from the NbClust output is the cluster assignment, so I also saved it as a separate variable.

``` r
#amending the data frame
songs_numbers <- songs[,1:11]
#excluding key and mode as they are not relevant values in the cluster analysis (numeric values not actually numerically representative)
#also excluding loudness, instrumentalness and speechiness as it doesn't seem like a relevant enough variable
songs_numbers <- subset(songs_numbers, select = -c(key, mode, loudness, speechiness, instrumentalness))
#clustering
nbclust <- NbClust(songs_numbers, distance = 'manhattan',
                   min.nc = 2, max.nc = 7,
                   method = 'ward.D', index = 'all')
```

![](spotify_playlist_files/figure-markdown_github/unnamed-chunk-8-1.png)

    ## *** : The Hubert index is a graphical method of determining the number of clusters.
    ##                 In the plot of Hubert index, we seek a significant knee that corresponds to a 
    ##                 significant increase of the value of the measure i.e the significant peak in Hubert
    ##                 index second differences plot. 
    ## 

![](spotify_playlist_files/figure-markdown_github/unnamed-chunk-8-2.png)

    ## *** : The D index is a graphical method of determining the number of clusters. 
    ##                 In the plot of D index, we seek a significant knee (the significant peak in Dindex
    ##                 second differences plot) that corresponds to a significant increase of the value of
    ##                 the measure. 
    ##  
    ## ******************************************************************* 
    ## * Among all indices:                                                
    ## * 2 proposed 2 as the best number of clusters 
    ## * 8 proposed 3 as the best number of clusters 
    ## * 4 proposed 4 as the best number of clusters 
    ## * 1 proposed 5 as the best number of clusters 
    ## * 3 proposed 6 as the best number of clusters 
    ## * 4 proposed 7 as the best number of clusters 
    ## 
    ##                    ***** Conclusion *****                            
    ##  
    ## * According to the majority rule, the best number of clusters is  3 
    ##  
    ##  
    ## *******************************************************************

``` r
#subsetting the cluster assignments
cluster_group <- nbclust$Best.partition
```

Since we have quite a few variables in consideration for the song, it might be useful to do principal component analysis to achieve dimensionality reduction. That way, it would be easier for us to visualize the data and see if clusters really do exist, and to what extent they are clumped together in their audio feature values (especially since PC1, PC2, and PC3 would often be enough number of variables to represent majority or more of all variables).

``` r
pca <- prcomp(songs_numbers, scale = T)
summary(pca)
```

    ## Importance of components:
    ##                           PC1    PC2    PC3    PC4     PC5     PC6
    ## Standard deviation     1.3957 1.1716 1.0566 0.9269 0.72578 0.42087
    ## Proportion of Variance 0.3247 0.2288 0.1861 0.1432 0.08779 0.02952
    ## Cumulative Proportion  0.3247 0.5534 0.7395 0.8827 0.97048 1.00000

``` r
#up to pc3 accounts for 85.4% of the variance, up to pc2 accounts for 65.5%
pca_df <- as.data.frame(pca$x)
#also adding cluster assignments as a column to the pca data frame, for visualization
pca_df$cluster <- as.character(cluster_group)
```

Above is the summary of the Principal Component Analysis done on the 6 numerical variables from the audio features. Here, we can see that more than half of the variance can be explained by PC1 and PC2, and nearly 3/4th of the variance is explained by PC1, PC2, and PC3. Let us now see if we can plot a 2D graph of PC1 and PC2 for each song and see if clusters do exist in the visualization.

``` r
ggplot(pca_df, aes(x = PC1, y = PC2, color = cluster)) + geom_point() + ggtitle("Clustered songs and their PC1, PC2 values")
```

![](spotify_playlist_files/figure-markdown_github/unnamed-chunk-10-1.png)

I think the clusters positions are still somewhat scattered. Let us try to bring in PC3 variable to see if the clusters become clearer.

``` r
#3d plot - cannot use plotly on 3.5.1 R version so using threejs packaage
colors <- c("#F8766D", "#00BA38", "#619CFF", "purple")
pca_df$colors <- rep(0,nrow(pca_df))
for(i in 1:nrow(pca_df)) {
  pca_df$colors[i] <- colors[cluster_group[i]]
}
scatterplot3d(pca_df$PC3,
                pca_df$PC2,
                pca_df$PC1,
                color=pca_df$colors,
                pch = 19,
                xlab = "PC3",
                ylab = "PC2",
                zlab = "PC1",
                main = "3D Scatterplot of PC1, PC2, PC3")
```

![](spotify_playlist_files/figure-markdown_github/unnamed-chunk-11-1.png)

Here we can see that the clusters are formed more noticeably. This verifies that the clustering method we used did manage to group the songs in relation to the audio features.

4. Adding the Clustered Songs into Spotify Playlists
----------------------------------------------------

Now that we've managed to create the clusters and verify that they are accurate clusters of multi-variables, we can add the songs into separate playlists. We can use the `httr` function again to do this. The token required for adding songs is of higher level of access, and therefore cannot be acquired through the `POST` method I used earlier. One can get the token access on the Spotify for Developers website.

``` r
#creating objects for playlist id
playlist_cluster1 <- '2eiPgxPpLZWraUDfZTuQUH'
playlist_cluster2 <- '5B6iTqPDh2hfy1WRrJ7THb'
playlist_cluster3 <- '6FzBLsv7nO8cz89Htuvk2g'
```

``` r
#creating clusters 
songs$cluster <- cluster_group
cluster1 <- songs %>% filter(cluster == 1)
cluster2 <- songs %>% filter(cluster == 2)
cluster3 <- songs %>% filter(cluster == 3)
#adding the songs by their clusters to designated playlists
for(song in 1:nrow(cluster1)) {
  URL_temp <- paste0('https://api.spotify.com/v1/users/', userID, 
                     '/playlists/', playlist_cluster1, '/tracks?uris=spotify%3Atrack%3A', cluster1[song, "id"])
  POST(url = URL_temp, add_headers(Authorization = hvmodify, scope = 'playlist-modify-public'
  ))
}
for(song in 1:nrow(cluster2)) {
  URL_temp <- paste0('https://api.spotify.com/v1/users/', userID, 
                     '/playlists/', playlist_cluster2, '/tracks?uris=spotify%3Atrack%3A', cluster2[song, "id"])
  POST(url = URL_temp, add_headers(Authorization = hvmodify, scope = 'playlist-modify-public'
  ))
}
for(song in 1:nrow(cluster3)) {
  URL_temp <- paste0('https://api.spotify.com/v1/users/', userID, 
                     '/playlists/', playlist_cluster3, '/tracks?uris=spotify%3Atrack%3A', cluster3[song, "id"])
  POST(url = URL_temp, add_headers(Authorization = hvmodify, scope = 'playlist-modify-public'
  ))
}
```

Now we have three playlists, each with all the songs from a cluster! You can have a listen here and see if it's a musically homogeneous playlist.

[Playlist 1](https://open.spotify.com/user/spider3003/playlist/2eiPgxPpLZWraUDfZTuQUH?si=Wkwx3K03RTaSjJuqX7ns5Q)
[Playlist 2](https://open.spotify.com/user/spider3003/playlist/5B6iTqPDh2hfy1WRrJ7THb?si=jGrfrsosS5OAEAFP2sE-Lg)
[Playlist 3](https://open.spotify.com/user/spider3003/playlist/6FzBLsv7nO8cz89Htuvk2g?si=BOIJg6TNT2OQ-RQzbcx4Uw)

Let's now see what the playlists' songs have in common, starting with the first cluster.

``` r
summary(cluster1[,1:11])
```

    ##   danceability        energy            key            loudness      
    ##  Min.   :0.3120   Min.   :0.0952   Min.   : 0.000   Min.   :-18.588  
    ##  1st Qu.:0.5320   1st Qu.:0.3478   1st Qu.: 1.000   1st Qu.:-10.016  
    ##  Median :0.6335   Median :0.4935   Median : 5.000   Median : -8.409  
    ##  Mean   :0.6280   Mean   :0.4800   Mean   : 4.714   Mean   : -8.751  
    ##  3rd Qu.:0.7645   3rd Qu.:0.6650   3rd Qu.: 8.250   3rd Qu.: -7.385  
    ##  Max.   :0.8720   Max.   :0.7300   Max.   :11.000   Max.   : -4.225  
    ##       mode         speechiness       acousticness     instrumentalness   
    ##  Min.   :0.0000   Min.   :0.02730   Min.   :0.01720   Min.   :0.0000000  
    ##  1st Qu.:0.0000   1st Qu.:0.04118   1st Qu.:0.06148   1st Qu.:0.0000000  
    ##  Median :1.0000   Median :0.05525   Median :0.28900   Median :0.0001045  
    ##  Mean   :0.6429   Mean   :0.12214   Mean   :0.36526   Mean   :0.0233966  
    ##  3rd Qu.:1.0000   3rd Qu.:0.14850   3rd Qu.:0.57875   3rd Qu.:0.0026550  
    ##  Max.   :1.0000   Max.   :0.40800   Max.   :0.97200   Max.   :0.5500000  
    ##     liveness         valence           tempo       
    ##  Min.   :0.0649   Min.   :0.0370   Min.   : 99.95  
    ##  1st Qu.:0.1070   1st Qu.:0.2400   1st Qu.:112.38  
    ##  Median :0.1615   Median :0.3405   Median :122.97  
    ##  Mean   :0.2450   Mean   :0.4162   Mean   :121.38  
    ##  3rd Qu.:0.3157   3rd Qu.:0.5627   3rd Qu.:131.50  
    ##  Max.   :0.9420   Max.   :0.9060   Max.   :140.10

We can see that the songs in the first cluster are: + In general, quite groovy (danceable)
+ Medium energy + Not very acoustic, nor live (these two tend to come together often at times) + Sad songs (low valence) + Lastly, relatively fast in tempo (most being between 112bpm and 131bpm).

The songs' commonalities and characteristics suggest that the songs are mostly pop, pop-influenced R&B, or R&B with faster tempo (like Drake's R&B songs). This hypothesis is backed by songs such as *TEAM* and *Dance, Baby!*.

Now let's have a look at the second cluster.

``` r
summary(cluster2[,1:11])
```

    ##   danceability        energy            key            loudness      
    ##  Min.   :0.4070   Min.   :0.1780   Min.   : 1.000   Min.   :-15.209  
    ##  1st Qu.:0.4953   1st Qu.:0.4920   1st Qu.: 2.000   1st Qu.: -7.769  
    ##  Median :0.5910   Median :0.5840   Median : 6.000   Median : -6.297  
    ##  Mean   :0.5922   Mean   :0.5703   Mean   : 5.562   Mean   : -6.901  
    ##  3rd Qu.:0.6703   3rd Qu.:0.6552   3rd Qu.: 8.250   3rd Qu.: -5.521  
    ##  Max.   :0.8460   Max.   :0.8360   Max.   :10.000   Max.   : -3.619  
    ##       mode         speechiness       acousticness     instrumentalness   
    ##  Min.   :0.0000   Min.   :0.02570   Min.   :0.00558   Min.   :0.0000000  
    ##  1st Qu.:0.0000   1st Qu.:0.04487   1st Qu.:0.06640   1st Qu.:0.0000000  
    ##  Median :0.0000   Median :0.06185   Median :0.15500   Median :0.0000253  
    ##  Mean   :0.4375   Mean   :0.10226   Mean   :0.28773   Mean   :0.0046666  
    ##  3rd Qu.:1.0000   3rd Qu.:0.11275   3rd Qu.:0.46125   3rd Qu.:0.0003535  
    ##  Max.   :1.0000   Max.   :0.31100   Max.   :0.86500   Max.   :0.0490000  
    ##     liveness         valence           tempo      
    ##  Min.   :0.0844   Min.   :0.0514   Min.   :68.69  
    ##  1st Qu.:0.1130   1st Qu.:0.2417   1st Qu.:80.65  
    ##  Median :0.1250   Median :0.4055   Median :84.94  
    ##  Mean   :0.1466   Mean   :0.4003   Mean   :83.57  
    ##  3rd Qu.:0.1573   3rd Qu.:0.5827   3rd Qu.:87.77  
    ##  Max.   :0.2820   Max.   :0.6860   Max.   :92.29

the songs in the second cluster are: + Groovier than the first cluster
+ Medium - high energy
+ Not very acoustic, nor live (these two tend to come together often at times)
+ Sad songs (low valence)
+ Slow in tempo (most being between 80bpm and 88bpm).

Judging from the categories, the songs in the second cluster are clearer in their musical characteristics than those of the first cluster. Judging by the slow tempo along with low valence, liveness, and acousticness values, I assume the cluster to contain the traditional R&B and slow hip hop songs.

Finally, the third cluster.

``` r
summary(cluster3[,1:11])
```

    ##   danceability        energy            key          loudness     
    ##  Min.   :0.3680   Min.   :0.4190   Min.   :1.00   Min.   :-9.242  
    ##  1st Qu.:0.4883   1st Qu.:0.4800   1st Qu.:1.75   1st Qu.:-8.123  
    ##  Median :0.5300   Median :0.6055   Median :6.50   Median :-6.715  
    ##  Mean   :0.5315   Mean   :0.5810   Mean   :5.50   Mean   :-6.847  
    ##  3rd Qu.:0.5890   3rd Qu.:0.6695   3rd Qu.:9.00   3rd Qu.:-5.521  
    ##  Max.   :0.6780   Max.   :0.7270   Max.   :9.00   Max.   :-4.694  
    ##       mode    speechiness      acousticness      instrumentalness   
    ##  Min.   :1   Min.   :0.0357   Min.   :0.000718   Min.   :0.000e+00  
    ##  1st Qu.:1   1st Qu.:0.0434   1st Qu.:0.090350   1st Qu.:4.908e-06  
    ##  Median :1   Median :0.0598   Median :0.345500   Median :1.620e-05  
    ##  Mean   :1   Mean   :0.1010   Mean   :0.313420   Mean   :1.894e-05  
    ##  3rd Qu.:1   3rd Qu.:0.0906   3rd Qu.:0.471500   3rd Qu.:2.982e-05  
    ##  Max.   :1   Max.   :0.3120   Max.   :0.670000   Max.   :4.570e-05  
    ##     liveness          valence           tempo      
    ##  Min.   :0.06440   Min.   :0.0577   Min.   :145.0  
    ##  1st Qu.:0.09002   1st Qu.:0.2502   1st Qu.:149.3  
    ##  Median :0.14650   Median :0.3075   Median :165.9  
    ##  Mean   :0.17552   Mean   :0.3566   Mean   :162.8  
    ##  3rd Qu.:0.19375   3rd Qu.:0.4975   3rd Qu.:173.5  
    ##  Max.   :0.41500   Max.   :0.6750   Max.   :180.1

This is by far the smallest cluster (with only 5 songs), so is more difficult to generalize due to its statistical significance. Nonetheless, the songs are similar in all their traits to songs in cluster 1 and 2, except the tempo. The tempo for songs in the third cluster are really high, ranging from 145 to 180.1. This was probably enough for a differentiating factor for these songs to be clustered on their own. Judging from the songs and the statistics, the cluster contains pop songs and high-tempo hiphop songs.

5. Conclusion and Reflections
-----------------------------

Overall, it was an interesting and fun experience to work with the Spotify API and see the possibilities and limitations with the API. I think the analysis would have been more effecitve had I had a more musically versatile playlist (with more genres), especially since I listen mostly to Indie, Hip-Hop, and R&B music. I also figured as I was working through this project that the API only allows you to work with 100 songs at a time, and I want to see if there is a way around this. It would be great to get a playlist with loads of songs and see if the R code can still do the same effective job in clustering the songs.
