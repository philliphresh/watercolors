library(tidyverse)
library(spotifyr)

# set credentials
# I put my actual client id and client secret in the console
# Sys.setenv(SPOTIFY_CLIENT_ID = 'xxxxxxxxxxxxxxxxxxxxx')
# Sys.setenv(SPOTIFY_CLIENT_SECRET = 'xxxxxxxxxxxxxxxxxxxxx')

access_token <- get_spotify_access_token()

# Search for a song
test_search_results <-
  search_spotify(q = "track:Morning Breeze artist:Jazz Holdouts",
                 type = "track",
                 limit = 1)

test_search_results$uri

# Get URIs of songs
# songs_clean <- read.csv()

search_input <- 
  songs_clean %>% 
  mutate(artist = str_replace_all(artist, "/", ", ")) %>% 
  mutate(query = paste0("track:", song, " ",
                        "artist:", artist))

test_search_results <- 
  search_input %>% 
  head(3) %>% 
  rowwise() %>% 
  mutate(search_results = search_spotify(query,
                                         type = "track",
                                         limit = 1))

test_search_results$search_results$uri

# Start by creating an empty playlist
# (I had a lot of issues getting the authentication to work here
# I ended up needing to set my Redirect URI to http://localhost:1410/
# in the Spotify developer dashboard.)
create_playlist(
  user_id = "philliphresh",
  name = "WatercoloRs",
  description = "Songs played on Watercolors 66",
  authorization = 
    get_spotify_authorization_code(scope = "playlist-modify-public")
)

# Add tracks to playlist
