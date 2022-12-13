library(tidyverse)
library(RSelenium)
library(netstat)

# List chrome versions
binman::list_versions("chromedriver")

# Start the server
rs_driver_object <- rsDriver(browser = "chrome",
                             chromever = "108.0.5359.71",
                             verbose = FALSE,
                             port = netstat::free_port())

# Create a client object
remDr <- rs_driver_object$client

# Open a browser
remDr$open()

url <- "https://www.siriusxm.com/channels/watercolors"

# Open siruisxm website
remDr$navigate(url)

# Find song name element
song <- remDr$findElement(using = "class name", "card-spotlight--now-playing--song")


# Find artist name element
artist <- remDr$findElement(using = "class name", "card-spotlight--now-playing--artist")


# Create empty dataframe
songs <- 
  data.frame(time_stamp = character(),
             song = character(),
             artist = character())

# Setup counter
song_count <- 0

while (song_count < 2) {
  
  # Get song name
  song_name <- 
    song$getElementText() |> 
    unlist()
  
  # Get artist name
  artist_name <- 
    artist$getElementText() |> 
    unlist()
  
  # Save data in dataframe
  songs[song_count + 1, "time_stamp"] <- Sys.time() |> as.character()
  songs[song_count + 1, "song"] <- song_name
  songs[song_count + 1, "artist"] <- artist_name
  
  # increase counter if the song is new
  if (length(songs$song) == 1) {
    song_count <- song_count + 1
  } else if (song_name != songs[song_count, "song"]) {
    song_count <- song_count + 1
  }
  
  # Sleep so that R isn't constantly running
  Sys.sleep(60 * 0.5)
}



# terminate the selenium server

