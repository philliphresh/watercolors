library(tidyverse)
library(RSelenium)
library(netstat)

# Create folder to store CSVs
# dir.create("Raw Song CSVs")

# List chrome versions
binman::list_versions("chromedriver")

# Note I tried running this code in 2023-04-29 and had issues getting it to work. After a lot of head scratching I ultimately got it to work after deleting the license files from the folders here 
# /Users/Phillip/Library/Application Support/binman_chromedriver/mac64
# I learned to to this from watching Samer Hijjazi's video here
# https://www.youtube.com/watch?v=GnpJujF9dBw&t=10s
# 
# You can run this to see where the chrome drivers are stored on your computer
# selenium_object <- selenium(retcommand = TRUE, check = FALSE)
# selenium_object

# Start the server
rs_driver_object <- rsDriver(browser = "chrome",
                             chromever = "112.0.5615.49",
                             verbose = FALSE,
                             port = netstat::free_port()
                             )

# Create a client object
remDr <- rs_driver_object$client

# Open a browser
remDr$open()

url <- "https://www.siriusxm.com/channels/watercolors"

# Open siruisxm website
remDr$navigate(url)

# I really need to make the window half-size to be able to collect the text
remDr$setWindowSize(width = 600, height = 731)




# Create empty dataframe
songs <- 
  data.frame(time_stamp = character(),
             song = character(),
             artist = character())

# Setup counter
song_count <- 0

while (song_count < 300) {
  
  # Find song name element
  song <-
    remDr$findElement(using = "class name", "card-spotlight--now-playing--song")
  
  # Get song name
  song_name <-
    song$getElementText() |>
    unlist()
  
  # Find artist name element
  artist <-
    remDr$findElement(using = "class name", "card-spotlight--now-playing--artist")
  
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
  
  # Sleep for 30 seconds so that the while loop isn't constantly running
  Sys.sleep(60 * 0.5)
  
  # Refresh the page every 5 songs
  if (length(songs$song) / 5 - (length(songs$song) / 5) == 0) {
    remDr$refresh()
  }
}

# Export data as a CSV
write_csv(songs, 
          paste0("Raw Song CSVs/",
                 "watercolors", Sys.time(), ".csv"))


# terminate the selenium server
remDr$closeWindow()
remDr$closeServer()
rs_driver_object$server$stop()
