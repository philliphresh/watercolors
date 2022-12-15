library(tidyverse)

# Import all the CSV files as one dataframe

csv_path <- "Raw Song CSVs/"

csv_file_names <-
  csv_path %>%
  paste0(list.files(csv_path))

songs_raw <- do.call(rbind,lapply(csv_file_names,read_csv))

# Clean up data--remove duplicates and remove blanks
songs_clean <- 
  songs_raw %>% 
  distinct(song, artist, .keep_all = TRUE) %>% 
  na.omit()

# Just a quick check on artist count
songs_clean %>% 
  count(artist, sort = TRUE)

songs_clean %>% distinct(artist) %>% nrow() #222 unique artists
# out of...
(songs_clean %>% nrow()) # 338 songs
