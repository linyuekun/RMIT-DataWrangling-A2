install.packages("outliers")
install.packages("splitstackshape")
install.packages("editrules")
install.packages("lubridate")
install.packages("forecast")

library(readr)
library(tidyr)
library(dplyr)
library(outliers)
library(ggplot2)
library(stringr)
library(splitstackshape)
library(editrules)
library(lubridate)
library(outliers)
library(forecast)

# Data ----

unpopular_songs <- read_csv("unpopular_songs.csv")
genre <- read_csv("z_genre_of_artists.csv")

head(unpopular_songs)
head(genre)

unpopular_songs %>% count()
genre %>% count()

new_unpopular_songs <- unpopular_songs %>% unique()
new_genre <- genre %>% distinct()

new_unpopular_songs %>% count()
new_genre %>% count()

songs <- unpopular_songs %>% left_join(new_genre, by=c("track_artist"="artist_name"))

# Understand ----

str(songs)

songs$popularity <- factor(songs$popularity,
                           levels=c("0", "1", "2", "3", "4", "5", "6", "7", 
                                    "8", "9", "10", "11", "12", "13", "14", 
                                    "15", "16", "17", "18"), 
                           ordered = TRUE)
class(songs$popularity)

str(songs$mode)

songs$mode <- factor(songs$mode,
                     levels=c("0","1"),
                     labels=c("Major","Minor"))

levels(songs$mode)

# Tidy & Manipulate Data I ----

songs$genre <- str_sub(songs$genre, start=2L, end=nchar(songs$genre)-1L)

songs <- cSplit(songs, "genre", ",", direction = "long")

songs$genre <- str_sub(songs$genre, start=2L, end=nchar(songs$genre)-1L)

songs_selected <- songs %>% select(popularity, track_name, track_artist, duration_ms, mode, genre)
str(songs_selected)

songs_selected <- songs_selected %>% arrange(popularity, track_name)

unpopular_genre <- songs_selected %>% 
  filter(popularity == "0") %>% 
  group_by(genre) %>% 
  summarise(Number_of_most_unpopular_songs = n()) %>%
  arrange(desc(Number_of_most_unpopular_songs)) %>% 
  drop_na()

head(unpopular_genre, n = 10)

# Tidy & Manipulate Data II ----

songs_selected %<>% 
  mutate(songs_selected, duration_min = round(seconds_to_period(songs_selected$duration_ms/1000),2)) %>%
  select (-duration_ms)

# Scan I ----

colSums(is.na(songs_selected))

songs_selected$genre <- replace_na(songs_selected$genre, "uncategorised")

colSums(is.na(songs_selected))

is.specialorNA <- function(x) {
  if(is.numeric(x)) {sum(is.infinite(x) | is.nan(x) | is.na(x))} 
  else {sum(is.na(x))}
}

sapply(songs_selected, function(x) {
  if(is.numeric(x)) {sum(is.infinite(x) | is.nan(x) | is.na(x))} 
  else {sum(is.na(x))}
})

(Rule1 <- editset(c("duration_min > 0")))
sum(violatedEdits(Rule1, songs_selected))

# Scan II ----

songs_selected$duration_min %>% 
  period_to_seconds() %>% 
  boxplot(main="Boxplot of Songs' Duration", 
          ylab="Duration in seconds", 
          col = "blue")

songs_selected$duration_min %>% 
  period_to_seconds() %>% 
  hist(main="Histogram of Songs' Duration", xlab="Duration in seconds",
       breaks = 100)

songs_selected$duration_min %>% 
  period_to_seconds() %>% 
  hist(main = "Histogram of Songs' Duration", xlim=c(0,700), breaks=100)

z.scores <- songs_selected$duration_min %>% period_to_seconds() %>% scores(type = "z")
z.scores %>% summary()
which(abs(z.scores) >3 )
length (which(abs(z.scores) >3 ))
  
# Transform ----

songs_selected$duration_min %>% period_to_seconds() %>%
  hist(main="Histogram of Sqrt Songs' Duration", xlab="Duration in seconds", breaks=100)

songs_selected$duration_min %>% period_to_seconds() %>% sqrt() %>%
  hist(main="Histogram of Sqrt Songs' Duration", xlab="Duration in seconds", breaks=100)

songs_selected$duration_min %>% period_to_seconds() %>% log10() %>%
  hist(main="Histogram of Log10 Songs' Duration", xlab="Duration in seconds", breaks=100)

Boxcox_duration <- songs_selected$duration_min %>% 
  period_to_seconds() %>% 
  BoxCox(lambda = "auto")

Boxcox_duration %>% tail(n = 30)

hist(Boxcox_duration)
