install.packages('flexdashboard')
install.packages("plotly")
install.packages("Cairo")
library(flexdashboard)
library(plotly)
library(ggplot2)
library(tidyverse)
library(Cairo)
library(spotifyr)

#  .tabset-fade
 # theme: "united"

topnummers %>%
  rename(Your Top Songs 2021 = JTNV2020)

id <- "81f3df676d604409abfe7d3452016983"
secret <- "4dad5e4ae74e48849e41af9b3bc941c2"
Sys.setenv(SPOTIFY_CLIENT_ID = id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = secret)

JTNV2020 <- get_playlist_audio_features("", "37i9dQZF1ELZEbCiN09pZG?si=eace51fcd9ac4855")
JTNV2020

JTNV2021 <- get_playlist_audio_features("", "4uti3gC6MRxJFVu3P6Zq09?si=261c2fc1668d4b99")
JTNV2021

topnummers <- bind_rows(
  JTNV2020 %>% mutate(category = "JTNV2020"),
  JTNV2021 %>% mutate(category = "JTNV2021")
)






topnummers %>%
  ggplot(aes(x = energy)) + geom_histogram(binwidth = 0.1) +
  facet_wrap(~category)

topnummers %>% 
  ggplot(aes(x = valence, y = energy)) + geom_point() + geom_smooth()


energy_valence = topnummers %>% 
  mutate(
    mode = ifelse(mode == 0, "Minor", "Major")
  ) %>%
  ggplot( 
    aes(
      x = valence,
      y = energy,
      size = loudness,
      colour = mode
    )
  ) +
  geom_point() +             
  geom_rug(size = 0.1) +     
  geom_text(                 
    aes(
      x = valence,
      y = energy,
      label = label
    ),
    data = 
      tibble(
        label = c(".", "."),
        category = c("JTNV2020", "JTNV2021"),
        valence = c(0.0605, 0.9600),
        energy = c(0.101, 0.967)
      ),
    colour = "black",         
    size = 3,                
    hjust = "left",          
    vjust = "bottom",     
    nudge_x = -0.05,        
    nudge_y = 0.02      
  ) +
  facet_wrap(~category) +     
  scale_x_continuous(      
    limits = c(0, 1),
    breaks = c(0, 0.50, 1),  
    minor_breaks = NULL      
  ) +
  scale_y_continuous(        
    limits = c(0, 1),
    breaks = c(0, 0.50, 1),
    minor_breaks = NULL
  ) +
  scale_colour_brewer(       
    type = "qual",          
    palette = "Paired"      
  ) +
  scale_size_continuous(     
    trans = "exp",           
    guide = "none"          
  ) +
  theme_light() +           
  labs(                  
    x = "Valence",
    y = "Energy",
    colour = "Mode"
  )

ggplotly(energy_valence)




```{r}
library(flexdashboard)
library(plotly)
library(ggplot2)
library(tidyverse)
library(spotifyr)
```

```{r}
id <- "81f3df676d604409abfe7d3452016983"
secret <- "4dad5e4ae74e48849e41af9b3bc941c2"
Sys.setenv(SPOTIFY_CLIENT_ID = id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = secret)

```{r}
id <- "81f3df676d604409abfe7d3452016983"
secret <- "4dad5e4ae74e48849e41af9b3bc941c2"
Sys.setenv(SPOTIFY_CLIENT_ID = id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = secret)

```
```{r}
JTNV2020 <- get_playlist_audio_features("", "37i9dQZF1ELZEbCiN09pZG?si=eace51fcd9ac4855")
```

JTNV2021 <- get_playlist_audio_features("", "4uti3gC6MRxJFVu3P6Zq09?si=780acfc809e64cb1")

tophisto = topnummers %>%
  ggplot(aes(x = energy)) + geom_histogram(binwidth = 0.1) +
  facet_wrap(~category)

topviolin = topnummers %>%
  ggplot(aes(x = category, y = energy)) +
  geom_violin()

topsmooth = topnummers %>% 
  ggplot(aes(x = valence, y = energy)) + geom_point() + geom_smooth()


remotes::install_github('jaburgoyne/compmus')

library(tidyverse)
library(spotifyr)
library(compmus)

wood <-
  get_tidy_audio_analysis("6IQILcYkN2S2eSu5IHoPEH") %>%
  select(segments) %>%
  unnest(segments) %>%
  select(start, duration, pitches)

wood

whywhywhy <-
  get_tidy_audio_analysis("4zwq3QUKgMNk0NSLl7fpbP?si=dcd0036a52154ea5") %>%
  select(segments) %>%
  unnest(segments) %>%
  select(start, duration, pitches)

whywhywhy %>%
  mutate(pitches = map(pitches, compmus_normalise, "euclidean")) %>%
  compmus_gather_chroma() %>% 
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = pitch_class,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
  theme_minimal() +
  scale_fill_viridis_c()


chaka <-
  get_tidy_audio_analysis("0W8MpnhaImy0oPRkVpRvjI?si=a6fff186744843ec") %>%
  select(segments) %>%
  unnest(segments) %>%
  select(start, duration, pitches)

chaka %>%
  mutate(pitches = map(pitches, compmus_normalise, "euclidean")) %>%
  compmus_gather_chroma() %>% 
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = pitch_class,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
  theme_minimal() +
  scale_fill_viridis_c()


JTNV2020 %>%
  summarise(energy)


summary(JTNV2020$energy)

summary(JTNV2020$energy, JTNV2020$valence)


1


ggplot(topnummers, aes(x = energy, y = valence)) +
  geom_hex(bins = 25) + facet_wrap(~playlist_name, dir = "v")

ggplot(JTNV2021, aes(x = energy, y = valence)) +
  geom_hex(bins = 25)





More visualisation
====================
  
  Column{data-width=300}
------------------
  
  Correlation 2020 - Energy / Valence
```{r}
cor(JTNV2020$energy, JTNV2020$valence)
```

Correlation 2021 - Energy / Valence
```{r}
cor(JTNV2021$energy, JTNV2021$valence)
```


Energy valence The first graph shows the relation between energy and valence for both 2020 and 2021. I also calculated the correlation between these two variables. That calculation suggests that there is more correlation between energy and valence in the 2020 playlist than in the 2021 playlist. This is visually confirmed by the second chart, which in addition to energy and valence, also shows minor and major. The values seem to be more scattered in the 2021 graph.

Something I found interesting about this graph is that in the 2020 chart, minor songs generally score higher than major songs when it comes to energy. Something I think could explain this is that the top ten songs with the highest energy are all (hard)rock songs. These songs are often quite high in energy, even when they are in a minor key.

Another thing I found interesting about this graph is that the song with both the least energy and valence in the 2021 chart, is actually a song that does not really belong in the playlist. Last year I took a Musicological History course, for which we had to take a listening test, to prove that we were able to recognize a song by hearing it. One of the songs that I struggled with while studying was Symphony No. 5: IV. Adagietto. Sehr langsam by Gustav Mahler. This is the song with the lowest energy and valence. This means that it is not really a representative song, since I didn’t listen to it because I wanted to, but because I had to.


Column{data-width=700, .tabset .tabset-fade}
----------------
  
  ### Chart 3 {data-padding=10}
  
  ```{r}
topsmooth = topnummers %>% 
  ggplot(aes(x = valence, y = energy)) + geom_point() + geom_smooth() + 
  facet_wrap(~playlist_name, dir = "v")

ggplotly(topsmooth)
```


### Chart 4
```{r, echo=FALSE}

energy_valence = topnummers %>% 
  mutate(
    mode = ifelse(mode == 0, "Minor", "Major")
  ) %>%
  ggplot( 
    aes(
      x = valence,
      y = energy,
      size = loudness,
      colour = mode
    )
  ) +
  geom_point() +             
  geom_rug(size = 0.1) +     
  geom_text(                 
    aes(
      x = valence,
      y = energy,
      label = label
    ),
    data = 
      tibble(
        label = c(".", "."),
        category = c("JTNV2020", "JTNV2021"),
        valence = c(0.0605, 0.9600),
        energy = c(0.101, 0.967)
      ),
    colour = "black",         
    size = 3,                
    hjust = "left",          
    vjust = "bottom",     
    nudge_x = -0.05,        
    nudge_y = 0.02      
  ) +
  facet_wrap(~category) +     
  scale_x_continuous(      
    limits = c(0, 1),
    breaks = c(0, 0.50, 1),  
    minor_breaks = NULL      
  ) +
  scale_y_continuous(        
    limits = c(0, 1),
    breaks = c(0, 0.50, 1),
    minor_breaks = NULL
  ) +
  scale_colour_brewer(       
    type = "qual",          
    palette = "Paired"      
  ) +
  scale_size_continuous(     
    trans = "exp",           
    guide = "none"          
  ) +
  theme_light() +           
  labs(                  
    x = "Valence",
    y = "Energy",
    colour = "Mode"
  )

ggplotly(energy_valence)

```

