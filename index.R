install.packages('flexdashboard')
install.packages("plotly")
install.packages("Cairo")
library(flexdashboard)
library(plotly)
library(ggplot2)
library(tidyverse)
library(Cairo)
library(spotifyr)

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




install.packages("compus")




