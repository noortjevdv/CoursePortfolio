# Dancibility 

viz4 <- ggplot(topnummers, aes(x=danceability, fill=playlist_name,
                            text = paste(playlist_name)))+
  geom_density(alpha=0.4, color=NA)+
  scale_fill_manual(values=c("blue", "orange"))+
  labs(x="Danceability", y="Density") +
  guides(fill=guide_legend(title="Playlist"))+
  theme_minimal()+
  ggtitle("Distribution of Danceability Data")

ggplotly(viz4, tooltip=c("text"))


glimpse(topnummers)

help(spotifyr)

test = get_track_audio_features(
  "", "4uti3gC6MRxJFVu3P6Zq09?si=261c2fc1668d4b99")

cor(JTNV2020$energy, JTNV2020$valence)
