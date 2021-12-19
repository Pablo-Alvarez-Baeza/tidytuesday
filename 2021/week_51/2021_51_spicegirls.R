# Load packages -----------------------------------------------------------
library(pacman)
p_load(tidyverse, tidytuesdayR, showtext, ggtext)

font_add_google("Lato")
showtext_auto()


# Load data ---------------------------------------------------------------
tt <- tt_load('2021-12-14')
studio_album_tracks <- tt$studio_album_tracks

studio_album_tracks |> View()
studio_album_tracks |> glimpse()

dance_df <- studio_album_tracks |> 
  arrange(album_release_year) |>
  select(album_release_year,album_name,track_name, track_number, danceability) |> 
  mutate(album_name = factor(album_name),
         track_all_number = factor(1:n())) |>
  relocate(track_all_number, .after = track_number) |> 
  group_by(album_name) |> 
  mutate(danceability_max = if_else(danceability == max(danceability), 1, 2)) |> 
                                  
  ungroup() |> 
  mutate(group = factor(case_when(album_name == "Spice" & danceability_max == 1 ~ 1,
                                  album_name == "Spiceworld" & danceability_max == 1 ~ 2,
                                  album_name == "Forever" & danceability_max == 1 ~ 3,
                                  TRUE ~ 4)))

dance_df |> View()

# Basic plot --------------------------------------------------------------


plot <- dance_df |> 
  ggplot(aes(track_all_number, danceability, fill = group)) +
  geom_col() +
  ylim(-.25, 1.2) +
  coord_polar(start = 0)
  

plot +
  labs(x = NULL,
       y = NULL) +
  scale_fill_manual(values = c("pink", "yellow", "green", "grey90")) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.position = "none"
  )
  

# Libraries
library(tidyverse)

# Create dataset
data <- data.frame(
  id=seq(1,60),
  individual=paste( "Mister ", seq(1,60), sep=""),
  value=sample( seq(10,100), 60, replace=T)
)

# Make the plot
 ggplot(data, aes(x=as.factor(id), y=value)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  # This add the bars with a blue color
  geom_bar(stat="identity", fill=alpha("blue", 0.3)) +
  
  # Limits of the plot = very important. The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
  ylim(-100,200) +
   coord_polar(start = 0) +
  # Custom the theme: no axis title and no cartesian grid
  theme(
    panel.background = element_rect(color = "black"),
    panel.grid = element_blank()# This remove unnecessary margin around plot
  ) 
  
  # This makes the coordinate polar instead of cartesian.
  
p
