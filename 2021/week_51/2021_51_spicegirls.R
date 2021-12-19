# Load packages -----------------------------------------------------------
library(pacman)
p_load(tidyverse, tidytuesdayR, showtext, ggtext, patchwork, cowplot)

font_add_google("Lato")
showtext_auto()


# Load data ---------------------------------------------------------------
tt <- tt_load('2021-12-14')
studio_album_tracks <- tt$studio_album_tracks

studio_album_tracks |> glimpse()

df_dance <- studio_album_tracks |> 
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


# Basic plot --------------------------------------------------------------


plot <- df_dance |> 
  ggplot(aes(track_all_number, danceability, fill = group)) +
  geom_col() +
  ylim(-.75, 1.2) +
  coord_polar(start = 0)
  

plot <- plot +
  labs(x = NULL,
       y = NULL,
       title = "Spice Girls",
       subtitle = "The most danceable tracks by album",
       caption = "Visualization by Pablo Alvarez • Data from TidyTuesday | 2021 - Week 51") +
  scale_fill_manual(values = c("#00028C", "#FFEF78", "#47D0BD", "grey85")) +
  theme_minimal() +
  theme(text = element_text(family = "Lato"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.position = "none",
        # Customize title appearance
        plot.title = element_text(
          color = "grey25", 
          size = 20, 
          face = "bold"
        ),
        # Customize subtitle appearance
        plot.subtitle = element_text(
          color = "grey50",
          size = 12,
          margin = margin(t = 5, b = 5)
        ),
        # Title and caption are going to be aligned
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption = element_text(
          color = "grey30", 
          size = 8,
          hjust = 0,
          margin = margin(t = 40) # Large margin on the top of the caption.
        ) 
  )



plot <- ggdraw(plot) +
  draw_image("PngItem_195841.png",
             x = .5, y = .5013,
             hjust = .5, vjust = .5,
             width = 0.25, height = 0.25)

# Table -------------------------------------------------------------------

df_dance |> glimpse()
  
plot2 <- df_dance |> 
  ggplot(aes(y = as.numeric(track_all_number), color = group)) +
  geom_text(x = 1.5, label = df_dance$album_release_year,
           size = 2, family = "Lato", hjust = 0) +
  geom_text(x = 2, label = df_dance$album_name,
            size = 2, family = "Lato", hjust = 0) +
  geom_text(x = 2.5, label = df_dance$track_name,
            size = 2, family = "Lato", hjust = 0) +
  geom_text(x = 3.5, label = df_dance$danceability,
            size = 2, family = "Lato", hjust = 0) +
  annotate("text", x = 1.5, y = 32, label = "Release year",
           size = 2, family = "Lato", hjust = 0) +
  annotate("text", x = 2, y = 32, label = "Album",
           size = 2, family = "Lato", hjust = 0) +
  annotate("text", x = 2.5, y = 32, label = "Track",
           size = 2, family = "Lato", hjust = 0) +
  annotate("text", x = 3.5, y = 32, label = "Danceability",
           size = 2, family = "Lato", hjust = 0.5) +
  scale_x_continuous(limits = c(1, 4)) +
  scale_y_continuous(limits = c(1, 36)) +
  scale_color_manual(values = c("#00028C", "#FFEF78", "#47D0BD", "grey85")) +
  theme_void() +
  theme(
    text = element_text(family = "Lato"),
    legend.position = "none"
  )


plot | plot2 

ggsave("tidytuesday_2021_w49.png", width = 12, height = 10, units = "in", dpi = 320)


