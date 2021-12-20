# Load packages -----------------------------------------------------------
library(pacman)
p_load(tidyverse, tidytuesdayR, showtext, ggtext, patchwork, cowplot, lubridate)

font_add_google(c("Monoton", "Lato"))
showtext_auto()


# Load data ---------------------------------------------------------------
tt <- tt_load('2021-12-14')
studio_album_tracks <- tt$studio_album_tracks

studio_album_tracks |> glimpse()

  
  df_dance <- studio_album_tracks |>
    arrange(album_release_year) |> 
    select(album_release_year,album_name,track_name, track_number, danceability) |> 
    group_by(album_name) |> 
    arrange(desc(danceability), .by_group = TRUE) |> 
    arrange(album_release_year) |> 
    mutate(danceability_max = if_else(danceability == max(danceability), 1, 2),
           group = factor(case_when(album_name == "Spice" & danceability_max == 1 ~ 1,
                                    album_name == "Spiceworld" & danceability_max == 1 ~ 2,
                                    album_name == "Forever" & danceability_max == 1 ~ 3,
                                    TRUE ~ 4),
                          levels = c(1:4), ordered = TRUE)) |> 
    ungroup() |> 
    mutate(album_name = factor(album_name),
           track_all_number = factor(1:n())) 

  # Reverse order of 'track_all_number' so they appear in order in the second plot
df_dance <- df_dance |> 
  arrange(desc(album_release_year)) |> 
  group_by(album_name) |> 
  arrange(desc(danceability), .by_group = TRUE) |> 
  ungroup() |> 
  arrange(album_release_year) |>
  mutate(track_all_number_reversed = 1:n()) |> 
  arrange(desc(track_all_number_reversed)) |> 
  mutate(track_all_number_reversed = 1:n())
    
    

# Basic plot --------------------------------------------------------------
plot <- df_dance |>  
  ggplot(aes(x = track_all_number, y = danceability, fill = group)) +
  geom_col() +
  ylim(-.9, 1) +
  coord_polar(start = 0) +
  scale_fill_manual(values = c("#00028C", "#06FF00", "#FFF338", "grey85")) 
    

plot <- plot +
  labs(x = NULL,
       y = NULL,
       title = "SPICE GIRLS",
       subtitle = "The most danceable tracks by album",
       caption = "Visualization by Pablo Alvarez • Data from Spotify • TidyTuesday | 2021 - Week 51\nDanceability ranges from 0.0 to 1.0, with 1.0 being the most danceable.") +
  theme_void() +
  theme(text = element_text(family = "Lato"),
        plot.background = element_rect(fill = "#FF87CA", color = "#FF87CA"),
        panel.background = element_rect(fill = "#FF87CA", color = "#FF87CA"),
        legend.position = "none",
        plot.title = element_text(
          family = "Monoton",
          color = "black", 
          size = 40, 
          face = "bold",
          margin = margin(t = 20),
          hjust = 0.5
        ),
        plot.subtitle = element_text(
          color = "black",
          size = 12,
          margin = margin(t = 10),
          hjust = 0.5
        ),
        plot.caption = element_text(
          color = "grey30", 
          size = 8,
          margin = margin(b = 20),
          hjust = 0.5
        ) 
  )


# Table -------------------------------------------------------------------
df_dance |> glimpse()
  
plot2 <- df_dance |> 
  ggplot(aes(y = as.numeric(track_all_number_reversed), color = group)) +
  geom_text(x = 1.5, label = df_dance$album_release_year,
           size = 2.8, family = "Lato", hjust = 0.5,
           fontface = if_else(df_dance$group %in% c(1, 2, 3),  "bold", "plain")) +
  geom_text(x = 2, label = df_dance$album_name,
            size = 2.8, family = "Lato", hjust = 0.5,
            fontface = if_else(df_dance$group %in% c(1, 2, 3),  "bold", "plain")) +
  geom_text(x = 2.3, label = df_dance$track_name,
            size = 2.8, family = "Lato", hjust = 0,
            fontface = if_else(df_dance$group %in% c(1, 2, 3),  "bold", "plain")) +
  geom_text(x = 3.7, label = df_dance$danceability,
            size = 2.8, family = "Lato", hjust = 0.5,
            fontface = if_else(df_dance$group %in% c(1, 2, 3),  "bold", "plain")) +
  annotate("text", x = 1.5, y = 32.5, label = "Release year",
           size = 3.2, family = "Lato", hjust = 0.5, fontface = "bold", color = "white") +
  annotate("text", x = 2, y = 32.5, label = "Album",
           size = 3.2, family = "Lato", hjust = 0.5, fontface = "bold", color = "white") +
  annotate("text", x = 2.3, y = 32.5, label = "Track",
           size = 3.2, family = "Lato", hjust = 0, fontface = "bold", color = "white") +
  annotate("text", x = 3.7, y = 32.5, label = "Danceability",
           size = 3.2, family = "Lato", hjust = 0.5, fontface = "bold", color = "white") +
  scale_x_continuous(limits = c(1, 4)) +
  scale_y_continuous(limits = c(1, 36)) +
  scale_color_manual(values = c("1" = "#00028C", "2" = "#06FF00",
                               "3" = "#FFF338", "4" = "grey85"))  +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#FF87CA", color = "#FF87CA"),
    panel.background = element_rect(fill = "#FF87CA", color = "#FF87CA"),
    text = element_text(family = "Lato"),
    legend.position = "none"
  )

theme_border <- theme_void() + 
  theme(plot.background = element_rect(fill = "#FF87CA", color = '#FF87CA'))

plot_final <- plot | plot2 +
  plot_annotation(theme = theme_border)


# Final plot --------------------------------------------------------------
  ggdraw(plot_final) +
  draw_image("spice_girls.png",
             x = 1, y = .45,
             hjust = 4.2, vjust = .45,
             width = 0.2, height = 0.2)

ggsave("tidytuesday_2021_w49.png", width = 10, height = 8, units = "in", dpi = 320)


