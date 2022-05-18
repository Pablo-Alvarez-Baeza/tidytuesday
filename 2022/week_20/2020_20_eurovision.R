library(pacman)
p_load(tidyverse, camcorder, showtext, ggtext)

gg_record(dir = "temp_20_eurovision", device = "png", width = 3.375, height = 3.375, units = "in", dpi = 320)

font_add_google("Lato")
font_add(family = "Lato black",
         regular = "/Users/pabloalvarezbaeza/Library/Fonts/Lato-Black.ttf")

showtext_auto()


# Data --------------------------------------------------------------------
eurovision_raw <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-17/eurovision.csv')
eurovision <- eurovision_raw |> 
  filter(year >= 2005, section == "grand-final") # This is the first year where the 'grand-final' happens

# Analysis ----------------------------------------------------------------
highest_scores_by_rank <- eurovision |> 
  filter(rank <= 3) |> 
  group_by(rank) |> 
  slice_max(total_points, n = 1) |> 
  ungroup() |> 
  select(year, artist_country, total_points)

# Margin between 1st and 2nd
eurovision |> 
  select(rank, total_points, year) |> 
  filter(rank <= 2) |> 
  group_by(year) |> 
  arrange(rank) |> 
  mutate(margin = total_points - lag(total_points)) |> 
  ungroup() |> 
  filter(rank == 2) |> 
  slice_min(margin, n = 1)


# Plot --------------------------------------------------------------------
eurovision |> 
  mutate(rank = factor(rank)) |> 
  ggplot(aes(year, total_points, group = rank, color = rank)) +
  geom_line(data = eurovision |> filter(rank > 3), color = "grey75", size = .25, alpha = .5) +
  geom_line(data = eurovision |> filter(rank == 3), color = "#0FB5AE", size = .75) +
  geom_line(data = eurovision |> filter(rank == 2), color = "#4046CA", size = .75) +
  geom_line(data = eurovision |> filter(rank == 1), color = "#F68511", size = .75) +
  geom_point(data = eurovision |> filter(year == 2017, rank == 1), shape = 21, color = "white", fill = "#F68511" , size = 1.5) +
  geom_point(data = eurovision |> filter(year == 2009, rank == 1), shape = 21, color = "white", fill = "#F68511" , size = 1.5) +
  geom_point(data = eurovision |> filter(year == 2009, rank == 2), shape = 21, color = "white", fill = "#4046CA" , size = 1.5) +
  annotate(geom = "segment", x = 2009, xend = 2009, y = 218 + 169, yend = 218, size = .1, color = "black") +
  annotate("text", x = 2017, y = highest_scores_by_rank$total_points[1] + 30, label = highest_scores_by_rank$total_points[1], hjust = .5, color = "#F68511", family = "Lato", fontface = "bold", size = 6) +
  annotate("text", x = 2009.1, y = 218 + 169 + 30, label = 218 + 169, hjust = 0, color = "#F68511", family = "Lato", fontface = "bold", size = 6) +
  annotate("text", x = 2009.1, y = 218 + 30, label = 218, hjust = 0, color = "#4046CA", family = "Lato", fontface = "bold", size = 6) +
  coord_cartesian(expand = c(0, 0),
                  ylim = c(0, 800)) +
  theme_minimal(base_family = "Lato") +
  labs(x = NULL,
       y = "Total points",
       title = "EUROVISION points are trending upwards",
       subtitle = "The upward gradual trend reflects that countries need more points to come <span style='color:#F68511'>**first**</span>, <span style='color:#4046CA'>**second**</span>, and <span style='color:#0FB5AE'>**third**</span><br>with each new edition",
       caption = "Visualization by Pablo Alvarez | Data from Eurovision"
  ) +
  theme(
    plot.margin = margin(rep(10, 4)),
    panel.grid = element_blank(),
    panel.grid.major = element_line(size = .15, color = "grey75"),
    plot.background = element_rect(fill = "grey90", color = "grey90"),
    panel.background = element_rect(fill = "grey90", color = "grey90"),
    axis.title.y = element_text(angle = 90, hjust = .99, size = 16.5, color = "grey25"),
    axis.title.x = element_text(hjust = 0, size = 20, color = "grey15"),
    axis.text = element_text(size = 16, color = "grey50"), 
    axis.line = element_line(color = "grey75", size = .15),
    plot.title = element_text(size = 32,
                              color = "black",
                              family = "Lato black",
                              hjust = 0),
    plot.subtitle = element_markdown(size = 16,
                                 color = "black",
                                 family = "Lato",
                                 hjust = 0,
                                 margin = margin(b = 20, t = 1),
                                 lineheight = .35),
    plot.caption = element_text(size = 18,
                                color = "grey50",
                                hjust = 0,
                                margin = margin(t = 20),
                                lineheight = .3)
  )

ggsave("tidytuesday_2022_w20.png", width = 1080, height = 1080, units = "px", dpi = 320)

gg_playback(
  first_image_duration = 8,
  last_image_duration = 12,
  frame_duration = .15
)
