library(pacman)
p_load(tidyverse, tidytuesdayR, ggtext, showtext)

font_add_google("Lato")

# Get the data
tt <- tidytuesdayR::tt_load('2021-11-23')
#tidytuesdayR::readme(tt)

# Data frames
directors <- tt$directors
episodes <- tt$episodes
writers <- tt$writers
imdb <- tt$imdb

imdb |> 
  glimpse()


# Convert variable season to a categorical variable 
imdb <- imdb |> 
  mutate(season = factor(season),
         season_label = factor(paste("Season", season), 
                               levels = c("Season 1", "Season 2", "Season 3",
                                          "Season 4", "Season 5", "Season 6",
                                          "Season 7", "Season 8", "Season 9",
                                          "Season 10", "Season 11", "Season 12"),
                               ordered = TRUE))


# Basic graph -------------------------------------------------------------


plot <- imdb |> 
  ggplot(aes(ep_num, rating))  + 
  geom_line(color = "#14279B",
            size = 1.2) +
  geom_hline(
    yintercept = 5.5,
    color = "white",
    size = .5,
  ) +
  facet_wrap(~ season_label, scales='free')


# Final chart
theme_set(theme_minimal(base_family = "Lato"))

plot +
  scale_y_continuous(limits = c(1, 15),
                     breaks = seq(1, 10, by = .5),
                     labels = c(1, rep("", 8), 5.5, rep("", 8), 10)) +
  labs(x = NULL,
       y = NULL,
       title = "Doctor Who",
       subtitle = "Seasons IMDb rating over time",
       caption = "Visualization by Pablo Alvarez • TidyTuesday | 2021 - Week 48 • IMDb ratings are on a scale from 1 - 10 with 1 meaning the title was terrible and 10 meaning it was excellent.") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 17, margin = margin(r = 5),
                               color = "grey40"),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(20, 40, 20, 40),
    plot.background = element_rect(fill = "grey98", color = "grey98"),
    panel.background = element_rect(fill = "#EAEAEA", color = "#EAEAEA"),
    strip.text = element_text(hjust = 0,
                              size = 20,
                              face = "bold",
                              margin = margin(t = 12,
                                              l = 8)),
    panel.spacing = unit(2, "lines"),
    strip.background = element_rect(fill = "#EAEAEA",
                                    color = "#EAEAEA"),
    plot.title = element_text(
      color = "black",
      face = "bold",
      size = 32,
      margin = margin(t = 15)
    ),
    plot.subtitle = element_markdown(
      color = "grey30",
      size = 20,
      lineheight = 1.35,
      margin = margin(t = 15, b = 40)
    ),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.caption = element_text(
      size = 13,
      color = "grey30",
      lineheight = 1.2,
      hjust = 0,
      margin = margin(t = 40)
    )
  )

ggsave("tidytuesday_2021_w48.png", width = 20, height = 12, units = "in", dpi = 320)
