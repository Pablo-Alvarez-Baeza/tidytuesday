library(pacman)
p_load(tidyverse, tidytuesdayR, showtext, ggtext)

font_add_google("Lato")
showtext_auto()

tt <- tidytuesdayR::tt_load('2022-01-25')

ratings <- tt$ratings
details <- tt$details


details_filtered <- details |> 
  select(id, primary, year = yearpublished) |> 
  filter(year >= 1930, year <= 2021) 

# Popular games published during 1930 - 2021
games <- details_filtered |> 
  filter(primary %in% c("Monopoly", "Scrabble", "Risk", "Catan",
                        "Jenga", "Pictionary", "Pandemic", "Azul"))  |> 
  arrange(year) |> 
  distinct(primary, year) |> 
  group_by(primary) |> 
  slice(which.min(year)) |> 
  ungroup() |> 
  arrange(year)

# How many games have been published since 1930?
details_filtered |> 
  summarize(n_distinct(id))

# Final plot
details_filtered |> 
  filter(year >= 1930,
         year <= 2021) |> 
  group_by(year) |> 
  count() |> 
  ggplot(aes(year, n)) +
  geom_area(fill = "#FC28FB", color = "white", size = .3) +
  ylim(c(0, 1350)) +
  scale_x_continuous(limits = c(1929, 2023),
                     breaks = seq(1930, 2021, 10)) +
  coord_cartesian(expand = c(0, 0)) +
  annotate(geom = "segment", x = 1933, xend = 1933, y = 0, yend = 250, size = .3, color = "white") +
  annotate(geom = "text", x = 1933, y = 270, hjust = 0, size = 4, color = "white", label = "Monopoly", family = "Lato") + 
  annotate(geom = "segment", x = 1948, xend = 1948, y = 0, yend = 250, size = .3, color = "white") +
  annotate(geom = "text", x = 1948, y = 270, hjust = 0, size = 4, color = "white", label = "Scrabble", family = "Lato") +
  annotate(geom = "segment", x = 1959, xend = 1959, y = 0, yend = 250, size = .3, color = "white") +
  annotate(geom = "text", x = 1959, y = 270, hjust = 0, size = 4, color = "white", label = "Risk", family = "Lato") +
  annotate(geom = "segment", x = 1983, xend = 1983, y = 0, yend = 250, size = .3, color = "white") +
  annotate(geom = "text", x = 1983, y = 270, hjust = 1, size = 4, color = "white", label = "Jenga", family = "Lato") +
  annotate(geom = "segment", x = 1985, xend = 1985, y = 0, yend = 250, size = .3, color = "white") +
  annotate(geom = "text", x = 1985, y = 270, hjust = 0, size = 4, color = "white", label = "Pictionary", family = "Lato") +
  annotate(geom = "segment", x = 1995, xend = 1995, y = 0, yend = 300, size = .3, color = "white") +
  annotate(geom = "text", x = 1995, y = 320, hjust = 0, size = 4, color = "white", label = "Catan", family = "Lato") +
  annotate(geom = "segment", x = 2008, xend = 2008, y = 0, yend = 800, size = .3, color = "white") +
  annotate(geom = "text", x = 2008, y = 820, hjust = 1, size = 4, color = "white", label = "Pandemic", family = "Lato") +
  annotate(geom = "segment", x = 2017, xend = 2017, y = 0, yend = 1250, size = .3, color = "white") +
  annotate(geom = "segment", x = 2015, xend = 2017, y = 1250, yend = 1250, size = .3, color = "white") +
  annotate(geom = "text", x = 2015, y = 1270, hjust = 1, size = 4, color = "white", label = "Azul", family = "Lato") +
  labs(title = "The Rise of Board Games",
       subtitle = "<span style='font-size:22pt; color:#FC28FB'>**+20,000**</span> board games have been published since 1930",
       caption = "Visualization by Pablo Alvarez | Data from Kaggle") +
  theme_minimal(base_family = "Lato") +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "black", color = "black"),
    plot.background = element_rect(fill = "black", color = "black"),
    plot.margin = margin(10, 10, 10, 10),
    axis.text.y = element_blank(),
    axis.text.x = element_text(color = "white",
                               size = 10,
                               margin = margin(10)),
    plot.title = element_text(
      color = "white",
      face = "bold",
      size = 36,
      margin = margin(40)
    ),
    plot.subtitle = element_markdown(
      color = "white",
      size = 20,
      margin = margin(10)
    ),
    plot.caption = element_text(
      color = "white",
      size = 10,
      hjust = .5,
      margin = margin(t = 40, b = 10)
    )
  )

ggsave("tidytuesday_2022_w4.png", width = 12, height = 10, units = "in", dpi = 320)
