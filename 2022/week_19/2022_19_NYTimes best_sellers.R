library(pacman)
p_load(tidyverse, janitor, camcorder, showtext, lubridate, ggtext)

gg_record(dir = "temp_NYTimes", device = "png", width = 3.375, height = 3.375, units = "in", dpi = 320)


# Fonts
font_add_google("Lato")
font_add(family = "Lato black",
         regular = "/Users/pabloalvarezbaeza/Library/Fonts/Lato-Black.ttf")

showtext_auto()


# Data --------------------------------------------------------------------
nyt_titles <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_titles.tsv')
nyt_full <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_full.tsv')

# Author with the most number of weeks 2000-2020: John Grisham, 471 weeks
nyt_full |> 
  filter(year >= 2000) |> 
  group_by(author) |> 
  summarize(n = n_distinct(week)) |> 
  slice_max(n, n = 1)

weeks <- data.frame(week = seq(ymd('2000-01-01'), ymd('2020-12-31'), by = 'weeks') + 1) |> 
  mutate(week_n = week(week))

nyt_john_grisham <- nyt_full |> 
  filter(year >= 2000, author == "John Grisham") |> 
  select(week, rank, title) |> 
  mutate(week_n = week(week),
         year = year(week)) |> 
  group_by(year, week) |> 
  count() |> 
  mutate(group = if_else(n == 1, 1, 2)) |> 
  ungroup()

nyt_john_grisham_list <- left_join(weeks, nyt_john_grisham, by = "week") |> 
  mutate(listed = factor(if_else(is.na(group), 0, group)),
         year = year(week))

# Check all weeks with a title are part of the data frame
nyt_john_grisham_list |> 
  na.omit() |> 
  summarize(n_distinct(week))
  

# Plot --------------------------------------------------------------------
nyt_john_grisham_list |> 
  count(week, year, listed) |> 
  ggplot(aes(fill = listed, values = n, fill = listed)) +
  geom_waffle(size = .1, n_rows = 1, color = "white") +
  coord_equal(expand = c(0, 0)) +
  facet_wrap(~ year, ncol = 1, strip.position = "top") +
  scale_fill_manual(values = c("grey50", "#FF3EB5", "#0A8FD4")) +
  theme_void(base_family = "Lato") +
  labs(title = "John Grisham, America's Favorite Storyteller",
       subtitle = "Grisham is the author with the most number of weeks with <span style='color:#FF3EB5'>**one**</span> or <span style='color:#0A8FD4'>**multiple**</span> books appearing<br> on the New York Times Fiction Best Sellers List between the years of 2000 and 2020",
       caption = "Visualization by Pablo Alvarez | Data from Post45") +
  theme(plot.margin = margin(c(0, 0, 0, 0)),
        panel.spacing = unit(.15, "lines"),
        strip.text = element_text(hjust = 0, margin = margin(b = 1), color = "black", size = 11),
        plot.background = element_rect(fill = "grey90", color = "grey90"),
        panel.background = element_rect(fill = "grey90", color = "grey90"),
        legend.position = "none",
        plot.title = element_text(color = "black",
                                      family = "Lato black",
                                      size = 30,
                                      hjust = 0),
        plot.subtitle = element_markdown(color = "black",
                                         size = 16,
                                         margin = margin(t = 3.5, b = 12),
                                         hjust = 0,
                                         lineheight = .35),
        plot.caption = element_text(color = "black",
                                    size = 12,
                                    margin = margin(t = 8.5, b = 2.5),
                                    hjust = 0))

ggsave("tidytuesday_2022_w19.png", width = 1080, height = 1080, units = "px", dpi = 320)



