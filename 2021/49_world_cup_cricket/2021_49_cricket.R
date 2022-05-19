library(pacman)
p_load(tidyverse, tidytuesdayR, showtext, ggtext)

font_add_google("Lato")


# Get the data ------------------------------------------------------------


tt <- tidytuesdayR::tt_load('2021-11-30')
matches <- tt$matches


# Data wrangling ----------------------------------------------------------


teams_played <- matches |> 
  pivot_longer(cols = c(team1, team2), values_to = "team") |> 
  count(team) |> 
  rename(played = n)

teams_wins <- matches |> 
  count(winner) |> 
  rename(wins = n)

teams_played_wins <- left_join(teams_played, teams_wins, by = c("team" = "winner")) |> 
  replace_na(list(wins = 0)) |> 
  slice_max(played, n = 10) |> 
  mutate(wins_value = wins) |> 
  pivot_longer(cols = c("wins", "played"), names_to = "category", values_to = "games") |> 
  mutate(category = factor(category)) |> 
  relocate(wins_value, .after = games)


# Basic chart -------------------------------------------------------------


plot <- teams_played_wins |> 
  group_by(team) |> 
  # Calculate y position, placing it in the middle
  mutate(label_y = cumsum(games) - 0.5 * games) |> 
  ungroup() |> 
  ggplot(aes(team, games, fill = category)) +
  geom_col(aes(fct_reorder(team, wins_value)),
           color = "grey65",
           size = .2) +
  geom_text(aes(y = label_y, label = games),
            size = 7,
            color = "grey60") +
  coord_flip()


# Final graph -------------------------------------------------------------


theme_set(theme_minimal(base_family = "Lato"))

plot + 
  labs(x = NULL,
       y = NULL,
       title = "Which team has <span  style = 'color: #185ADB; '>won</span> the most games in ODI?",
       subtitle = "Australia is the team with the most victories.",
       caption= "Visualization by Pablo Alvarez • Data from TidyTuesday | 2021 - Week 49 • Games were played during the following time period: January 1, 1996 - December 31, 2005. <br>A One Day International (ODI) is a form of limited overs cricket, played between two teams with international status, in which each team faces a fixed number of overs, currently 50, with the game lasting up to 9 hours.") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c("white", "#185ADB")) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(
          color = "grey50",
          face = "bold",
          size = 24,
          margin = margin(r = 10)
          ),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.margin = margin(c(20, 40, 20, 40)),
        plot.background = element_rect(
          fill = "white",
          color = "white"
          ),
        panel.background = element_rect(
          fill = "white", color = "white"
          ),
        legend.position = "none",
        # Customize title appearance
        plot.title = element_markdown(
          color = "grey25", 
          size = 38, 
          face = "bold",
          margin = margin(t = 15)
        ),
        # Customize subtitle appearance
        plot.subtitle = element_markdown(
          color = "grey50",
          size = 26,
          lineheight = 1.35,
          margin = margin(t = 15, b = 40)
        ),
        # Title and caption are going to be aligned
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption = element_markdown(
          color = "grey30", 
          size = 13,
          lineheight = 1.2, 
          hjust = 0,
          margin = margin(t = 40) # Large margin on the top of the caption.
        ) 
  )

ggsave("tidytuesday_2021_w49.png", width = 20, height = 12, units = "in", dpi = 320)