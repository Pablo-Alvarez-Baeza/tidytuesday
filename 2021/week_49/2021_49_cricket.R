library(pacman)
p_load(tidyverse, tidytuesdayR, showtext)

font_add_google("Lato")

# Get the data ------------------------------------------------------------

tt <- tidytuesdayR::tt_load('2021-11-30')
matches <- tt$matches

matches |> View()


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
  geom_col(aes(fct_reorder(team, wins_value))) +
  geom_text(aes(y = label_y,
                label = games),
            size = 2) +
  coord_flip()


# Final graph -------------------------------------------------------------
theme_set(theme_minimal(base_family = "Lato"))

plot + 
  labs(x = NULL,
       y = NULL) +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.margin = margin(c(20, 40, 20, 40)))
