library(pacman)
p_load(tidyverse, tidytuesdayR)

tt <- tidytuesdayR::tt_load('2022-01-11')

colony <- tt$colony

colony |> glimpse()

df <- colony |> 
  select(year, months, state, colony_lost_pct, colony_reno_pct) |> 
  filter(!state %in% c("United States", "Other States")) |> 
  mutate(state = factor(state)) |> 
  group_by(state, year) |> 
  summarize(mean_lost_pct = round(mean(colony_lost_pct, na.rm = TRUE), 2),
            mean_reno_pct = round(mean(colony_reno_pct, na.rm = TRUE), 2)) |> 
  mutate(rank = if_else(mean_lost_pct > mean_reno_pct, 1, 0)) |> 
  group_by(state) |> 
  summarize(consecutive_year = sum(rank)) |> 
  ungroup() |> 
  mutate(group = factor(case_when(consecutive_year == 0 ~ "A",
                                  consecutive_year %in% c(1:6) ~ "B",
                                  consecutive_year == 7 ~ "C"))) 


df |> 
  filter(state == "Alabama") |>
  ggplot() +
  geom_segment(aes(x = x_start, xend = x_end, y = 100, yend = y_end, color = group),
               linetype = 1, show.legend = FALSE) +
  geom_point(x = 1, y = 100, size = 2) +
  geom_point(aes(x = 2, y = y_end), size = 2) +
  scale_y_continuous(limits = c(0, 100))
  facet_geo(~ state, scales = "free") +
  theme_void() 
  theme(
    plot.margin = margin(20, 40, 20, 40),
    
  )

  df |> 
    ggplot(aes(x = 1, y = 1, fill = group)) +
    geom_col(show.legend = FALSE) +
    facet_geo(~ state) +
    coord_cartesian(expand = c(0, 0)) +
    geom_text(aes(label = state, x = 0.75, y = .9), size = .5) +
    theme_void() +
    theme(panel.spacing = unit(0, "lines"),
          strip.text = element_blank(),
          strip.background = element_blank())
  
  df |> 
    arrange(desc(colony_lost_pct)) |> View()
  