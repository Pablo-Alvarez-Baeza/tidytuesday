library(pacman)
p_load(tidyverse, tidytuesdayR, geofacet, ggtext, showtext)

font_add_google("Lato")
showtext_auto()

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
                                  consecutive_year == 7 ~ "C"))) |> 
  add_row(state = c("Nevada", "Alaska", "New Hampshire", "Rhode Island", "Delaware"),
          consecutive_year = rep(5), group = rep("D", 5))

 p <-  df |> 
    ggplot(aes(x = 1, y = 1, fill = group)) +
    geom_col(show.legend = FALSE) +
    facet_geo(~ state) +
    geom_text(aes(label = state, x = 1, y = .8, ),
              size = 1.5, fontface = "bold") +
    coord_cartesian(expand = c(0, 0), clip = "off") +
    scale_fill_manual(values = c("#face48","#f0f0f0","#ed4e83", "grey95")) +
    labs(title = "Hawaiian bees thrive",
         subtitle = "States' consecutive percentage of bee colonies lost vs. renovated, 2015 - 2021",
         caption = "Visualization by Pablo Alvarez | Data from United States Department of Agriculture (USDA)") +
    theme_void(base_family = "Lato") +
    theme(plot.margin = margin(20, 40, 20, 40),
          panel.spacing = unit(0, "lines"),
          panel.border = element_rect(color = "white", fill = NA, size = .3),
          strip.text = element_blank(),
          strip.background = element_blank(),
          plot.title = element_text(
            size = 12,
            margin = margin(t = 15),
            face = "bold",
            family = "Lato"
          ),
          plot.subtitle = element_text(
            size = 10,
            margin = margin(t = 20, b = 20)
          ),
          plot.caption = element_text(
            color = "grey50",
            hjust = .5,
            size = 10,
            margin = margin(t = 15, b = 5)
          )
    )
 
 
#A data frame with labels for each facet
f_labels <- data.frame(state = c("Hawaii"), label = "<span style='color:green'>Hawaii</span>")
 
p +
   annotate(x = 1, y = 1, geom = "text", data = f_labels)
 
 
p +
  annotate(data = df |> filter(state == "Hawaii"), geom = "text", label = "a", x = 1, y = 1))
  
  ggdraw(p) + draw_label("<span style='color:green'>Hawaii</span>" , x = 0.1, y = 0.35)
  subtitle = "<span style='color:green'>Hawaii</span> is the only state where the percentage of colonies renovated is greater than the percentage of colonies lost each year.<br>In contrast, <span style='color:red'>Minnesota, Illinois, and Virginia</span> have a higher percentage of colonies lost than renovated every year.",
  
  