library(pacman)
p_load(tidyverse, tidytuesdayR, geofacet, ggtext, showtext, cowplot)

font_add_google("Lato")
showtext_auto()

tt <- tidytuesdayR::tt_load('2022-01-11')
colony <- tt$colony

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


plot <- df |> 
  ggplot(aes(x = 1, y = 1, fill = group)) +
  geom_col(show.legend = FALSE) +
  facet_geo(~ state) +
  geom_text(aes(label = state, x = 1, y = 5),
              size = 2.3, fontface = "bold") +
  coord_cartesian(expand = c(0, 0), clip = "off") +
  scale_fill_manual(values = c("#2596be","#f2f2f2","#ed4e83", "grey50")) +
  labs(title = "Hawaiian bees thrive",
         subtitle = "States' consecutive percentage of bee colonies lost vs. renovated, 2015 - 2021",
         caption = "Visualization by Pablo Alvarez | Data from United States Department of Agriculture (USDA). Data is not available for the states colored in dark grey") +
  theme_void(base_family = "Lato") +
  theme(panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        plot.margin = margin(20, 40, 20, 80),
        panel.spacing = unit(0, "lines"),
        panel.border = element_rect(color = "white", fill = NA, size = .3),
        strip.text = element_blank(),
        strip.background = element_blank(),
        legend.position = "top",
        plot.title = element_text(
          size = 18,
          margin = margin(t = 15),
          face = "bold",
          family = "Lato Black (900)"
          ),
          plot.subtitle = element_text(
            size = 14,
            margin = margin(t = 10, b = 40),
            family = "Lato"
          ),
          plot.caption = element_text(
            color = "grey50",
            family = "Lato",
            hjust = .5,
            size = 10,
            margin = margin(t = 40, b = 5)
          )
    )
 
ggdraw(plot) + 
  draw_label(label = "Hawaii is the only state\nwhere the percentage of colonies\nrenovated is greater than\nthe percentage of colonies lost each year",
             x = .035, y = 0.275,
             size = 10, hjust = 0,
             lineheight = 1,
             fontfamily = "Lato",
             fontface = "bold",
             color = "#2596be") +
  draw_label(label = "Minnesota, Illinois, and Virginia\nhave a higher percentage\nof colonies lost than\nrenovated every year",
             x = .75, y = 0.32,
             size = 10, hjust = 0,
             lineheight = 1,
             fontfamily = "Lato",
             fontface = "bold",
             color = "#ed4e83") 

ggsave("tidytuesday_2022_w2.png", width = 10, height = 8.5, units = "in", dpi = 320)