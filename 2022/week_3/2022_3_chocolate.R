library(pacman)
p_load(tidyverse, tidytuesdayR, ggalluvial, gapminder, ggtext, showtext)

#font_add_google()

tt <- tidytuesdayR::tt_load('2022-01-18')
chocolate <- tt$chocolate

chocolate |> glimpse()
gapminder <- gapminder |> 
  select(country, continent)

continents_summary <- chocolate |> 
  left_join(gapminder, by = c("country_of_bean_origin" = "country")) |>
  relocate(continent_of_bean_origin = continent, .after = country_of_bean_origin) |> 
  group_by(continent_of_bean_origin) |> 
  count() |> 
  ungroup() |> 
  mutate(total = sum(n),
         perc_continent = round(n / total, 2)) |> 
  select(continent_of_bean_origin, perc_continent)

df <- chocolate |> 
  left_join(gapminder, by = c("country_of_bean_origin" = "country")) |>
  relocate(continent_of_bean_origin = continent, .after = country_of_bean_origin) |> 
  group_by(continent_of_bean_origin) |> 
  count(company_location, sort = TRUE) |> 
  mutate(sum = sum(n),
         perc = round(n / sum, 2),
         continent_of_bean_origin = factor(continent_of_bean_origin),
         company_location = factor(company_location),
         group = factor(if_else(continent_of_bean_origin == "Americas", 1, 2))) |>   ungroup() |> 
  left_join(continents_summary, by = "continent_of_bean_origin") |> 
  mutate(continent_of_bean_origin = fct_reorder(continent_of_bean_origin, desc(perc_continent))) 

countries_text <- chocolate |> 
  left_join(gapminder, by = c("country_of_bean_origin" = "country")) |> 
  filter(continent == "Americas") |> 
  count(country_of_bean_origin, sort = TRUE) |> 
  mutate(text_size = rank(n))

df |> 
  ggplot(aes(axis1 = continent_of_bean_origin, axis2 = company_location, y = n)) +
  geom_alluvium(aes(fill = group), show.legend = FALSE, na.rm = TRUE, alpha = .65) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c("grey25", "white")) +
  labs(title = "<span style='font-size:36pt'>**74%**</span> of the cacao beans in the world are distributed by the Americas",
       caption = "Visualization by Pablo Alvarez | Data from Flavors of Cacao") +
  annotate(geom = "text", x = .8, y = 24000, label = "Venezuela", hjust = 1, size = 3, color = "white") +
  annotate(geom = "text", x = .8, y = 23000, label = "Peru", hjust = 1, size = 3) +
  annotate(geom = "text", x = .8, y = 22000, label = "Dominican Republic", hjust = 1, size = 3) +
  annotate(geom = "text", x = .8, y = 21000, label = "Ecuador", hjust = 1, size = 3) +
  annotate(geom = "text", x = .8, y = 20000, label = "Nicaragua", hjust = 1, size = 3) +
  annotate(geom = "text", x = .8, y = 19000, label = "Bolivia", hjust = 1, size = 3) +
  annotate(geom = "text", x = .8, y = 18000, label = "Colombia", hjust = 1, size = 3) +
  annotate(geom = "text", x = .8, y = 17000, label = "Brazil", hjust = 1, size = 3) +
  annotate(geom = "text", x = .8, y = 16000, label = "Guatemala", hjust = 1, size = 3) +
  annotate(geom = "text", x = .8, y = 15000, label = "Mexico", hjust = 1, size = 3) +
  annotate(geom = "text", x = .8, y = 14000, label = "Costa Rica", hjust = 1, size = 3) +
  annotate(geom = "text", x = .8, y = 13000, label = "Honduras", hjust = 1, size = 3) +
  annotate(geom = "text", x = .8, y = 12000, label = "Jamaica", hjust = 1, size = 3) +
  annotate(geom = "text", x = .8, y = 11000, label = "Cuba", hjust = 1, size = 3) +
  annotate(geom = "text", x = .8, y = 10000, label = "Panama", hjust = 1, size = 3) +
  annotate(geom = "text", x = .8, y = 9000, label = "Puerto Rico", hjust = 1, size = 3) +
  annotate(geom = "text", x = .8, y = 8000, label = "El Salvador", hjust = 1, size = 3) +
  theme_void() +
  theme(plot.margin = margin(10, 40, 10, 40),
        plot.background = element_rect(fill = "black", color = "black"),
        panel.background = element_rect(fill = "black", color = "black"),
        plot.title = element_markdown(
          color = "white", 
          size = 16, 
          hjust = .5,
          face = "bold",
          margin = margin(t = 15)
        )
  )

ggsave("plot.png", width = 12, height = 8.5, dpi = 320)
