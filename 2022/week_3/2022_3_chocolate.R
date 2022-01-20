library(pacman)
p_load(tidyverse, tidytuesdayR, ggalluvial, gapminder, ggtext, showtext)

#font_add_google()

tt <- tidytuesdayR::tt_load('2022-01-18')
chocolate <- tt$chocolate


countries_summary <- chocolate |> 
  group_by(country_of_bean_origin) |> 
  count() |> 
  ungroup() |> 
  mutate(total = sum(n),
         perc_country = round(n / total, 2)) |>
  arrange(desc(perc_country)) |> 
  mutate(cumsum_perc = cumsum(perc_country),
         group = factor(if_else(cumsum_perc <= .45, 1, 2))) |> 
  select(country_of_bean_origin, perc_country, group)


df <- chocolate |> 
  group_by(country_of_bean_origin) |> 
  count(company_location, sort = TRUE) |> 
  ungroup() |> 
  left_join(countries_summary, by = "country_of_bean_origin") |> 
  mutate(country_of_bean_origin = fct_reorder(country_of_bean_origin, desc(perc_country)))

 
ggplot(df,
       aes(axis1 = country_of_bean_origin, axis2 = company_location, y = n, label = country_of_bean_origin)) +
  geom_alluvium(aes(fill = group), show.legend = FALSE, na.rm = TRUE, alpha = .65) +
  #geom_stratum(alpha = .5)  +
  #geom_text(stat = "stratum", size = 2, min.y = 73, aes(label = after_stat(stratum))) +
  scale_y_continuous(breaks = seq(0, 2500, by = 100),
                     expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  annotate(geom = "text", x = 1, y = 2410, label = "Peru", hjust = .5, size = 4, color = "white", fontface = "bold") +
  annotate(geom = "text", x = 1, y = 2160, label = "Venezuela", hjust = .5, size = 4, color = "white", fontface = "bold") +
  annotate(geom = "text", x = 1, y = 1930, label = "Dominican Republic", hjust = .5, size = 4, color = "white", fontface = "bold") +
  annotate(geom = "text", x = 1, y = 1700, label = "Ecuador", hjust = .5, size = 4, color = "white", fontface = "bold") +
  annotate(geom = "text", x = 1, y = 1500, label = "Madagascar", hjust = .5, size = 4, color = "white", fontface = "bold") +
  annotate(geom = "text", x = 2, y = 1700, label = "United States", hjust = .5, size = 4, color = "black", fontface = "bold") +
  annotate("segment", x = .9, xend = 1.1, y = 2525, yend = 2525, colour = "white") +
  annotate("segment", x = .9, xend = 1.1, y = 2290, yend = 2290, colour = "white") +
  annotate("segment", x = .9, xend = 1.1, y = 2025, yend = 2025, colour = "white") +
  annotate("segment", x = .9, xend = 1.1, y = 1805, yend = 1805, colour = "white") +
  annotate("segment", x = .9, xend = 1.1, y = 1590, yend = 1590, colour = "white") +
  annotate("segment", x = .9, xend = 1.1, y = 1405, yend = 1405, colour = "white") +
  annotate("segment", x = 1.9, xend = 2.1, y = 1110, yend = 1110, colour = "white") +
  annotate("segment", x = 1.9, xend = 2.1, y = 2250, yend = 2250, colour = "white") +
  scale_fill_manual(values = c("#654321", "grey75")) +
  labs(title = "<span style='font-size:36pt'>**45%**</span> of the cacao beans in the world are distributed by these countries") +
  theme_void() +
  theme(plot.margin = margin(10, 40, 10, 40),
        plot.background = element_rect(fill = "black", color = "black"),
        panel.background = element_rect(fill = "black", color = "black"),
        plot.title = element_markdown(
          color = "white", 
          size = 16, 
          hjust = .5,
          face = "bold",
          margin = margin(b = 40)
        )
  )

ggsave("plot.png", width = 12, height = 8.5, dpi = 320)
