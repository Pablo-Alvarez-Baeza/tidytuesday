library(pacman)
p_load(tidyverse, tidytuesdayR, ggalluvial, ggtext, showtext)

font_add_google("Lato")
showtext_auto()

tt <- tidytuesdayR::tt_load('2022-01-18')
chocolate <- tt$chocolate


# South American countries export to 35 countries 
chocolate |> 
  filter(country_of_bean_origin != company_location) |> 
  mutate(group = factor(if_else(country_of_bean_origin %in%
                                  c("Brazil", "Argentina", "Chile",
                                    "Peru", "Venezuela", "Colombia",
                                    "Ecuador", "Bolivia", "Uruguay",
                                    "Paraguay", "Guyana", "Suriname",
                                    "French Guiana", "Falkland Islands"), 1, 2))) |> 
  group_by(group) |>
  summarize(n_countries = n_distinct(company_location)) |> 
  ungroup()

# country_of_bean_origin exports to company_location, n times 
chocolate_exports <- chocolate |>
  count(country_of_bean_origin, company_location, sort = TRUE) |> 
  rename(total_exports_by_country = n) 

# total exports by country
chocolate_exports_by_country <- chocolate |>
  count(country_of_bean_origin, sort = TRUE) |> 
  rename(total_exports = n) 


df <- left_join(chocolate_exports, chocolate_exports_by_country) |>
  mutate(exported_imported = if_else(country_of_bean_origin == company_location, 1, 2),
         country_of_bean_origin = factor(country_of_bean_origin),
         country_of_bean_origin = fct_reorder(country_of_bean_origin, desc(total_exports)),
         group = factor(if_else(!country_of_bean_origin %in%
                                  c("Brazil", "Argentina", "Chile",
                                    "Peru", "Venezuela", "Colombia",
                                    "Ecuador", "Bolivia", "Uruguay",
                                    "Paraguay", "Guyana", "Suriname",
                                    "French Guiana", "Falkland Islands"), 1, 2)))


df |> 
  filter(group == 2,
         exported_imported == 2) |> 
  ggplot(aes(axis1 = country_of_bean_origin, axis2 = company_location, y = total_exports_by_country,
           label = country_of_bean_origin)) +
  geom_alluvium(width = 1/50, alpha = .65) +
  geom_stratum(width = 1/50, size = .2, fill = NA, color = "white") +
  #geom_text(stat = "stratum", size = 2, aes(label = after_stat(stratum))) +
  coord_cartesian(clip = "off",
                  expand = c(0, 0)) +
  #scale_y_continuous(breaks = seq(0, 800, 50)) +
  annotate(geom = "text", x = .9, y = 677, label = "Venezuela", hjust = 0, size = 4, color = "white", fontface = "bold") +
  annotate(geom = "text", x = .9, y = 460, label = "Peru", hjust = 0, size = 4, color = "white", fontface = "bold") +
  annotate(geom = "text", x = .9, y = 270, label = "Ecuador", hjust = 0, size = 4, color = "white", fontface = "bold") +
  annotate(geom = "text", x = .9, y = 150, label = "Bolivia", hjust = 0, size = 4, color = "white", fontface = "bold") +
  annotate(geom = "text", x = .9, y = 80, label = "Colombia", hjust = 0, size = 4, color = "white", fontface = "bold") +
  annotate(geom = "text", x = .9, y = 30, label = "Brazil", hjust = 0, size = 4, color = "white", fontface = "bold") +
  annotate(geom = "text", x = 2, y = 600, label = "United States", hjust = .5, size = 4, color = "black", fontface = "bold", angle = 90) +
  annotate(geom = "text", x = 1.1, y = -50, label = "Origin", hjust = .5, size = 4, color = "white") +
  annotate(geom = "text", x = 1.9, y = -50, label = "Destination", hjust = .5, size = 4, color = "white") +
  labs(title = "If you eat chocolate, it likely comes from South America",
       subtitle = "South American countries export cocoa beans to 35 countries",
       caption = "Visualization by Pablo Alvarez | Data from Flavors of Cacao") +
  theme_void(base_family = "Lato") +
  theme(plot.margin = margin(10, 20, 10, 20),
        plot.background = element_rect(fill = "black", color = "black"),
        panel.background = element_rect(fill = "black", color = "black"),
        plot.title = element_text(
          size = 32,
          color = "white",
          margin = margin(t = 15),
          face = "bold",
        ),
        plot.subtitle = element_text(
          size = 18,
          color = "white",
          margin = margin(t = 10, b = 40),
        ),
        plot.caption = element_text(
          color = "grey50",
          hjust = .5,
          size = 10,
          margin = margin(t = 40, b = 5)
        )
  )

ggsave("tidytuesay_2022_w3.png", width = 12, height = 8.5, dpi = 320)