library(pacman)
p_load(tidyverse, tidytuesdayR, lubridate, ggtext, showtext)


font_add(family = "Lato Black",
         regular = "Lato-Black.ttf")
font_add(family = "Lato",
         regular = "Lato-Regular.ttf")
showtext_auto()

tt <- tidytuesdayR::tt_load('2022-02-08')
airmen <- tt$airmen


airmen |> 
  arrange(desc(name)) |> 
  mutate(n_row = 1:n(),
         group = case_when(n_row <= 100 ~ 10,
                           n_row > 100 & n_row <= 200 ~ 9,
                           n_row > 200 & n_row <= 300 ~ 8,
                           n_row > 300 & n_row <= 400 ~ 7,
                           n_row > 400 & n_row <= 501 ~ 6,
                           n_row > 501 & n_row <= 602 ~ 5,
                           n_row > 602 & n_row <= 703 ~ 4,
                           n_row > 703 & n_row <= 804 ~ 3,
                           n_row > 804 & n_row <= 905 ~ 2,
                           n_row > 905 ~ 1)) |> 
  group_by(group) |> 
  mutate(n_row = 1:n()) |> 
  ungroup() |> 
  mutate(n_row = if_else(group %in% c(7:10), n_row + 1, n_row + 0)) |> 
  ggplot(aes(group, n_row)) +
  geom_text(aes(label = name), size = 5, hjust = 0, color = "grey25", family = "Lato") +
  scale_x_continuous(limits = c(1, 11)) +
  labs(title = "The Tuskegee Airmen",
       subtitle = "The first <span style='color:grey25'>**Black**</span> military aviators in the U.S. Army Air Corps<br>They confronted <span style='color:grey25'>**racism**</span> at home in addition to the enemy abroad during World War II",
       caption = "Visualization by Pablo Alvarez | Data from the Commemorative Air Force") +
  theme_void() +
  theme(plot.margin = margin(rep(20, 4)),
        plot.background = element_rect(color = "black", fill = "black"),
        panel.background = element_rect(color = "black", fill = "black"),
        plot.title = element_markdown(
          size = 120,
          color = "white",
          family = "Lato Black",
          margin = margin(rep(15, 3), 30),
          hjust = 0
        ),
        plot.subtitle = element_markdown(
          size = 50,
          color = "white",
          family = "Lato",
          margin = margin(10, 120, 15, 30),
          hjust = 0,
          lineheight = .4
        ),
        plot.caption = element_text(
          size = 28,
          color = "white",
          family = "Lato",
          margin = margin(t = 20, b = 10),
          hjust = .5
        )
  )

ggsave("tidytuesday_2022_w6.png", dpi = 320, width = 10, height = 12)

