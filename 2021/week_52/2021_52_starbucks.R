library(pacman)
p_load(tidyverse, tidytuesdayR, showtext, ggtext)

tt <- tt_load('2021-12-21')

starbucks <- tt$starbucks

starbucks |> glimpse()

df <- starbucks |> 
  select(product_name, size, serv_size_m_l, calories, sugar_g) |> 
  mutate(size = factor(size,
                       levels = c("short", "tall", "grande", "venti"),
                       ordered = TRUE),
         sugar_who = factor(if_else(sugar_g > 50, 1, 2)))

df |> 
  slice_max(sugar_g, n = 1) |> View()

df |> 
  count(sugar_who) |> 
  summarize(total = sum(n),
            round(n / total, 2) * 100)

df |> 
  ggplot(aes(calories, sugar_g, color = sugar_who)) +
  geom_point(size = .5) +
  scale_x_continuous(limits = c(0, 650),
                     labels = scales::label_number(suffix = " KCal")) +
  scale_y_continuous(limits = c(0, 100),
                     labels = scales::label_number(suffix = "mg")) +
  scale_color_manual(values = c("1" = "red", "2" = "white")) +
  labs(x = "Calories",
       y = "Sugar grams",
       title = "<span style='color:red'>23% of Starbuck drinks</span>exceed the maximum amount of sugar recommended by WHO for adults and children",
       subtitle = "",
       caption = "Visualization by Pablo Alvarez • Data from Starbucks Coffee Company | 2021 - Week 52<br>Each bubble represents a different spider genus.") +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    plot.margin = margin(20, 40, 20, 40),
    plot.background = element_rect(fill = "black", color = "black"),
    panel.background = element_rect(fill = "black", color = "black"),
    axis.text.x = element_text(color = "white", size = 4),
    axis.text.y = element_text(color = "white", size = 4),
    # Customize title appearance
    plot.title = element_markdown(
      color = "white", 
      size = 32, 
      face = "bold",
      margin = margin(t = 15)
    ),
    # Customize subtitle appearance
    plot.subtitle = element_markdown(
      color = "white",
      size = 26,
      lineheight = 1.35,
      margin = margin(t = 15, b = 40)
    ),
    # Title and caption are going to be aligned
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.caption = element_markdown(
      color = "white", 
      size = 13,
      lineheight = 1.2, 
      hjust = 0,
      margin = margin(t = 40) # Large margin on the top of the caption.
  )
  )

ggsave("tidytuesday_2021_w52.png", width = 12, height = 10, units = "in", dpi = 320)

