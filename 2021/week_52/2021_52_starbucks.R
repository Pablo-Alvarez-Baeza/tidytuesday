library(pacman)
p_load(tidyverse, tidytuesdayR, showtext, ggtext)

font_add_google("Lato")
showtext_auto()

tt <- tt_load('2021-12-21')

starbucks <- tt$starbucks

starbucks |> glimpse()

df <- starbucks |> 
  select(product_name, size, serv_size_m_l, calories, sugar_g) |> 
  mutate(sugar_who = factor(if_else(sugar_g > 50, 1, 2)))

# Drinks with more than 50 grams of sugar
df |> 
  count(sugar_who) |> 
  summarize(total = sum(n),
            round(n / total, 2) * 100)

theme_set(theme_minimal(base_family = "Lato"))


df |> 
  ggplot(aes(calories, sugar_g, color = sugar_who, fill = sugar_who)) +
  geom_point(aes(stroke = sugar_who), size = .5, shape = 21) +
  scale_x_continuous(limits = c(0, 650),
                     labels = scales::label_number(suffix = " KCal")) +
  scale_y_continuous(limits = c(0, 100),
                     labels = scales::label_number(suffix = " g")) +
  scale_color_manual(values = c("1" = "white", "2" = "grey25")) +
  scale_fill_manual(values = c("1" = "white", "2" = "black")) +
  scale_discrete_manual(aesthetics = "stroke", values = c("1" = .8, "2" = .5)) +
  labs(x = "Calories",
       y = "Sugar grams",
       title = "<span style='font-size:36pt'>**23%**</span> of Starbucks drinks exceed WHO recommendations for daily sugar intake",
       subtitle = "268 drinks contained more than 50 grams of free sugar.",
       caption = "Visualization by Pablo Alvarez • Data from Starbucks Coffee Company | TidyTuesday 2021 - Week 52<br>Sugar intake recommendations based on a 2,000 calorie diet.") +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    plot.margin = margin(20, 40, 20, 40),
    plot.background = element_rect(fill = "black", color = "black"),
    panel.background = element_rect(fill = "black", color = "black"),
    axis.text.x = element_text(color = "white", size = 7),
    axis.text.y = element_text(color = "white", size = 7),
    # Customize title appearance
    plot.title = element_markdown(
      color = "white", 
      size = 16, 
      face = "bold",
      margin = margin(t = 15)
    ),
    # Customize subtitle appearance
    plot.subtitle = element_markdown(
      color = "white",
      size = 14,
      margin = margin(t = 15, b = 40)
    ),
    # Title and caption are going to be aligned
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.caption = element_markdown(
      color = "white", 
      size = 8,
      lineheight = 1.4, 
      hjust = 0.5,
      margin = margin(t = 40) # Large margin on the top of the caption.
  )
  )

ggsave("tidytuesday_2021_w52.png", width = 10, height = 10, units = "in", dpi = 320)

