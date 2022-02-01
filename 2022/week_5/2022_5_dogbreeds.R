library(pacman)
p_load(tidyverse, janitor, showtext, ggtext)

font_add(family = "Regular",
         regular = "Abdullah-rggX7.otf")
font_add_google("Lato")


breed_traits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv') |> 
  clean_names()


dalmatians <- breed_traits |> 
  filter(breed == "Dalmatians") |> 
  select(-c(coat_type, coat_length)) |> 
  pivot_longer(cols = -breed, names_to = "trait", values_to = "score") |> 
  arrange(desc(score)) 


# x0 = x axis position
# y0 = y axis position
# a = width
# b = height
# m1 = shape

# Final plot
ggplot() +
  geom_ellipse(aes(x0 = 0, y0 = 0, a = 5, b = 5, angle = 0, m1 = 3), fill = "black") + 
  geom_ellipse(aes(x0 = 7, y0 = 9, a = 4, b = 4, angle = 1, m1 = 3), fill = "black") +
  geom_ellipse(aes(x0 = 0, y0 = 18, a = 4, b = 4, angle = 2, m1 = 3), fill = "black") +
  geom_ellipse(aes(x0 = 10, y0 = 25, a = 4, b = 4, angle = 1, m1 = 3), fill = "black") +
  geom_ellipse(aes(x0 = 12, y0 = 0, a = 4, b = 4, angle = 1, m1 = 3), fill = "black") +
  geom_ellipse(aes(x0 = 20, y0 = 10, a = 4, b = 4, angle = 2, m1 = 3), fill = "black") +
  geom_ellipse(aes(x0 = 20, y0 = 25, a = 4, b = 4, angle = 0, m1 = 3), fill = "black") +
  geom_ellipse(aes(x0 = 30, y0 = 0, a = 4, b = 4, angle = 2, m1 = 3), fill = "black") +
  geom_ellipse(aes(x0 = 30, y0 = 15, a = 4, b = 4, angle = 2, m1 = 3), fill = "black") +
  geom_ellipse(aes(x0 = 0, y0 = 30, a = 3, b = 3, angle = 0, m1 = 3), fill = "black") +
  geom_ellipse(aes(x0 = 13, y0 = 16, a = 3, b = 3, angle = 2, m1 = 3), fill = "black") +
  geom_ellipse(aes(x0 = 21, y0 = 0, a = 3, b = 3, angle = 1, m1 = 3), fill = "black") +
  geom_ellipse(aes(x0 = 28, y0 = 25, a = 2, b = 2, angle = 2, m1 = 3), fill = "black") +
  geom_ellipse(aes(x0 = 16, y0 = 32, a = 2, b = 2, angle = 2, m1 = 3), fill = "black") +
  ylim(-10, 55) +
  xlim(-6, 36) +
  annotate(geom = "segment", x = -6, xend = 36, y = 50, yend = 50, color = "black", size = 10) +
  annotate(geom = "richtext", x = 15, y = 49.5, hjust = .5, label = "Dalmatian <span style=color:red>Traits</span>", color = "white", family = "Abdullah", fill = NA, label.color = NA) +
  annotate(geom = "text", x = 15, y = 43, hjust = .5, label = "Highly energetic and playful, Dalmatians thrive on human companionship.\nThese dogs can be well trained and make good watchdogs.", color = "black", family = "Lato", size = 1) +
  annotate(geom = "text", x = 0.25, y = -8, hjust = 0, label = "Affectionate with family\n5/5", color = "red", size = 1, family = "Lato", fontface = "bold") +
  annotate(geom = "text", x = 28.25, y = 30, hjust = 0, label = "Drooling level\n2/5", color = "red", size = 1, family = "Lato", fontface = "bold") +
  annotate(geom = "text", x = 8.25, y = 32, hjust = 0, label = "Playfulness level\n4/5", color = "red", size = 1, family = "Lato", fontface = "bold") +
  annotate(geom = "text", x = 32.25, y = -7, hjust = 0, label = "Energy Level\n4/5", color = "red", size = 1, family = "Lato", fontface = "bold") +
  annotate(geom = "text", x = 28.25, y = 8, hjust = 0, label = "Watchdog Protective Nature\n4/5", color = "red", size = 1, family = "Lato", fontface = "bold") +
  annotate(geom = "segment", x = 8, xend = 8, y = 28, yend = 32, color = "red", size = .09) +
  annotate(geom = "segment", x = 28, xend = 28, y = 26, yend = 30, color = "red", size = .09) +
  annotate(geom = "segment", x = 0, xend = 0, y = -8, yend = -4, color = "red", size = .09) +
  annotate(geom = "segment", x = 32, xend = 32, y = -7, yend = -3, color = "red", size = .09) +
  annotate(geom = "segment", x = 28, xend = 28, y = 8, yend = 12, color = "red", size = .09) +
  labs(caption = "Visualization by Pablo Alvarez | Data from  American Kennel Club") +
  theme_void() +
  theme(plot.margin = margin(rep(5, 4)),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.caption = element_text(
          color = "black",
          size = 2.5,
          hjust = 0.5,
          family = "Lato",
          margin = margin(t = 5, b = 2)
        )
  )

ggsave("tiytuesday_2022_w5.png", width = 900, height = 900, units = "px", dpi = 320)