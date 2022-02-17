library(pacman)
p_load(tidyverse, janitor, showtext)

font_add_google("Public Sans")
font_labels <- font_add(family = "Public Sans Thin",
         regular = "PublicSans-Thin.ttf")
font_title <- font_add(family = "Public Sans Black",
         regular = "PublicSans-Black.ttf")

showtext_auto()


plate6 <- read_csv('https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/challenge/2022/challenge06/data.csv')

plate06

d <- rev(seq(50, 90, by = 10))
rev(d)

plate6 <- plate6 |> 
  clean_names() |> 
  mutate(iliteracy_rate_rev = rev(iliteracy_rate),
         y_height =  rev(seq(50, 90, by = 10))) 

plate6 |> 
ggplot(aes(iliteracy_rate, y_height + .9)) +
  geom_col(width = 3, fill = "black") +
  scale_x_reverse(limits = c(120, 0),
                  breaks = plate6$iliteracy_rate_rev,
                  labels = c( paste0("(", plate6$iliteracy_rate_rev[1], "%?)"), paste0(plate6$iliteracy_rate_rev[2:5], "%"))) +
  coord_cartesian(expand = c(0, 0),
                  clip = "off") +
  geom_segment(aes(x = iliteracy_rate + 1.5, xend= 102.5, y=y_height, yend=y_height),
               size = 2.2, color = "black") +
  geom_segment(aes(x = iliteracy_rate + 1.5, xend= 102.5, y=y_height, yend=y_height),
               size = 2, color = "#dccaba") +
  scale_y_continuous(limits = c(0, 110),
                     breaks = plate06$y_height,
                     labels = c(plate06$date[1:4], "(1900?)"),
                     expand = c(0, 0)) +
  annotate(geom = "text", x = 115, y = 0, label = "PERCENT OF\nILLITERACY.", size = 5, family = "Public Sans Thin", lineheight = .3) +
  annotate(geom = "text", x = 65, y = 105, label = "ILLITERACY.", size = 10, family = "Public Sans Black") +
  theme_minimal() +
  theme(
    plot.margin = margin(10, 0, 10, 50),
    panel.background = element_rect(fill = "#dccaba", color = "#dccaba"),
    plot.background = element_rect(fill = "#dccaba", color = "#dccaba"),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_text(margin = margin(r = -24),
                               family = "Public Sans Thin",
                               size = 24),
    axis.text.x = element_text(margin = margin(t = -1.5),
                               family = "Public Sans Thin",
                               size = 13,
                               hjust = .5)
  ) 

ggsave("2022_7_duboischallenge.png", width = 1000, height = 1350, units = "px", dpi = 320)
