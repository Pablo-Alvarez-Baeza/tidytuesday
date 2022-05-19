library(pacman)
p_load(tidyverse, janitor, showtext)

# Fonts
font_add(family = "Public Sans Thin",
         regular = "PublicSans-Thin.ttf")

font_add(family = "Dubois Wide",
         regular = "VTCDuBoisTrial-LightWide.ttf")

font_add(family = "Dubois Bold",
         regular = "VTCDuBoisTrial-Bold.ttf")

showtext_auto()

# Data
plate6 <- read_csv('https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/challenge/2022/challenge06/data.csv')

plate6 <- plate6 |> 
  clean_names() |> 
  mutate(iliteracy_rate = case_when(iliteracy_rate == 67.2 ~ 67.27,
                                    TRUE ~ iliteracy_rate),
         iliteracy_rate_rev = rev(iliteracy_rate),
         y_height =  rev(seq(50, 90, by = 10))) 

# Plot
plate6 |> 
ggplot(aes(iliteracy_rate, y_height + .9)) +
  geom_col(width = 1.5, fill = "black") +
  scale_x_reverse(limits = c(120, 45),
                  breaks = plate6$iliteracy_rate_rev,
                  labels = c( paste0("(", plate6$iliteracy_rate_rev[1], "%?)"), paste0(plate6$iliteracy_rate_rev[2:5], "%"))) +
  coord_cartesian(expand = c(0, 0),
                  clip = "off") +
  geom_segment(aes(x = iliteracy_rate + .7, xend= 101, y=y_height, yend=y_height),
               size = 2.2, color = "black") +
  geom_segment(aes(x = iliteracy_rate + .7, xend= 101, y=y_height, yend=y_height),
               size = 2, color = "#dccaba") +
  scale_y_continuous(limits = c(0, 110),
                     breaks = plate6$y_height,
                     labels = c(plate6$date[1:4], "(1900?)"),
                     expand = c(0, 0)) +
  annotate(geom = "text", x = 106, y = 0, label = "PERCENT OF\nILLITERACY.", size = 4.5, family = "Dubois Wide", lineheight = .3) +
  annotate(geom = "text", x = 75, y = 105, label = "ILLITERACY.", size = 10, family = "Dubois Bold") +
  labs(caption = "Visualization by Pablo Alvarez | #DuBoisChallenge2022") +
  theme_minimal() +
  theme(
    plot.margin = margin(rep(10, 4)),
    panel.background = element_rect(fill = "#dccaba", color = "#dccaba"),
    plot.background = element_rect(fill = "#dccaba", color = "#dccaba"),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_text(margin = margin(r = -60),
                               family = "Public Sans Thin",
                               size = 24),
    axis.text.x = element_text(margin = margin(t = -1.5),
                               family = "Public Sans Thin",
                               size = 18.5,
                               hjust = .5),
    plot.caption = element_text(
      margin = margin(t = 10),
      size = 15,
      family = "Public Sans Thin",
      hjust = .5
    )
  ) 

ggsave("2022_7_duboischallenge.png", width = 1000, height = 1350, units = "px", dpi = 320)
