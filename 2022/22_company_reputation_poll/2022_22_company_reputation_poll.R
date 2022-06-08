library(pacman)
p_load(tidyverse, janitor, camcorder, showtext, ggrepel, scales)

gg_record(dir = "temp_22", device = "png", width = 3.375, height = 3.375, units = "in", dpi = 320)

font_add_google("Lato")
font_add(family = "Lato black",
         regular = "/Users/pabloalvarezbaeza/Library/Fonts/Lato-Black.ttf")

showtext_auto()


# Data --------------------------------------------------------------------
poll <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-31/poll.csv')
reputation <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-31/reputation.csv')

#write.csv(reputation, "reputation.csv")

big_five_companies <- c("Apple", "Amazon.com", "Google", "Facebook", "Microsoft")

reputation_filtered <- reputation |> 
  select(-score) |> 
  clean_names() |> 
  mutate(name = if_else(name == "P&S", "Products &\nServices", name),
         group = factor(if_else(company %in% big_five_companies, 1, 2)),
         company = case_when(company == "Amazon.com" ~ "Amazon",
                             company == "Facebook" ~ "Meta",
                             TRUE ~ company))


# Plot --------------------------------------------------------------------
reputation_filtered |>  
ggplot(aes(factor(str_to_title(name)), rank, group = company)) +
  geom_line(data = reputation_filtered |> filter(group == 2), size = .25, color = "grey75", alpha = .35) +
  geom_line(data = reputation_filtered |> filter(group == 1), aes(color = company), size = .5) +
  scale_color_manual(values = c("#0FB5AE", "#F68511", "#DE3D82", "#7E84FA", "#4046CA")) +
  geom_point(data = reputation_filtered |> filter(group == 1), aes(fill = company), shape = 21, size = 1.5, color = "white", stroke = .25) +
  scale_fill_manual(values = c("#0FB5AE", "#F68511", "#DE3D82", "#7E84FA", "#4046CA")) +
  geom_text_repel(
    data = reputation_filtered |> filter(group == 1, name == "VISION"),
    aes(color = company, label = company),
    family = "Lato",
    fontface = "bold",
    size = 7.5,
    direction = "y",
    xlim = c(7.5, NA),
    hjust = 0,
    segment.size = .25,
    segment.alpha = .5,
    segment.linetype = "dotted",
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20
  ) +
  coord_cartesian(
    clip = "off",
    expand = FALSE,
    xlim = c(1, 8.5)
  ) +
  scale_y_reverse(limits = c(100, 1),
                  breaks = c(1, 25, 50, 75, 100),
                  labels = label_ordinal()) +
                  #labels = c("1", rep("", 23), "25", rep("", 24), "50", rep("", 24), "75", rep("", 24), "100")) +
  geom_vline(
    xintercept = seq(1, 7, by = 1),
    color = "grey25", 
    size = .05,
    alpha = .5
  ) +
  geom_vline(
    xintercept = 100,
    color = "grey25", 
    size = .05,
  ) +
  geom_segment(
    data = tibble(y = seq(1, 100, by = 25), x1 = 1, x2 = 7),
    aes(x = x1, xend = x2, y = y, yend = y),
    inherit.aes = FALSE,
    color = "grey25",
    size = .05,
  ) +
  labs(y = "Rank",
       title = "The Big Five Tech Companies in the Eyes of Americans",
       subtitle = "Companies' reputation rank against the seven measures of the\nAxios-Harris Poll 100",
       caption = "Visualization by Pablo Alvarez | Data from the 2022 Axios-Harris Poll 100") +
  theme_minimal(base_family = "Lato") +
  theme(plot.margin = margin(rep(5, 4)),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.title.y = element_text(angle = 90, hjust = .99, color = "grey25", size = 20),
        axis.text = element_text(color = "grey25", size = 16, lineheight = .3),
        axis.text.x = element_text(margin = margin(t = 5)),
        panel.background = element_rect(color = "grey90", fill = "grey90"),
        plot.background = element_rect(color = "grey90", fill = "grey90"),
        legend.position = "none",
        plot.title = element_text(size = 28,
                                  color = "black",
                                  family = "Lato black"),
        plot.subtitle = element_text(size = 24,
                                     color = "black",
                                     family = "Lato",
                                     lineheight = .3,
                                     margin = margin(b = 10)),
        plot.caption = element_text(color = "grey25",
                               size = 16,
                               hjust = 0,
                               margin = margin(t = 10)))

ggsave("tidytuesday_2022_w22.png", width = 1080, height = 1080, units = "px", dpi = 320)

gg_playback(
  name = file.path("~/Desktop/Github/tidytuesday/2022/22_company_reputation_poll","tidytuesday_2022_w22_gif.gif"),
  first_image_duration = 4,
  last_image_duration = 18,
  frame_duration = .18
)

