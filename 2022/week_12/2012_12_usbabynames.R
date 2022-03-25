library(pacman)
p_load(tidyverse, showtext, ggtext, shadowtext, scales, camcorder)

gg_record(dir = "temp", device = "png", width = 16.5, height = 11.7, units = "in", dpi = 320)

font_add_google("Lato")
font_add(family = "Lato black",
         regular = "Lato-Black.ttf")

babynames <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-22/babynames.csv')

 
# https://disneymovieslist.com/list-of-disney-princess-movies/
princesses <- babynames |> 
  filter(sex == "F",
         name %in% c("Snow", "Cinderella", "Aurora", "Ariel", "Belle",
                     "Jasmine", "Pocahontas", "Mulan", "Tiana",
                     "Rapunzel", "Merida", "Elsa", "Moana"),
         year >= 1937) |> 
  group_by(year) |> 
  summarize(total = sum(n)) |> 
  ungroup() 

# Final plot
ggplot(princesses, aes(x = year, y = total)) +
  geom_area(fill = "white", color = "#F4A7C2", size = 2) +
  scale_x_continuous(limits = c(1937, 2018),
                     breaks = seq(1937, 2017, by = 10)) +
  scale_y_continuous(labels = scales::comma) +
  coord_cartesian(expand = c(0, 0)) +
  theme_minimal(base_family = "Lato") +
  labs(title = "The <span style='color:#F4A7C2'>Disney Princess Effect </span> on American Baby Names",
       subtitle = "Since the release of Snow White, a great number of moms and dads have named their daughters<br>after Disney princesses",
       caption = "Visualization by Pablo Alvarez | Data from the 'babynames' R package from Hadley Wickham",
       x = NULL,
       y = "Number of female babies born in the U.S.") +
  geom_text(aes(x = 1938, y = 3800, label = "Snow White\nSnow"), color = "white", size = 4, hjust = 0, family = "Lato") +
  annotate(geom = "segment", x = 1937, xend = 1937, y = 0, yend = 4200, size = .25, color = "grey50") +
  annotate(geom = "segment", x = 1937, xend = 1937.5, y = 4200, yend = 4200, size = .25, color = "grey50") +
  geom_text(aes(x = 1950, y = 3800, label = "Cinderella"), color = "white", size = 4, hjust = 0, family = "Lato") +
  annotate(geom = "segment", x = 1950, xend = 1950, y = 0, yend = 3600, size = .25, color = "grey50") +
  geom_text(aes(x = 1959, y = 3800, label = "Sleeping Beauty\nAurora"), color = "white", size = 4, hjust = 0, family = "Lato") +
  annotate(geom = "segment", x = 1959, xend = 1959, y = 0, yend = 3200, size = .25, color = "grey50") +
  geom_text(aes(x = 1989, y = 15000, label = "The Little Mermaid\nAriel"), color = "white", size = 4, hjust = 1, family = "Lato") +
  annotate(geom = "segment", x = 1989, xend = 1989, y = 0, yend = 14400, size = .25, color = "grey50") +
  geom_text(aes(x = 1991, y = 12000, label = "Beauty and the Beast\nBelle"), color = "black", size = 4, hjust = 0, family = "Lato") +
  annotate(geom = "segment", x = 1991, xend = 1991, y = 0, yend = 11400, size = .25, color = "grey50") +
  geom_text(aes(x = 1992, y = 7000, label = "Aladdin\nJasmine"), color = "black", size = 4, hjust = 0, family = "Lato") +
  annotate(geom = "segment", x = 1992, xend = 1992, y = 0, yend = 6400, size = .25, color = "grey50") +
  geom_text(aes(x = 1995, y = 3000, label = "Pocahontas"), color = "black", size = 4, hjust = 0, family = "Lato") +
  annotate(geom = "segment", x = 1995, xend = 1995, y = 0, yend = 2800, size = .25, color = "grey50") +
  geom_text(aes(x = 1998, y = 1500, label = "Mulan"), color = "black", size = 4, hjust = 0, family = "Lato") +
  annotate(geom = "segment", x = 1998, xend = 1998, y = 0, yend = 1300, size = .25, color = "grey50") +
  geom_text(aes(x = 2000, y = 16000, label = "Disney launched its\nprincess campaign"), color = "#F4A7C2", size = 4, hjust = 0, family = "Lato") +
  annotate(geom = "segment", x = 2000, xend = 2000, y = 0, yend = 15400, size = .25, color = "#F4A7C2", linetype = "dashed") +
  geom_text(aes(x = 2009, y = 5000, label = "The Princess and the Frog\nTiana"), color = "black", size = 4, hjust = 1, family = "Lato") +
  annotate(geom = "segment", x = 2009, xend = 2009, y = 0, yend = 4400, size = .25, color = "grey50") +
  geom_text(aes(x = 2010, y = 7000, label = "Tangled\nRapunzel"), color = "black", size = 4, hjust = 1, family = "Lato") +
  annotate(geom = "segment", x = 2010, xend = 2010, y = 0, yend = 6400, size = .25, color = "grey50") +
  geom_text(aes(x = 2012, y = 12000, label = "Brave\nMerida"), color = "white", size = 4, hjust = 1, family = "Lato") +
  annotate(geom = "segment", x = 2012, xend = 2012, y = 0, yend = 11400, size = .25, color = "grey50") +
  geom_text(aes(x = 2013, y = 15000, label = "Frozen\nElsa"), color = "white", size = 4, hjust = 1, family = "Lato") +
  annotate(geom = "segment", x = 2013, xend = 2013, y = 0, yend = 14400, size = .25, color = "grey50") +
  geom_text(aes(x = 2016, y = 13000, label = "Moana"), color = "white", size = 4, hjust = 1, family = "Lato") +
  annotate(geom = "segment", x = 2016, xend = 2016, y = 0, yend = 12800, size = .25, color = "grey50") +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "#003170", color = "#003170"),
    panel.background = element_rect(fill = "#003170", color = "#003170"),
    plot.margin = margin(rep(40, 4)),
    axis.title.y = element_text(color = "white", angle = 90, size = 12, hjust = .79, margin = margin(r =10)),
    axis.text = element_text(color = "white"),
    axis.text.x = element_text(margin = margin(t = 10),
                               size = 12),
    axis.text.y = element_text(margin = margin(r = 5),
                               size = 12),
    plot.title = element_markdown(color = "white",
                              size = 36,
                              family = "Lato black"),
    plot.subtitle = element_markdown(color = "white",
                                 size = 22,
                                 lineheight = 1.25,
                                 margin = margin(t = 20, b = 100)),
    plot.caption = element_text(color = "white",
                                size = 12,
                                margin = margin(t = 60),
                                hjust = .5)
  )

ggsave("tidytuesday_2022_w2.png", width = 16.5, height = 11.7, units = "in", dpi = 320)


# Small multiples ---------------------------------------------------------
princesses_2 <- babynames |> 
  filter(sex == "F",
         name %in% c("Snow", "Cinderella", "Aurora", "Ariel", "Belle",
                     "Jasmine", "Pocahontas", "Mulan", "Tiana",
                     "Rapunzel", "Merida", "Elsa", "Moana"),
         year >= 1937) 
  

princess <- function(princess_name = "x",
                     princess_year = 1) {
  babynames |> 
    filter(sex == "F",
           name == princess_name,
           year %in% princess_year) |> 
    mutate(pct = round(n/lag(n)/10 * 100, 0)) |> 
    replace_na(list(pct = 0))
}

snow_white <- princess(princess_name = "Snow",
                       princess_year = c(1937:1938)) |> 
  add_row(year = 1935, sex = "F", name = "Snow", n = 0, prop = 0)

cinderella <- princess(princess_name = "Cinderella",
         princess_year = c(1950:1951)) 

sleeping_beauty <- princess(princess_name = "Aurora",
                       princess_year = c(1959:1960))

ariel <- princess(princess_name = "Ariel",
                            princess_year = c(1989:1990))

belle <- princess(princess_name = "Belle",
                  princess_year = c(1991:1992))

jasmine <- princess(princess_name = "Jasmine",
                  princess_year = c(1992:1993))

pocahontas <- princess(princess_name = "Pocahontas",
                    princess_year = c(1995:1996))  

mulan <- princess(princess_name = "Mulan",
                       princess_year = c(1998:1999))

tiana <- princess(princess_name = "Tiana",
                  princess_year = c(2009:2010))

rapunzel <- princess(princess_name = "Rapunzel",
                  princess_year = c(2010:2011))

merida <- princess(princess_name = "Merida",
                     princess_year = c(2012:2013))

elsa <- princess(princess_name = "Elsa",
                   princess_year = c(2013:2014))

moana <- princess(princess_name = "Moana",
                 princess_year = c(2016:2017))

princesses_comparison <- bind_rows(snow_white, cinderella,
                                   sleeping_beauty, ariel,
                                   belle, jasmine,
                                   pocahontas, mulan,
                                   tiana, rapunzel,
                                   merida, elsa, moana) |> 
  select(-c(sex, prop)) |> 
  group_by(name) |> 
  mutate(period = c(1:n())) |>
  group_by(period) |> 
  arrange(-n) |> 
  mutate(princess_number = as.double(1:n())) |> 
  group_by(name) |> 
  mutate(princess_number = if_else(period == 1, princess_number -.1, princess_number)) |> 
  ungroup() 


princesses_comparison |> 
  ggplot(aes(princess_number, n)) +
  geom_col(data = princesses_comparison |> filter(period == 2),
           width = .5, fill = "#F4A7C2", color = "black") +
  geom_col(data = princesses_comparison |> filter(period == 1),
           width = .5, fill = "grey75", color = "black") +
  scale_y_continuous(breaks = seq(0, 12000, by = 2000),
                                  labels = scales::comma) +
  scale_x_continuous(breaks = seq(1, 11, by = 1),
                     labels = c("Jasmine", "Ariel", "Elsa", "Tiana", "Aurora", "Moana",
                                "Merida", "Cinderella", "Belle", "Mulan", "Snow")) +
  coord_cartesian(expand = c(0, 0)) +
  theme_minimal(base_family = "Lato") +
  labs(title = "American Baby Girl Names <span style='color:grey75'>before </span> and <span style='color:#F4A7C2'>after </span> Disney Princess Movies",
       subtitle = "In the <span style='color:#F4A7C2'>year following </span> a Disney princess movie, princess names are more popular",
       caption = "Visualization by Pablo Alvarez | Data from the 'babynames' R package from Hadley Wickham",
       x = NULL,
       y = "Number of girls born in the U.S. with the corresponding princess name") +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "#003170", color = "#003170"),
    panel.background = element_rect(fill = "#003170", color = "#003170"),
    plot.margin = margin(rep(40, 4)),
    axis.title.y = element_text(color = "white", angle = 90, size = 12, hjust = 1, margin = margin(r =10)),
    axis.text = element_text(color = "white"),
    axis.text.x = element_text(margin = margin(t = 10),
                               size = 12),
    axis.text.y = element_text(margin = margin(r = 20),
                               size = 12),
    plot.title = element_markdown(color = "white",
                                  size = 32,
                                  family = "Lato black"),
    plot.subtitle = element_markdown(color = "white",
                                     size = 20,
                                     lineheight = 1.25,
                                     margin = margin(t = 10, b = 100)),
    plot.caption = element_text(color = "white",
                                size = 12,
                                margin = margin(t = 60),
                                hjust = .5)
  )

ggsave("tidytuesday2_2022_w2.png", width = 16.5, height = 11.7, units = "in", dpi = 320)
