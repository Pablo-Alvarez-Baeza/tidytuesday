library(pacman)
p_load(tidyverse, igraph, ggraph, showtext, ggtext)


# Fonts -------------------------------------------------------------------
font_add_google("Lato")
font_add(family = "Lato black",
         regular = "Lato-Black.ttf")
font_add(family = "Lato bold",
         regular = "Lato-Bold.ttf")
showtext_auto()


# Data --------------------------------------------------------------------
erasmus <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-08/erasmus.csv')
#https://data.europa.eu/data/datasets?locale=en&catalog=eac&query=erasmus&page=1&sort=issued%2Bdesc,%20relevance%2Bdesc,%20title.en%2Basc
#https://english.elpais.com/elpais/2012/11/28/inenglish/1354114165_335994.html

# Analysis ----------------------------------------------------------------
# Spain, together with France, is the largest receiving country
erasmus |> 
  filter(sending_country_code != receiving_country_code) |> 
  distinct(sending_country_code, receiving_country_code) |> 
  count(receiving_country_code) |>  
  arrange(-n)

erasmus_spain <- erasmus |> 
  filter(sending_country_code != receiving_country_code,
         receiving_country_code == "ES") |> 
  count(sending_country_code, receiving_country_code, wt = participants, name = "students") |> 
  mutate(across(contains("country_code"), countrycode::countrycode,
                origin = "eurostat", destination = "country.name"),
                sending_country_code = case_when(sending_country_code == "Bosnia & Herzegovina" ~ "Bosnia\n& Herzegovina",
                                                 sending_country_code == "North Macedonia" ~ "North\nMacedonia",
                                                 sending_country_code == "Palestinian Territories" ~ "Palestinian\nTerritories",
                                                 sending_country_code == "United Kingdom" ~ "United\nKingdom",
                                                 TRUE ~ sending_country_code),
         receiving_country_code = if_else(receiving_country_code == "Spain", "SPAIN", receiving_country_code)) |> 
  arrange(sending_country_code) |> 
  graph_from_data_frame(directed = FALSE)


# Graph -------------------------------------------------------------------
erasmus_spain |> 
  ggraph(layout = "linear", circular = TRUE) +
  geom_edge_arc(aes(edge_width = students), edge_color = "white") +
  geom_node_text(aes(label = name), size = 4, family = "Lato bold", lineheight = .25) +
  labs(title = "<span style='color:white;font-size:32pt'>SPAIN:</span> the destination most preferred by European Erasmus students",
       subtitle = "The weather, the social life, and the widespread belief that Spaniards are open and friendly draw thousands of foreigners every year",
       caption = "Visualization by Pablo Alvarez | Data from Data.Europa") +
  theme_void() +
  theme(plot.margin = margin(rep(10, 4)),
        panel.background = element_rect(fill = "#F93822", color = "#F93822"),
        plot.background = element_rect(fill = "#F93822", color = "#F93822"),
        legend.position = "none",
        plot.title = element_markdown(family = "Lato black",
                                  size = 24, hjust = .5),
        plot.subtitle = element_text(family = "Lato",
                                  size = 12.5,
                                  margin = margin(t = 2, b = 10)),
        plot.caption = element_text(family = "Lato",
                                  size = 12,
                                  hjust = .5)
  )
  
ggsave("tidytuesday_2022_w10.png", width = 1080, height = 1080, units = "px", dpi = 320)
