library(pacman)
p_load(tidyverse, tidytuesdayR)

tt <- tidytuesdayR::tt_load('2022-01-25')

ratings <- tt$ratings
details <- tt$details


 categories <- details |> 
   rename(category = boardgamecategory) |> 
   filter(!is.na(category)) |> 
   select(id, category) |> 
   mutate(category = str_sub(category, 2, -2),
          n_categories = ifelse(is.na(category), 0,
                                 lengths(str_split(category, ","))))   
   
 
categories |> 
   separate(category, paste0("cat", 1:max(categories$n_categories)), ",") |> 
  select(-n_categories) %>% 
  pivot_longer(!id, names_to = "category", values_drop_na = TRUE) |> 
  select(id, category = value) %>% 
  mutate(category = parse_character(category, trim_ws = TRUE)) %>% 
  mutate(category = str_sub(category, 2, -2))


details |>  
  filter(!is.na(category)) |> 
  select(id, category) |> 
  mutate(category = str_sub(category, 2, -2)) |> 
  mutate(n_categories = ifelse(is.na(category), 0,
                                lengths(str_split(category, ",")))) |> 
  separate(category, into = paste0("cat", 1:max(n_categories)), ",") |> 
  select(-nb_categories) %>% 
  pivot_longer(!id, names_to = "category", values_drop_na = TRUE) %>% 
  select(id, category = value) %>% 
  mutate(category = parse_character(category, trim_ws = TRUE)) %>% 
  mutate(category = str_sub(category, 2, -2))

ratings |> glimpse()
details |> glimpse()

ratings |> 
  slice_min(rank, n = 10) |> View()

df <- inner_join(ratings, details, by = "id")

df |> 
  group_by(playingtime, r) |> 
  arrange(desc(average)) |> View()


df |> 
  ggplot(aes(year, average)) +
  geom_line()

details |> 
  mutate(minplayers = factor(minplayers)) |> 
  group_by(minplayers) |> 
  count(yearpublished, sort = TRUE) |> 
  filter(yearpublished >= 1900,
         yearpublished <= 2021) |>
  ungroup() |> 
  ggplot(aes(yearpublished, n, color = minplayers)) +
  geom_line() +
  scale_x_continuous(breaks = seq(1900, 2025, 1))

ratings |> 
  filter(year >= 1900,
         year <= 2021,
         str_detect(name, "Monopoly")) |> 
  group_by(year) |> 
  #count(year, sort = TRUE) |> 
  ungroup() |> 
  ggplot(aes(year, average)) +
  geom_line() +
  scale_x_continuous(breaks = seq(1900, 2025, 1))


ratings |> 
  filter(year >= 1900,
         year <= 2021) |> 
  group_by(year) |> 
  #count(year, sort = TRUE) |> 
  summarize(mean = mean(average)) |> 
  ungroup() |> 
  ggplot(aes(year, mean)) +
  geom_line() 
  

details |> 
  group_by()