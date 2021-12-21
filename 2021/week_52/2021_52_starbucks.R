library(pacman)
p_load(tidyverse, tidytuesdayR)

tt <- tt_load('2021-12-21')

starbucks <- tt$starbucks

starbucks |> glimpse()

df <- starbucks |> 
  select(product_name, size, serv_size_m_l, calories, sugar_g) |> 
  mutate(size = factor(size,
                       levels = c("short", "tall", "grande", "venti"),
                       ordered = TRUE))

df |> glimpse()

df |> 
  arrange(desc(sugar_g)) |> 
  slice_max(sugar_g, n = 1) |> 
  mutate(sugar_points = 1) |> 
  add_row(sugar_points = 2:89) |> 
  ggplot() +
  geom_jitter(aes(x = sugar_points, y = 1),
              width = 5,
              height = 15,
              size = 2) +
  geom_circle(aes(x0 = 50, y0 = 1, r = 50)) +
  coord_fixed()
  
  
  
  students %>% 
  ggplot() +
  geom_circle(aes(x0 = 1, y0 = 1, r = 0.3)) +
  geom_circle(aes(x0 = 2, y0 = 1, r = 0.3)) +
  geom_circle(aes(x0 = 3, y0 = 1, r = 0.3)) +
  geom_jitter(aes(x = student_class, y = 1),
              width = 0.2,
              height = 0.2,
              size = 2) +
  xlim(0.5, 3.5) +
  coord_fixed()
coord_fixed()