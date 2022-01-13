library(pacman)
p_load(tidyverse, tidytuesdayR)

tt <- tidytuesdayR::tt_load('2022-01-11')

colony <- tt$colony

colony |> glimpse()

colony |> 
  mutate(state = factor(state)) |> 
  group_by(state) |> 
  slice_max(colony_lost_pct, n = 1) |> View()
