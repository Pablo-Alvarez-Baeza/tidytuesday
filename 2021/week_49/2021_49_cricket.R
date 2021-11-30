library(pacman)
p_load(tidyverse, tidytuesdayR)


# Get the data ------------------------------------------------------------

tt <- tidytuesdayR::tt_load('2021-11-30')
matches <- tt$matches

matches |> View()


