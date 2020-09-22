library(adagio)
library(tidyverse)
library(dplyr)

source("extract_data.R")
source("my_knapsack2.R")
source("squad.R")
source("position.R")
source("filter.R")
source("top11.R")

all_players <- scrape_pl()

top_squad <- squad(all_players, n_gkp = 1, budget = 960)

gw1 <- top11(all_players)

top_scorers <- all_players[order(all_players$total_points, decreasing = T), ]
