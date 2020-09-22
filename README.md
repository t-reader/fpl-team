# fpl-team
Optimise fantasy football team selection

# extract_data.R
gets data from Fantasy Premier League api and turns into a dataframe.

# filter.R
filter data frame obtained from extract_data.R by team, position and price and choose whether it sorts the dataframe.

# my_knapsack2.R
used in position.R, an modified version of the knapsack function from the adagio package to solve the 0-1 or binary single knapsack problem, but making sure it is optimised with an exact number of items.

# position.R
takes a dataframe of players from extract_data, usually filtered to one position, and optimises the choice of N players. (by default optimises by total points)

# squad.R
takes a dataframe of players to make an optimised squad, to a budget. (by default optimises by total points)
