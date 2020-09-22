scrape_pl <- function() {
  
  ## Scrape Data
  
  # Load Libraries
  
  library(jsonlite)
  
  # Extract raw JSON data
  
  raw_data <- fromJSON("https://fantasy.premierleague.com/api/bootstrap-static/")
  df <- select(raw_data$elements, 
               web_name, element_type, team, now_cost, selected_by_percent,
               total_points, influence, creativity, threat, ict_index,
               chance_of_playing_next_round, points_per_game)
  
  # change numbers to factors
  
  colnames(df)[colnames(df) == "element_type"] <- "position"
  for(i in 1:nrow(raw_data$element_types)){
    df$position[df$position == raw_data$element_types$id[i]] <- raw_data$element_types$singular_name_short[i]
  }
  df$position <- as.factor(df$position)
  
  for(i in 1:nrow(raw_data$teams)){
    df$team[df$team == raw_data$teams$id[i]] <- raw_data$teams$short_name[i]
  }
  df$team <- as.factor(df$team)
  
  colnames(df)[colnames(df) == "now_cost"] <- "price"
  df$price <- as.numeric(df$price)
  
  colnames(df)[colnames(df) == "selected_by_percent"] <- "selected_by"
  df$selected_by <- as.numeric(df$selected_by)
  
  colnames(df)[colnames(df) == "web_name"] <- "player"
  
  colnames(df)[colnames(df) == "chance_of_playing_next_round"] <- "injury"
  df$injury[is.na(df$injury)] <- 100
  df$injury <- (100 - df$injury)/100
  
  df$influence <- as.numeric(df$influence)
  df$creativity <- as.numeric(df$creativity)
  df$threat <- as.numeric(df$threat)
  df$ict_index <- as.numeric(df$ict_index)
  df$points_per_game <- as.numeric(df$points_per_game)
  
  df$vapm <- (df$points_per_game-2)/(df$price/10)
  
  return(df)
  
}


