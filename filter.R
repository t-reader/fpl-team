filter <- function(data = df, pos = F, team = F, max_price = F, sort = F){
  if(sort != F){
    data <- data[order(data[, sort], decreasing = T),]
  }
  
  if(pos != F){
    data <- data[which(data$position == pos),]
  }
  
  if(team != F){
    data <- data[which(data$team == team),]
  }
  
  if(max_price != F){
    data <- data[which(data$price <= max_price),]
  }
  
  return(data)
}
