position <- function(players, N, opt_by = "total_points"){
  
  m <- matrix(ncol = N + 2, nrow = 0)
  c <- 0
  
  min <- sum(tail(players[order(players$price, decreasing = T),], N)$price)
  max <- sum(head(players[order(players$price, decreasing = T),], N)$price)
  prices <- seq(min, max, by = 1)
  
  for(i in 1:length(prices)){
    x <- my_knapsack2(players, prices[i], N, opt_by)
    y <- sum(players[x,]$price)
    p <- sum(players[x,]$total_points)
    z <- c(x,y,p)
    
    if(!(z[N + 2] %in% m[,(N + 2)])){
      m <- rbind(m, z)
      c <- c + 1
    }
  }
  return(m)
}