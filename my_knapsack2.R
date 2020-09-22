my_knapsack2 <- function(df, W, L, opt_by = "selected_by"){
  
  if(L == 1){
    newdf <- df[which(df[, "price"] <= W),]
    player <- newdf[which(newdf[, opt_by] == max(newdf[, opt_by]))[1],]
    ind <- which(df$player == player$player & df$price == player$price & df$total_points == player$total_points)
    return(ind)
  }
  
  V <- max(df[, opt_by])
  n <- nrow(df)
  p <- df$price + (W + 1)
  s <- df[, opt_by] + (n * (V + 1))
  W <- W + (L * (W + 1))
  
  x <- logical(n)
  E <- matrix(0, nrow = W + 1, ncol = n)
  G <- matrix(0, nrow = W + 1, ncol = 1)
  
  for(k in 1:n) {
    E[, k] <- G
    H <- c(numeric(p[k]), G[1:(W + 1 - p[k]), 1] + s[k])
    G <- pmax(G, H)
  }
  
  fmax <- G[W + 1, 1]
  e <- fmax
  j <- W + 1
  
  for(k in n:1) {
    if (E[j,k] < e) {
      x[k] <- TRUE
      j <- j - p[k]
      e <- E[j, k]
    }
  }
  
  inds <- which(x)
  return(inds)
}