my_knapsack <- function(df, cap){
  
  p <- df$price
  s <- df$selected_by
  
  n <- nrow(df)
  x <- logical(n)
  E <- matrix(0, nrow = cap + 1, ncol = n)
  G <- matrix(0, nrow = cap + 1, ncol = 1)
  
  for(k in 1:n) {
    E[, k] <- G
    H <- c(numeric(p[k]), G[1:(cap + 1 - p[k]), 1] + s[k])
    G <- pmax(G, H)
  }
  
  fmax <- G[cap + 1, 1]
  e <- fmax
  j <- cap + 1
  
  for(k in n:1) {
    if (E[j,k] < e) {
      x[k] <- TRUE
      j <- j - p[k]
      e <- E[j, k]
    }
  }
  
  inds <- which(x)
  return(df[inds,])
}