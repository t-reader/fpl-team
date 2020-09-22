squad <- function(df = data, opt_by = "total_points", n_gkp = 2, n_def = 5, n_mid = 5, n_fwd = 3, budget = 1000){
  
  gkp <- df[df$position == "GKP", ]
  def <- df[df$position == "DEF", ]
  mid <- df[df$position == "MID", ]
  fwd <- df[df$position == "FWD", ]
  
  p <- n_gkp + n_def + n_mid + n_fwd
  
  gkp_ind <- position(gkp, n_gkp, opt_by)
  def_ind <- position(def, n_def, opt_by)
  mid_ind <- position(mid, n_mid, opt_by)
  fwd_ind <- position(fwd, n_fwd, opt_by)
  
  n_comb <- nrow(gkp_ind) * nrow(def_ind) * nrow(mid_ind) * nrow(fwd_ind)
  comb <- matrix(ncol = 5, nrow = n_comb)
  
  m <- 1
  for(i in 1:nrow(gkp_ind)){
    for(j in 1:nrow(def_ind)){
      for(k in 1:nrow(mid_ind)){
        for(l in 1:nrow(fwd_ind)){
          comb[m,] <- c(gkp_ind[i, (n_gkp + 1)], def_ind[j, (n_def + 1)], 
                        mid_ind[k, (n_mid + 1)], fwd_ind[l, (n_fwd + 1)],
                        gkp_ind[i, (n_gkp + 2)] + def_ind[j, (n_def + 2)] + mid_ind[k, (n_mid + 2)] + fwd_ind[l, (n_fwd + 2)])
          m <- m + 1
        }
      }
    }
  }
  new_comb <- comb[which(rowSums(comb[,1:4]) <= budget), ]
  new_comb <- new_comb[order(new_comb[,5], decreasing = T), ]
  
  found <- F
  i <- 1
  
  while(found == F){
    
    draft_gkp <- gkp[gkp_ind[which(gkp_ind[, n_gkp + 1] == new_comb[i,1]),][1:n_gkp],]
    draft_def <- def[def_ind[which(def_ind[, (n_def + 1)] == new_comb[i,2]),][1:n_def],]
    draft_mid <- mid[mid_ind[which(mid_ind[, (n_mid + 1)] == new_comb[i,3]),][1:n_mid],]
    draft_fwd <- fwd[fwd_ind[which(fwd_ind[, (n_fwd + 1)] == new_comb[i,4]),][1:n_fwd],]
    
    draft_squad <- rbind(draft_gkp, draft_def, draft_mid, draft_fwd)
    
    if(any(table(draft_squad$team) > 3)){
      i <- i + 1
    } else {
      found <- T
    }
  }

  return(draft_squad)
}