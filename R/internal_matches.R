
if (FALSE) {
  tmp.dd <- matrix(
    c(0.99, 0.95, 0.89, 0.91),
    nrow = 2)
  rownames(tmp.dd) <- c("x1", "x2")
  colnames(tmp.dd) <- c("g1", "g2")
  tmp.dd <- 1 - tmp.dd
  internal_matches(tmp.dd, cutoff = 0.1)
  
}

# We break this out as a separate function to simplify testing.
internal_matches <- function(original.dd, cutoff) {

  # Cannot use Inf in the foreign function call clue::solve_LSAP(dd) (below)
  my.inf <- 9e99 
  
  # browser()
  
  dd <- original.dd
  was.transformed <- FALSE
  if (nrow(dd) > ncol(dd)) {
    # Add more columns
    dd <- t(dd)
    was.transformed <- TRUE
  }
  
  dd[dd > cutoff] <- my.inf
  
  solution <- clue::solve_LSAP(dd)
  
  cost.one.pair <- function(rowi) {
    # browser()
    colj <- solution[rowi]
    dist <- dd[rowi, colj]
    name1 <- rownames(dd)[rowi]
    name2 <- colnames(dd)[colj]
    return(list(name1 = name1, name2 = name2, dist = dist))
  }
  
  proto.table <- lapply(1:length(solution), FUN = cost.one.pair)
  
  table1 <- matrix(unlist(proto.table), ncol = 3, byrow = TRUE)
  if (was.transformed) {
    table1 <- table1[ , c(2,1,3)]
    dd <- t(dd)
  }
  
  colnames(table1) <- c("x1", "x2", "dist")
  # browser()
  table1 <- data.frame(table1)
  table1$dist <- as.numeric(table1$dist)
    
  table <- table1[table1$dist < my.inf, , drop = FALSE ]

  return(list(table = table, orig.matrix = original.dd, modified.matrix = dd))
      
}
