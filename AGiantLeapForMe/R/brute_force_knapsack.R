brute_force_knapsack <-
function(x, W) {
  if (any(W <= 0)) stop("Max weight needs to be positive.")
  if (any(x[,1] <= 0)) stop("Weights in x needs to be positive")
  if (any(x[,2] <= 0)) stop("Values in x needs to be positive")
  if (typeof(x) != "list") stop("x needs to be a data frame")
  
  best_value <- 0
  for (i in 1:(2^nrow(x))) {
    value <- 0
    weight <- 0
    index <- c()
    for (j in 1:nrow(x)) {
      if(bitwAnd(bitwShiftR(i,j), 1) == 1) {
        index <- append(index, j)
        value <- value + x[j, 2]
        weight <- weight + x[j, 1] 
      }
    }
    if (weight <= W & value > best_value) {
      best_position <- index
      best_value <- value
    }
  }
  lst <- list(value = round(best_value), elements = best_position)
  return(lst)
}
