greedy_knapsack <-
function(x, W){
  value <- 0
  x$ratio <- x$v / x$w
  x <- as.data.frame(x)
  elements <- vector()
  ordered_vec <- order(x$ratio, decreasing=TRUE)
  for (i in ordered_vec){
    if (x[i,]$w < W){
      value <- value + x[i,]$v
      W <- W -x[i,]$w
      elements <- c(elements, i)
    }
  }
  return(list(value=value, elements=elements))
}
