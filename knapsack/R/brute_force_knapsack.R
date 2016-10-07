brute_force_knapsack <- function(x, W) {
  if (!is.data.frame(x))
    stop("x is not a dataframe")
   else if (any(colnames(x) != c("w","v")))
     stop("columns from dataframe x must be named w and v")
  else if(any(!is.numeric(x$w)) || any(x$w < 0))
    stop("column w must be a vector of positive numeric values")
  else if(any(!is.numeric(x$v)) || any(x$v < 0))
    stop("column v must be a vector of positive numeric values")
  else if(!is.numeric(W) || W < 0)
    stop("W must be a positive numeric value")
  else {
    n <- nrow(x)
    A <- rep(FALSE, n)
    bestValue <- 0
    bestChoice <- c()
    for (i in 1:(2^n)){
      j <- n
      tempWeight <- 0
      tempValue <- 0
      while (A[j] && j > 0) {
        A[j] <- FALSE
        j <- j - 1
      }
      A[j] <- TRUE
      for (k in 1:n){
        if (A[k]) {
          tempWeight <- tempWeight + x$w[k]
          tempValue <- tempValue + x$v[k]
        }
      }
      if ((tempValue > bestValue) && (tempWeight <= W)) {
        bestValue <- tempValue
        bestChoice <- A
      }
      
    }
        
    return(list(value=bestValue, elements=(1:n)[bestChoice]))
  }
}