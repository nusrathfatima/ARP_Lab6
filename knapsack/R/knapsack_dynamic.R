#' Dynamic programming knapsack algorithm
#' @export
#' 
#' @description Implementation of the knapsack algorithm using the dynamic programming approach.
#' 
#' @param x A dataframe with columns weight (w) and value (v) of the objects 
#' @param W A positive scalar with the capacity of the bag

#' 
#' @return A list containing the \code{value} of the bag content and a vector of the \code{elements} inside
#' 
#' @references 
#' Knapsack Problem - \url{https://en.wikipedia.org/wiki/Knapsack_problem}
#' 
#' @examples
#' set.seed(42)
#' n <- 2000
#' knapsack_objects <- data.frame( w=sample(1:4000, size = n, replace = TRUE), v=runif(n = n, 0, 10000))
#' knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)
#'

knapsack_dynamic <- function(x,W) {
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
    m <- matrix(0, nrow = n+1, ncol = W+1)
    elements <- rep(0,W+1)
    
    for (i in 2:(n+1)) {
      for (j in 1:(W+1)) {
        if (x$w[i-1] > j)
          m[i, j] <- m[i-1, j]
        else
          m[i, j] <- max(m[i-1, j], m[i-1, j-x$w[i-1]] + x$v[i-1])
      }
    }
    
    result <- c()
    limit <- W+1
    for (j in (n+1):2){
      was_added <- m[j,limit] != m[j-1,limit]
      
      if (was_added) {
        result <- append(result, j-1)
        limit <- limit - x$w[j-1]
      }
    }
    
    return(list(value = m[n+1,W+1], elements = sort(result)))
  }
}