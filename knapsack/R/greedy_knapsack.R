#' Greedy knapsack algorithm
#' @export
#' 
#' @description Implementation of the knapsack algorithm using the greedy approach.
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
#' greedy_knapsack(x = knapsack_objects[1:8,], W = 3500)
#'

greedy_knapsack <- function(x,W) {
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
    x.sorted <- x[ order(-x$v/x$w),]
    elements <- c()
    total.weight <- 0
    total.value <- 0
    i <- 1
    repeat {
      if (total.weight + x.sorted$w[i] <= W) {
        total.weight <- total.weight + x.sorted$w[i]
        total.value <- total.value + x.sorted$v[i]
        elements <- append(elements, as.numeric(rownames(x.sorted[i,])))
        i <- i + 1
      } else
        break
    }
    return(list(value = total.value, elements = elements))
  }
}