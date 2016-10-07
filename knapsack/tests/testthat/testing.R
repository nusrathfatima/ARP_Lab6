# Test
library(knapsack)
library(testthat)

set.seed(42)
n <- 2000
knapsack_objects <- data.frame(w=sample(1:4000, size = n, replace = TRUE), 
                               v=runif(n = n, 0, 10000))

test_that("testing function brute_force_knapsack", {
  # true values
  expect_equal(brute_force_knapsack(knapsack_objects[1:8, ], W = 3500),
               list(value = 16770.38,
                    elements = c(5, 8)),
               tolerance = 0.000001)
  
  expect_equal(brute_force_knapsack(knapsack_objects[1:12, ], W = 2000),
               list(value = 15427.81,
                    elements = c(3, 8)),
               tolerance = 0.000001)
  # ERRORS 
  # argument x 
  # non-dataframe
  expect_error(brute_force_knapsack(x = "abc", W = 3500), 
               "x is not a dataframe")
  # column names
  expect_error(brute_force_knapsack(x = data.frame("aaa" = 1:5,
                                                  "v" = 1:5 ), W = 3),
              "columns from dataframe x must be named w and v")
  expect_error(brute_force_knapsack(x = data.frame("w" = 1:5,
                                                   "jose" = 1:5 ), W = 3),
               "columns from dataframe x must be named w and v")
  # argument w
  # non-numeric values
  expect_error(brute_force_knapsack(x = data.frame(w = c("a", "b", 3:10),
                                                   v = runif(10)), W = 5),
               "column w must be a vector of positive numeric values")
  # negative values 
  expect_error(brute_force_knapsack(x = data.frame(w = - c(1:10),
                                                   v = runif(10)), W = 25),
               "column w must be a vector of positive numeric values")
  # argument v
  # non-numeric value
  expect_error(brute_force_knapsack(x = data.frame(w = c(1:10),
                                                   v = c("a","b", runif(8))), W = 5),
               "column v must be a vector of positive numeric values")
  # negative values 
  expect_error(brute_force_knapsack(x = data.frame(w = c(1:10),
                                                   v = -runif(10)), W = 25),
               "column v must be a vector of positive numeric values")
  # negative value W
  expect_error(brute_force_knapsack(x = data.frame(w = c(1:10),
                                                   v = runif(10)), W = -100),
               "W must be a positive numeric value")
  })


# Dynamic programming  
test_that("testing function greedy_knapsack", {
  expect_equal(knapsack_dynamic(knapsack_objects[1:8, ], W = 3500),
               list(value = 16770.38,
                    elements = c(5, 8)),
               tolerance = 0.000001)
  expect_equal(knapsack_dynamic(knapsack_objects[1:12, ], W = 2000),
               list(value = 15427.81,
                    elements = c(3, 8)),
               tolerance = 0.000001)
  # ERRORS 
  # argument x 
  # non-dataframe
  expect_error(knapsack_dynamic(x = "abc", W = 3500), 
               "x is not a dataframe")
  # column names
  expect_error(knapsack_dynamic(x = data.frame("aaa" = 1:5,
                                                   "v" = 1:5 ), W = 3),
               "columns from dataframe x must be named w and v")
  expect_error(knapsack_dynamic(x = data.frame("w" = 1:5,
                                                   "jose" = 1:5 ), W = 3),
               "columns from dataframe x must be named w and v")
  # argument w
  # non-numeric values
  expect_error(knapsack_dynamic(x = data.frame(w = c("a", "b", 3:10),
                                                   v = runif(10)), W = 5),
               "column w must be a vector of positive numeric values")
  # negative values 
  expect_error(knapsack_dynamic(x = data.frame(w = - c(1:10),
                                                   v = runif(10)), W = 25),
               "column w must be a vector of positive numeric values")
  # argument v
  # non-numeric value
  expect_error(knapsack_dynamic(x = data.frame(w = c(1:10),
                                                   v = c("a","b", runif(8))), W = 5),
               "column v must be a vector of positive numeric values")
  # negative values 
  expect_error(knapsack_dynamic(x = data.frame(w = c(1:10),
                                                   v = -runif(10)), W = 25),
               "column v must be a vector of positive numeric values")
  # negative value W
  expect_error(knapsack_dynamic(x = data.frame(w = c(1:10),
                                                   v = runif(10)), W = -100),
               "W must be a positive numeric value")
})

test_that("testing function greddy_knapsack", {
  expect_equal(greedy_knapsack(knapsack_objects[1:800, ], W = 3500),
               list(value = 192646.7,
                    elements = c(92, 574, 472, 80, 110, 537, 332, 117,37, 776, 577,
                                 288, 234, 255, 500, 794, 55, 290, 436, 346, 282, 764,
                                 599, 303, 345, 300, 243, 43, 747, 35, 77, 229, 719, 564)),
               tolerance = 0.000001)
  # ERRORS 
  # argument x 
  # non-dataframe
  expect_error(greedy_knapsack(x = "abc", W = 3500), 
               "x is not a dataframe")
  # column names
  expect_error(greedy_knapsack(x = data.frame("aaa" = 1:5,
                                               "v" = 1:5 ), W = 3),
               "columns from dataframe x must be named w and v")
  expect_error(greedy_knapsack(x = data.frame("w" = 1:5,
                                               "jose" = 1:5 ), W = 3),
               "columns from dataframe x must be named w and v")
  # argument w
  # non-numeric values
  expect_error(greedy_knapsack(x = data.frame(w = c("a", "b", 3:10),
                                               v = runif(10)), W = 5),
               "column w must be a vector of positive numeric values")
  # negative values 
  expect_error(greedy_knapsack(x = data.frame(w = - c(1:10),
                                               v = runif(10)), W = 25),
               "column w must be a vector of positive numeric values")
  # argument v
  # non-numeric value
  expect_error(greedy_knapsack(x = data.frame(w = c(1:10),
                                               v = c("a","b", runif(8))), W = 5),
               "column v must be a vector of positive numeric values")
  # negative values 
  expect_error(greedy_knapsack(x = data.frame(w = c(1:10),
                                               v = -runif(10)), W = 25),
               "column v must be a vector of positive numeric values")
  # negative value W
  expect_error(greedy_knapsack(x = data.frame(w = c(1:10),
                                               v = runif(10)), W = -100),
               "W must be a positive numeric value")
})
  




