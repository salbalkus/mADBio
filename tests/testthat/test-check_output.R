library(testthat)
library(madbio)       # load our package

# Set up variables for testing
cases_test <- data.frame(region1 = c(1, 3, 4, 5, 8, 6, 4, 5), region2 = c(10, 15, 16, 17, 20, 18, 4, 9), region3 = c(0, 0, 1, 0, 4, 0, 0, 2))
pop_test <- c(500, 1000, 100)
pI <- 2.7
pE <- 1.4

# madbio_initial_values() tests
# Ensure the initial values function outputs in the correct format

test_that("madbio_initial_values() returns 4 x n matrix, where n is the number of regions", {
  initial_values_test <- madbio_initial_values(cases_test, pop_test, pI, pE)
  expect_equal(dim(initial_values_test), c(4, 3))
})

test_that("madbio_initial_values() returns correctly delayed values for S, E, I, and R compartments", {
  initial_values_test <- madbio_initial_values(cases_test, pop_test, pI, pE)
  expect_equal(initial_values_test, matrix(c(c(464, 15, 13, 8), c(891, 31, 37, 41), c(93, 2, 4, 1)), nrow = 4))
})

# madbio() tests
# Ensure the madbio model outputs predictions in the correct format


X0 <- madbio_initial_values(cases_test, pop_test, pI, pE)
C <- matrix(c(500, 200, 5, 100, 1000, 20, 10, 5, 100), nrow = 3)
C_nocommute <- diag(diag(C))
Rt <- list(c(0.9, 0.8, 0.6), c(1.1, 1.2, 1.4))
START_DATE <- as.Date("2021-01-12")
days <- 10

test_that("madbio() returns a data frame with the correct columns", {
  result_test <- madbio(c("region1","region2","region3"), pop_test, Rt, pE, pI, list(C, C_nocommute), X0, days, START_DATE)
  expect_equal(names(result_test), c("Time", "Date", "Compartment", "Geography", "Lower","Upper","Experiment"))
})

test_that("madbio() returns a data frame with no NA values", {
  result_test <- madbio(c("region1","region2","region3"), pop_test, Rt, pE, pI, list(C, C_nocommute), X0, days, START_DATE)
  expect_false(any(is.na(names(result_test))))
})


result_test <- madbio(c("region1","region2","region3"), pop_test, Rt, pE, pI, list(C, C_nocommute), X0, days, START_DATE)
test_labels <- c("m-ADBio")
region_to_display <- "region1"

# madbio_plot() tests
# Ensure that a plot is output

test_that("madbio_plot() returns a ggplot object", {
  expect_s3_class(madbio_plot(result_test, region_to_display, test_labels = test_labels, plot_type = "Cumulative Cases", plot_format = "ggplot"),
                  "ggplot")
})

true_cases <- data.frame(Time = c(1,2,3,4,5,6,7,8,9,10), Cases = c(30, 40, 60, 65, 80, 95, 105, 110, 120, 135))
test_labels <- c("Perfect Sensors, m-ADBio")


# get_cumulative_error tests
# Ensure that error is output to the console

test_that("get_cumulative_error() prints to the console", {
  expect_output(get_cumulative_error(result_test,
                                     true_cases,
                                     days,
                                     region_to_display,
                                     test_labels = test_labels))
})



