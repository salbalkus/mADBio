#' @title get_cumulative_error
#'
#' @description Prints the error at a given time point based on the output of the "madbio" function merged with the true number of cases at each time point
#'
#' @param data Dataframe output of the "madbio" function merged with a data frame of the true number of cases at each time point
#' @param time Integer representing the day for the error to be calculated
#' @param geo_name String representing the geography to be tested
#'
#' @return Prints the error for each experiment that was run. Experiment names are in the format #-#-# where each # represents the index of the input for initial values, reproductive rate, and the commuting matrix, respectively
#' @import dplyr
#' @import magrittr
#' @export

get_cumulative_error <- function(data, true_cases, time, geo_name, test_labels){
  df <- merge(data, true_cases, by = "Time")
  errcalc <- df %>% filter(Time == time, Geography == geo_name, Compartment != "S")
  n <- 1
  for(exp in unique(data$Experiment)){
    curErr <- errcalc %>% filter(Experiment == exp)
    pred_cases <- sum((curErr$Lower + curErr$Upper) / 2)
    cat("Error for", test_labels[n], "=", (pred_cases - curErr$Cases[1]) / curErr$Cases[1], "\n")
    n <- n + 1
  }
}
