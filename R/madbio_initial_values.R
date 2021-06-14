#' @title madbio_initial_values
#'
#' @description A function used to construct X0 for the madbio function. Takes in a matrix representing new cases at each discrete time interval and outputs the X0 starting matrix for SEIR compartments. Can also incorporate varying over- or under-estimation simulations of sensor-based initial value collection.
#' @param cases a matrix representing the number of recorded cases at each discrete time interval for each geography. Each column represents a geography.
#' @param pop a vector containing the population of each geography. The geographies should be in the same order as in the "cases" matrix.
#' @param LP an integer representing the latent period. This is the average time between exposure and infection, rounded to the nearest integer.
#' @param IP an integer representing the infectious period. This is the average time between infection and recovery, rounded to the nearest integer.
#' @param delay an integer representing the expected delay between infection and positive test. Defaults to 2.
#' @param acc a numeric representing the expected accuracy of the case records. Defaults to 1, which is perfect accuracy. Greater than 1 indicates an overestimation of cases, less than 1 indicates underestimation. If you do not want to compare results given different potential testing accuracies, do not set this variable.
#'
#' @return A data frame object containing the number of individuals in each compartment for each geography at the next time step.
#' @export

madbio_initial_values <- function(cases, pop, pE, pI, delay = 2, acc = 1){
  LP <- round(pE)
  IP <- round(pI)

  #This function produces the initial values for the SEIR simulation of a given state
  #Format the data frame of cases and the cumulative cases
  cases <- as.data.frame(cases)
  total_cases <- cumsum(data.frame(cases))

  #Calculate the population of each compartment by summing active cases as specified by the latent and infectious periods
  E <- as.vector(colSums(cases[(nrow(cases)- LP + 1):(nrow(cases)),]))
  I <- as.vector(colSums(cases[(nrow(cases)- LP - IP + 1):(nrow(cases)- LP -1),]))
  R <- as.vector(total_cases[nrow(total_cases),] - E - I)

  #Correct for the specified accuracy
  E <- E*acc
  I <- I*acc
  R <- R*acc

  #Calculate S as the remaining non-infected population
  S <- as.vector(pop - E - I - R)

  output <- matrix(unlist(t(matrix(c(S, E, I, R), nrow = 3))), nrow = 4)

  return(output)
}
