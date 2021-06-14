#' @title madbio
#'
#' @description An implementation of the m-ADBio model for epidemic forecasting. This function produces a data frame representing the number of individuals in each compartment (Susceptible, Exposed, Infectious, and Recovered) at each discrete time point. These may be plotted using madbio_plot.
#'
#' @param geo_names a vector containing the names of each geography in the order in which they appear in the rest of the data.
#' @param pop a vector containing the population of each geography. Populations must be ordered by their appearance in "geo_names"
#' @param Rt a list of vectors containing the effective reproductive rate of each geography. Each vector of Rt values will be simulated and output as a separate column
#' @param pE a numeric representing the latent period, or the average number of days to become infected once exposed
#' @param pI a numeric representing the recovery period, or the average number of days to recover once infected
#' @param C a matrix representing the number of commuters who travel between each pair of regions each day. The origin geographies are represented by the row index, and the destinations are represented by the column index.
#' @param X0 a matrix representing the number of individuals in each compartment for each geography. Each column represents a geography, and each row represents a different compartment. Alternatively, a list of matrices to be run.
#' @param n Number of iterations to run
#' @param start_date The starting date of the forecast
#'
#' @return A data frame object containing the number of individuals in each compartment at each time step for each geography.
#' @export

madbio <- function(geo_names, pop, Rt, pE, pI, C, X0, n, start_date){
  #Calculate number of regions being used
  numRegions <- length(pop)
  compartment_names <- c("S","E","I","R")

  #Cast singular X0 and C matrix to list if necessary
  if(typeof(X0) != "list"){
    X0 <- list(X0)
  }

  if(typeof(C) != "list"){
    C <- list(C)
  }

  #NOTE: "3" represents the default column of infected individuals. May need to recalculate if this is later refactored to allow adding additional compartments.
  infectedCol <- 3

  #Test to ensure all inputs are in the correct format
  if(dim(C[[1]])[1] != dim(C[[1]])[2]){
    stop("Commuting matrix must be square")
  }

  #if(numRegions != length(Rt[[1]]) || numRegions != dim(C[[1]])[1] || numRegions != dim(X0[[1]])[2]) {
  #  stop("Mismatch between number of geographies in population vector, Rt vector, commuting matrix, and/or initial value matrix")
  #}

  #Set up variables to store output
  output <- data.frame()
  labels <- data.frame()
  for(i in 1:n){
    labels <- rbind(labels, data.frame(Time = i, Compartment = compartment_names, Geography = rep(geo_names, each = length(compartment_names))))
  }

  #Simulate each commuting matrix
  for(k in 1:length(C)){
    #Simulate each set of starting values
    for(m in 1:length(X0)){
      #Simulate each set of Rt values
      for(v in seq(1,length(Rt), 2)){
        #Run the iteration for the lower bound

        #Calculate vectors and matrices needed for the iteration
        B <- Rt[[v]] / (pI*pop)
        At_const <- matrix(c(-1, 1, 0, 0, 0, -1/pE, 1/pE, 0, 0, 0, -1/pI, 1/pI, 0, 0, 0, 0), nrow = 4)
        Xt <- X0[[m]]
        lower <- c()

        #Run the iteration and record the results
        for(i in 1:n){
          Xt <- nextIterate(pop, C[[k]], B, At_const, Xt)
          lower <- c(lower, c(Xt))
        }

        #Run the iteration for the upper bound

        #Calculate vectors and matrices needed for the iteration
        B <- Rt[[v+1]] / (pI*pop)
        Xt <- X0[[m]]
        upper <- c()

        #Run the iteration and record the results
        for(i in 1:n){
          Xt <- nextIterate(pop, C[[k]], B, At_const, Xt)
          upper <- c(upper, c(Xt))
        }

        #Concatentate the output into a data frame for output
        output <- rbind(output,cbind(labels, Lower = lower, Upper = upper, Experiment = paste(toString(m), toString((v+1)/2), toString(k),sep="")))
      }
    }
  }
  output <- merge(data.frame(Time = 1:n, Date = as.Date(seq(start_date, by = 1, length.out = n), format = "%y-%m-%d")), output, by = "Time")
  return(output)
}
