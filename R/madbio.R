#' @title madbio
#'
#' @description test
#'
#' @param pop a vector containing the population of each geography
#' @param Rt a vector containing the effective reproductive rate of each geography
#' @param pE a numeric representing the latent period, or the average number of days to become infected once exposed
#' @param pI a numeric representing the recovery period, or the average number of days to recover once infected
#' @param C a matrix representing the number of commuters who travel between each pair of regions each day. The origin geographies are represented by the row index, and the destinations are represented by the column index.
#' @param X0 a matrix representing the number of individuals in each compartment for each geography. Each column represents a geography, and each row represents a different compartment.
#' @param n Number of iterations to run
#'
#' @return A data frame object containing the number of individuals in each compartment at each time step for each geography.
#' @export

madbio <- function(pop, Rt, pE, pI, C, X0, n){
  #Calculate number of regions being used
  numRegions <- length(pop)

  #NOTE: "3" represents the default column of infected individuals. May need to recalculate if this is later refactored to allow adding additional compartments.
  infectedCol <- 3

  #Test to ensure all inputs are in the correct format
  if(dim(C)[1] != dim(C)[2]){
    stop("Commuting matrix must be square")
  }

  if(numRegions != length(Rt) || numRegions != dim(C)[1] || numRegions != dim(X0)[2]) {
    stop("Mismatch between number of geographies in population vector, Rt vector, commuting matrix, and/or initial value matrix")
  }

  #Initialize matrices that remain constant at each iteration
  B <- Rt / (pI*pop)
  At_const <- matrix(c(-1, 1, 0, 0, 0, -1/pE, 1/pE, 0, 0, 0, -1/pI, 1/pI, 0, 0, 0, 1), nrow = 4)
  compartment_names = c("S","E","I","R") #May want to make this adjustable


  #Run the iteration and record the results
  output <- data.frame() #May want to include geography names as a future option using "columns="
  Xt <- X0
  for(i in 1:n){
    Xt <- nextIterate(pop, C, B, At_const, Xt)
    output <- rbind(output, cbind(data.frame(Compartment = compartment_names, Time = i), as.data.frame(Xt)))
  }
  return(output)
}
