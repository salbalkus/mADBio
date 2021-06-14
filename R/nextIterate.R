#' @title nextIterate
#'
#' @description A helper function for the madbio function - usually not used alone. Obtains the next iteration of the m-ADBio model by simulating a single step of the linear dynamical system.
#'
#' @param pop a vector containing the population of each geography
#' @param C a matrix representing the number of commuters who travel between each pair of regions each day. The origin geographies are represented by the row index, and the destinations are represented by the column index.
#' @param B a vector representing Rt / (pI*pop) which is the reproductive rate per capita, divided by the average recovery time. Calculated in the madbio function.
#' @param At_const a matrix representing the change in the number of individuals in each compartment of the model. This is scaled by alpha to get the final At matrix for the linear dynamical system simulation.
#' @param Xt a matrix representing the number of individuals in each compartment for each geography. Each column represents a geography, and each row represents a different compartment.
#'
#' @return A data frame object containing the number of individuals in each compartment for each geography at the next time step.
#' @export

nextIterate <- function(pop, C, B, At_const, Xt){
  #Set constants for this step
  numCompartments <- 4
  numRegions <- length(pop)
  alpha_const <- diag(numCompartments)
  Xt_new <- Xt
  #Iterate through each region

    # Calculate proportional commuting matrix describing proportion of commuters spreading virus
    M <- (t(C) / pop)*Xt[3,]*B
    A <- (M + t(M)) / 2
  for(i in 1:numRegions){
      #Calculate At piece by piece to prepare for iteration for the region i
      alpha_const[1,1] <- sum(A[i,])
      At <- At_const %*% alpha_const
      #Iterate for region i and store in the "new" variable
      Xt_new[,i] <- (diag(numCompartments) + At) %*% Xt[,i]
    }
    return(Xt_new)
}
