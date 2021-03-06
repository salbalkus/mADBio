% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/madbio_initial_values.R
\name{madbio_initial_values}
\alias{madbio_initial_values}
\title{madbio_initial_values}
\usage{
madbio_initial_values(cases, pop, pE, pI, delay = 2, acc = 1)
}
\arguments{
\item{cases}{a matrix representing the number of recorded cases at each discrete time interval for each geography. Each column represents a geography.}

\item{pop}{a vector containing the population of each geography. The geographies should be in the same order as in the "cases" matrix.}

\item{delay}{an integer representing the expected delay between infection and positive test. Defaults to 2.}

\item{acc}{a numeric representing the expected accuracy of the case records. Defaults to 1, which is perfect accuracy. Greater than 1 indicates an overestimation of cases, less than 1 indicates underestimation. If you do not want to compare results given different potential testing accuracies, do not set this variable.}

\item{LP}{an integer representing the latent period. This is the average time between exposure and infection, rounded to the nearest integer.}

\item{IP}{an integer representing the infectious period. This is the average time between infection and recovery, rounded to the nearest integer.}
}
\value{
A data frame object containing the number of individuals in each compartment for each geography at the next time step.
}
\description{
A function used to construct X0 for the madbio function. Takes in a matrix representing new cases at each discrete time interval and outputs the X0 starting matrix for SEIR compartments. Can also incorporate varying over- or under-estimation simulations of sensor-based initial value collection.
}
