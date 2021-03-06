% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_cumulative_error.R
\name{get_cumulative_error}
\alias{get_cumulative_error}
\title{get_cumulative_error}
\usage{
get_cumulative_error(data, true_cases, time, geo_name, test_labels)
}
\arguments{
\item{data}{Dataframe output of the "madbio" function merged with a data frame of the true number of cases at each time point}

\item{true_cases}{Dataframe of true cases. First column is "Time" representing time step, second column is "Cases" representing number of true cases}

\item{time}{Integer representing the day for the error to be calculated}

\item{geo_name}{String representing the geography to be tested}

\item{test_labels}{Vector of string representing label for each experiment run in the model in the data parameter}
}
\value{
Prints the error for each experiment that was run. Experiment names are in the format #-#-# where each # represents the index of the input for initial values, reproductive rate, and the commuting matrix, respectively
}
\description{
Prints the error at a given time point based on the output of the "madbio" function merged with the true number of cases at each time point
}
