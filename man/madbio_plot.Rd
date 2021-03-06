% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/madbio_plot.R
\name{madbio_plot}
\alias{madbio_plot}
\title{madbio_plot}
\usage{
madbio_plot(
  data,
  geo_name,
  true_cases,
  test_labels,
  pred_name = "",
  true_name = "",
  plot_type = "Cumulative cases",
  plot_format = "ggplot"
)
}
\arguments{
\item{data}{a data frame in the same format as that output by the "madbio" function}

\item{geo_name}{the geography that you want to examine.}

\item{true_cases}{Optional parameter. A data frame representing the true cases for evaluating how well the model has performed}

\item{test_labels}{a vector of labels representing the name of each experiment that you are plotting}

\item{plot_type}{a string specifying the data to plot. Options: "Cumulative Cases" to plot the Exposed, Infectious, and Recovered compartments (all cases ever); "Infectious Cases" to plot just the Infectious Compartment (active cases)}

\item{Xt}{a matrix representing the number of individuals in each compartment for each geography. Each column represents a geography, and each row represents a different compartment.}
}
\value{
A data frame object containing the number of individuals in each compartment for each geography at the next time step.
}
\description{
Takes in the data frame from the madbio function and plots the data
}
