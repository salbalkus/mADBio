#' @title madbio_plot
#'
#' @description Takes in the data frame from the madbio function and plots the data
#'
#' @param data a data frame in the same format as that output by the "madbio" function
#' @param geo_name the geography that you want to examine.
#' @param true_cases a data frame representing the true cases for comparison
#' @param test_labels a vector of labels representing the name of each experiment that you are plotting
#' @param plot_type a string specifying the data to plot. Options: "Cumulative Cases" to plot the Exposed, Infectious, and Recovered compartments (all cases ever); "Infectious Cases" to plot just the Infectious Compartment (active cases)
#' @param Xt a matrix representing the number of individuals in each compartment for each geography. Each column represents a geography, and each row represents a different compartment.
#'
#' @return A data frame object containing the number of individuals in each compartment for each geography at the next time step.
#' @import ggplot2
#' @import plotly
#' @export

madbio_plot <- function(data, geo_name, true_cases, test_labels, pred_name = "", true_name = "", plot_type = "Cumulative cases", plot_format = "ggplot"){
  df <- data %>% filter(Geography == geo_name)
  #Filter the data based off the selected plot type
  if(plot_type == "Cumulative Cases"){
    if(!base::missing(true_cases)){
      df <- df %>% group_by(Time, Date, Experiment) %>% filter(Compartment != "S") %>% summarise(Lower = sum(Lower), Upper = sum(Upper))
      df <- merge(df, true_cases, by = "Time")
    } else {
      df <- df %>% group_by(Time, Date, Experiment) %>% filter(Compartment != "S") %>% summarise(Lower = sum(Lower), Upper = sum(Upper))
    }
  }
  else if(plot_type == "Infectious Cases") {
    df <- df %>% filter(Compartment == "I")
  }
  else {
    stop("plot_type pararameter must be one of the following: \"Cumulative Cases\", \"Infectious Cases\" ")
  }

  n_exp <- length(unique(df$Experiment))
  #Plot the data
  p <- ggplot(df) + geom_ribbon(aes(ymin = Lower, ymax = Upper, x = Date, fill = as.factor(Experiment)), alpha = 0.5) + theme_light() +
    labs(fill = " ", x = "Time (days)", y = plot_type) +
    theme(legend.position = "bottom", text = element_text(size=12)) +
    scale_fill_discrete(name = pred_name, labels = test_labels)
  #Plot the true cases if they are specified
  if(!missing(true_cases)){
    p <- p + geom_point(aes(x = Date, y = Cases, shape = "True Cases"), size = 1) +
      scale_shape_manual(name = true_name, values = c("circle"), labels = c("True Cases"))
  }

  if(plot_format == "plotly"){

    p <- ggplotly(p, tooltip = c("y", "x","Upper","Lower"))
    p <- p %>% layout(legend = list(x = 0.05, y = 0.95))
    p <- plotly_build(p)

    for(i in 1:length(test_labels)){
      p$x$data[[i]]$name <- test_labels[[i]]
    }
    p$x$data[[length(test_labels)+1]]$name <- "True Cases"
  }
  return(p)
}
