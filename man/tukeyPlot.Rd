% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graphics.R
\name{tukeyPlot}
\alias{tukeyPlot}
\title{Function to create the fake Tukey plot}
\usage{
tukeyPlot(data, metric = "AUC", groupVars = "Learner", conf = 0.95)
}
\arguments{
\item{data}{The dataframe to be processed}

\item{metric}{The evaluation metric column to use (default is AUC)}

\item{groupVars}{The columns which are used to group the rows (default is Learner)}

\item{conf}{The confidence interval to use}
}
\description{
Function to create the fake Tukey plot
}

