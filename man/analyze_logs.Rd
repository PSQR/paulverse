% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Production_Ready_Functions.R
\name{analyze_logs}
\alias{analyze_logs}
\title{analyze_logs}
\usage{
analyze_logs(directory)
}
\arguments{
\item{the}{name of your directory}
}
\value{
dataframe
}
\description{
This function reads every txt of log file in directory and returns when the string "ERROR" was found
}
\examples{
tmp_df <- analyze_logs(directory = "/opt/R/R_project/pperrin/2020/SOE_Rebuild/Logs/202110_1/stderr/"))
}
