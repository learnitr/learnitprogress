% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/progression.R
\name{learnr_prog}
\alias{learnr_prog}
\alias{h5p_prog}
\alias{shiny_prog}
\title{Progression in learnrs, H5P and Shiny apps}
\usage{
learnr_prog(
  user_login,
  class_logins,
  class_aa,
  class_data = NULL,
  url = getOption("learnitr.lrs_url")
)

h5p_prog(
  user_login,
  class_logins,
  class_aa,
  class_data = NULL,
  url = getOption("learnitr.lrs_url")
)

shiny_prog(
  user_login,
  class_logins,
  class_aa,
  class_data = NULL,
  url = getOption("learnitr.lrs_url")
)
}
\arguments{
\item{user_login}{The GitHub login of one student}

\item{class_logins}{All the logins for the class}

\item{class_aa}{The AA identifier for this class}

\item{class_data}{The data for this class}

\item{url}{The url of the MongoDB data base}
}
\value{
A data frame with all the progression data for the user and the class
}
\description{
Progression in learnrs, H5P and Shiny apps
}
