% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class.R
\name{class_logins}
\alias{class_logins}
\alias{class_aa}
\alias{user_login}
\title{Get a list of all user logins in a given class}
\usage{
class_logins(class, url = getOption("learnitr.lrs_url"), as.json = FALSE)

class_aa(
  aa,
  url = getOption("learnitr.lrs_url"),
  as.json = FALSE,
  module = NULL
)

user_login(email, url = getOption("learnitr.lrs_url"))
}
\arguments{
\item{class}{The short identifier of the class}

\item{url}{The URL of the MongoDB database (LRS)}

\item{as.json}{If \code{TRUE}, return the result as a JSON string}

\item{aa}{The short identifier of the activity}

\item{module}{The module to restrict too, or \code{NULL} for all modules}

\item{email}{The email of the student}
}
\value{
A character vector with the results
}
\description{
Get a list of all user logins in a given class
}
