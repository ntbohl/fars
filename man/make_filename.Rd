% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{make_filename}
\alias{make_filename}
\title{Generate date-specific filename}
\usage{
make_filename(year)
}
\value{
This function returns a string that will serve as
a filename for the dataset in question. The main purpose of
this function is to ensure the unique year is added to the
filename.
}
\description{
This function accepts an input and returns a filename that
meets a proper templated format while including the unique
date of the input string. Given the as.integer function
within our function, the input must be numeric, otherwise an
error will be thrown.
}
\examples{
\dontrun{
make_filename(2018)
}
}
