% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepare4MFA.R
\name{samplesTable}
\alias{samplesTable}
\title{Build a table with information about samples in a data frame}
\usage{
samplesTable(x)
}
\arguments{
\item{x}{A data frame containing the variables to be analyzed.}
}
\value{
A data frame with the index, name and Group of each variable in the data frame.
}
\description{
May be useful when, given a dataset, one wishes to create a table
that will be later used to visualize individuals by groups
Notice that some columns of this table might be variables in the original dataset.

The function returns a data frame with
- the index,
- name,
- Group of each individual in a data frame.
The type of a variable is determined based on whether it is a factor/character or not.
}
\examples{
samplesTable(iris)

}
