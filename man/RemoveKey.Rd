% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rdict.R
\name{RemoveKey}
\alias{RemoveKey}
\title{A function to remove a key from the dictionary.}
\usage{
RemoveKey(object, key)
}
\arguments{
\item{object}{the rdict object}

\item{key}{the key}
}
\description{
A function to remove a key from the dictionary.
}
\examples{
myDict = rdict()

myDict = Add(myDict, "key1", 1)
myDict = RemoveKey(myDict, "key1")
}
