% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{flowpaths_to_linestrings}
\alias{flowpaths_to_linestrings}
\title{Flowpaths to linestrings}
\usage{
flowpaths_to_linestrings(fl)
}
\arguments{
\item{fl}{a LINESTRING/MULTILINESTRING \code{sf} flowlines object}
}
\value{
a LINESTRING \code{sf} flowlines object
}
\description{
Takes an input list of flowpaths object and converts
all possible features to LINESTRING
}
