% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{geos_union_linestring_hyaggregate}
\alias{geos_union_linestring_hyaggregate}
\title{Fast LINESTRING union}
\usage{
geos_union_linestring_hyaggregate(lines, ID)
}
\arguments{
\item{lines}{lines to merge}

\item{ID}{ID to merge over}
}
\value{
LINESTRING sf object
}
\description{
Wayyyy faster then either data.table, or sf based line merging
}
