% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hello.R
\docType{data}
\name{GeomTimelineLabel}
\alias{GeomTimelineLabel}
\title{TimelineLabel Geom}
\format{
An object of class \code{GeomTimelineLabel} (inherits from \code{Geom}, \code{ggproto}, \code{gg}) of length 5.
}
\usage{
GeomTimelineLabel
}
\arguments{
\item{x}{Dates (date format)}

\item{label}{Labels (char format)}

\item{y}{Country (a factor)}
}
\value{
This function returns a Geom.
}
\description{
This geom plots label over a timeline of hurricanes in a country. You need to input
the \code{x} coordinate, which should be a date, and a \code{label} parameter giving
the column from which you want to fish the labels. You can optionally
provide a \code{y} coordinate, e.g. a country, to have more timelines stacked
vertically. The usual aes for geom_point apply.
}
\keyword{datasets}
