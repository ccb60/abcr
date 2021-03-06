% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/split_contents.R
\name{find_tunes}
\alias{find_tunes}
\title{Find the .starts of all tunes within an ABC tune file}
\usage{
find_tunes(.lines)
}
\arguments{
\item{.lines}{vector of (character) lines from an ABC file, in order.}
}
\value{
An integer vector of .starting locations for each tune, as defined by
the \code{X:...} tag that begins each tune.
}
\description{
Find the .starts of all tunes within an ABC tune file
}
