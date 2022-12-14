% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ntickets.R
\name{ntickets}
\alias{ntickets}
\title{calculates the number of tickets (n) to be sold on a given flight,
then prints a named list containing nd (n-discrete),
nc (n-continuous), N, p and gamma (all below) -
then creates a plot of the objective function Vs n.}
\usage{
ntickets(N, gamma, p)
}
\arguments{
\item{N}{number of seats on the flight}

\item{gamma}{probability plane will be overbooked}

\item{p}{probability someone will show up for the flight.}
}
\description{
calculates the number of tickets (n) to be sold on a given flight,
then prints a named list containing nd (n-discrete),
nc (n-continuous), N, p and gamma (all below) -
then creates a plot of the objective function Vs n.
}
