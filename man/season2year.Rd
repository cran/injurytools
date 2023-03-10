% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{season2year}
\alias{season2year}
\title{Get the year}
\usage{
season2year(season)
}
\arguments{
\item{season}{Character/factor specifying the season. It should follow the
pattern "xxxx/yyyy", e.g. "2005/2006".}
}
\value{
Given the season, it returns the year (in \code{numeric}) in which the
season started.
}
\description{
Get the year given the season.
}
\examples{
season <- "2022/2023"
season2year(season)
}
