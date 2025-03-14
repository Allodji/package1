% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/uniComp.R
\name{ptruncReg}
\alias{ptruncReg}
\title{Estimates the regression coefficients in the tau-truncated Poisson regression model}
\usage{
ptruncReg(y, X, tau, tol = 1e-08, add_intercept = TRUE)
}
\arguments{
\item{y}{The vector of dependent (or response) variable}

\item{X}{a data frame or matrix containing the explanatory variables for the regression}

\item{tau}{the right-truncation threshold}

\item{tol}{the tolerance}

\item{add_intercept}{a Boolean that specifies if an intercept should be added to the model or not}
}
\value{
a list
}
\description{
Estimates the regression coefficients in the tau-truncated Poisson regression model
}
\examples{
data("data/data.rda")
ptruncReg(data$y,(data$y+1):ncol(data),tau, tol = 1e-8)




}
