% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/linear_regression.R
\name{linear_regression}
\alias{linear_regression}
\title{Performs linear regression with optional L2 regularization.}
\usage{
linear_regression(X, Y, reg = 0)
}
\arguments{
\item{X}{A numeric matrix of predictor variables.  Each row represents an observation, and each column represents a predictor.}

\item{Y}{A numeric vector of response variables.}

\item{reg}{A non-negative numeric value specifying the L2 regularization parameter (lambda). Default is 0 (no regularization).}
}
\value{
An object of class "linreg" containing:
  \item{beta}{The estimated regression coefficients.}
  \item{X}{The input data X.}
  \item{Y}{The input data Y.}
  \item{reg}{The regularization parameter used.}
}
\description{
This function estimates the coefficients of a linear regression model
using ordinary least squares, with the option to add L2 regularization
(ridge regression) to prevent overfitting.
}
\examples{
# Example usage:
X <- matrix(rnorm(100), nrow = 20)
Y <- rnorm(20)
model <- linear_regression(X, Y, reg = 0.1)
print(model)

}
