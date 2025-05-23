% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_data.R
\name{generate_data}
\alias{generate_data}
\title{Generates Example Data for Linear Regression}
\usage{
generate_data(n_row, n_col, std = 1)
}
\arguments{
\item{n_row}{Number of observations.}

\item{n_col}{Number of columns.}

\item{std}{The standard deviation of Xi.}
}
\value{
An object of class "linreg_data" with attributes:
 \item{B} coefficients
 \item{X} input matrix
 \item{Y} output vector
}
\description{
This function generates:
* coefficients B
* input matrix X
* output vector Y
for linear regression model:
     Y = X B + Xi
where xi ~ Normal(0, std^2)
}
