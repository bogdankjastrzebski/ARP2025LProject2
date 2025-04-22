#' Generates Example Data for Linear Regression
#'
#' This function generates:
#' * coefficients B
#' * input matrix X
#' * output vector Y
#' for linear regression model:
#'      Y = X B + Xi
#' where xi ~ Normal(0, std^2)
#'
#' @param n_row Number of observations.
#' @param n_col Number of columns.
#' @param std The standard deviation of Xi.
#' 
#' @return An object of class "linreg_data" with attributes:
#'  \item{B} coefficients
#'  \item{X} input matrix
#'  \item{Y} output vector
#'
#' @export 
generate_data = function(n_row, n_col, std=1) {
    stopifnot(
        is.numeric(n_row), 
        is.numeric(n_col), 
        is.numeric(std),
        length(n_row) == 1,
        length(n_col) == 1
    )
    B = rnorm(n_col)
    X = matrix(rnorm(n_row * n_col), n_row, n_col)
    Y = as.vector(X %*% B + std * rnorm(n_row))
    structure(
        list(B=B, X=X, Y=Y),
        class='linreg_data'
    )
}
