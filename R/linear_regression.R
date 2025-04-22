#' Performs linear regression with optional L2 regularization.
#'
#' This function estimates the coefficients of a linear regression model
#' using ordinary least squares, with the option to add L2 regularization
#' (ridge regression) to prevent overfitting.
#'
#' @param X A numeric matrix of predictor variables.  Each row represents an observation, and each column represents a predictor.
#' @param Y A numeric vector of response variables.
#' @param reg A non-negative numeric value specifying the L2 regularization parameter (lambda). Default is 0 (no regularization).
#'
#' @return An object of class "linreg" containing:
#'   \item{beta}{The estimated regression coefficients.}
#'   \item{X}{The input data X.}
#'   \item{Y}{The input data Y.}
#'   \item{reg}{The regularization parameter used.}
#'
#' @examples
#' # Example usage:
#' X <- matrix(rnorm(100), nrow = 20)
#' Y <- rnorm(20)
#' model <- linear_regression(X, Y, reg = 0.1)
#' print(model)
#'
#' @export
linear_regression = function(X, Y, reg=0) {
    stopifnot(
        "X is not a matrix." = is.matrix(X),
        "Y is not a vector." = is.vector(Y),
        "X is not numeric." = is.numeric(X),
        "Y is not numeric." = is.numeric(Y),
        "Regularization parameter must be non-negative." = reg >= 0,
        "Number of rows in X must equal the length of Y." = nrow(X) == length(Y) #Dimension check
    )

    C = t(X) %*% X
    if (reg > 0) {
        C = C + reg * diag(nrow(C))
    }

    beta = tryCatch(
        solve(C, t(X) %*% Y), #Returns the beta if there are no errors
        error=function(e) {
            warning("Singular matrix encountered.  Returning NA coefficients.")
            return(rep(NA, ncol(X))) # Return a vector of NAs with the correct dimension
        }
    )

    structure(
        list(beta=beta, X=X, Y=Y, reg=reg),
        class='linreg'
    )
}
