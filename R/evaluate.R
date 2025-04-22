#' Evaluates the performance of a linear regression model.
#'
#' This function calculates various metrics to assess the performance of a
#' linear regression model, including R-squared, adjusted R-squared, and MSE.
#'
#' @param linreg An object of class "linreg" returned by the linear_regression function.
#' @param X A numeric matrix of predictor variables.
#' @param Y A numeric vector of response variables.
#'
#' @return A list containing the following performance metrics:
#'   \item{r.squared}{The R-squared value (explained variance).}
#'   \item{adj.r.squared}{The adjusted R-squared value.}
#'   \item{mse}{The Mean Squared Error.}
#'
#' @examples
#' # Example usage:
#' X <- matrix(rnorm(100), nrow = 20)
#' Y <- rnorm(20)
#' model <- linear_regression(X, Y, reg = 0.1)
#' evaluation <- evaluate(model, X, Y)
#' print(evaluation)
#'
#' @export
evaluate = function(linreg, X, Y) {
  stopifnot(
    "linreg must be an object of class 'linreg'." = inherits(linreg, "linreg"),
    "X is not a matrix." = is.matrix(X),
    "Y is not a vector." = is.vector(Y),
    "X is not numeric." = is.numeric(X),
    "Y is not numeric." = is.numeric(Y),
    "Number of rows in X must equal the length of Y." = nrow(X) == length(Y),
    "Number of columns in X must match the length of the coefficients in linreg." =
      ncol(X) == length(linreg$beta)
  )

  predictions <- X %*% linreg$beta
  residuals <- predictions - Y

  n <- length(Y)
  p <- ncol(X)

  tss <- sum((Y - mean(Y))^2) # total sum of squares
  rss <- sum(residuals^2) # sum of square residuals

  r.squared <- 1 - (rss / tss)
  adj.r.squared <- 1 - ( (1 - r.squared) * (n - 1) / (n - p - 1))
  mse <- mean(residuals^2)

  structure(
    list(
      r.squared=r.squared,
      adj.r.squared=adj.r.squared,
      mse=mse
    ),
    class="linreg_results"
  )
}
