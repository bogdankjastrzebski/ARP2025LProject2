#' Plot Linreg Object
#'
#' Plots histogram of residuals.
#'
#' @param linreg linreg object containing coefficients B and regularization parameter reg.
#' @param X A numeric matrix of predictor variables.  Each row represents an observation, and each column represents a predictor.
#' @param Y A numeric vector of response variables.
#'
#' @return ggplot plot.
#'
#' @export 
plot.linreg = function(linreg) {
    stopifnot(
        "linreg must be an object of class 'linreg'." = inherits(linreg, "linreg"),
        "X is not a matrix." = is.matrix(X),
        "Y is not a vector." = is.vector(Y),
        "X is not numeric." = is.numeric(X),
        "Y is not numeric." = is.numeric(Y),
        "Number of rows in X must equal the length of Y." = nrow(X) == length(Y),
        "Number of columns in X must match the length of the coefficients in linreg." = ncol(X) == length(linreg$beta)
    )

    residuals <- linreg$X %*% linreg$beta - linreg$Y

    ggplot2::ggplot(
            data.frame(residuals = residuals),
            ggplot2::aes(x=residuals)
        ) +
        ggplot2::geom_histogram(bins=30) +
        ggplot2::theme_bw()
}
