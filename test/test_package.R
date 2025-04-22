
# roxygen2::roxygenize()
devtools::install("/home/bodo/Documents/GitHub/ARP2025LProject2")
library(ARP2025LProject2)

test = function() {

    linreg_data = generate_data(128, 16)

    X = linreg_data$X
    Y = linreg_data$Y
    B = linreg_data$B

    linreg = linear_regression(X, Y, reg=0.1)

    plot(linreg)
}


make_example_data = function() {

    example_data = generate_data(128, 16)
    save(example_data, file="../data/example_data.RData")

}
