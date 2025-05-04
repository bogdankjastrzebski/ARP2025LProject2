
# roxygen2::roxygenize()
# devtools::install_github(repo = "bogdankjastrzebski/ARP2025LProject2")
devtools::install("/home/bodo/Documents/GitHub/ARP2025LProject2")
library(ARP2025LProject2)


test = function() {

    linreg_data = generate_data(128, 16)

    X = linreg_data$X
    Y = linreg_data$Y
    B = linreg_data$B

    k = 100
    train_ind = 1:k
    valid_ind = (k+1):nrow(X)
    linreg = linear_regression(X[train_ind,], Y[train_ind], reg=0.1)

    plot(linreg)

    evaluate(linreg, X[valid_ind,], Y[valid_ind])

}


make_example_data = function() {

    example_data = generate_data(128, 16)
    save(example_data, file="data/example_data.RData")

}


knit_tutorial = function() {
    # install.packages("rmarkdown")
    # install.packages("knitr")
    library(knitr)
    library(rmarkdown)
    knit("Tutorial.Rmd")
    rmarkdown::render("Tutorial.Rmd")

}
