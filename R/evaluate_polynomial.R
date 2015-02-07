#' Evaluate a polynomial in matrix form
#'
#' @param pol The objective polynomial represented in matrix form.
#' @param val The values of variables, from x_1 to x_n.
#' @return Numeric.
#' @examples
#' polynomial <- pol(5,1:36) ##Create a polynomial with 5 variables. The
#' coefficients go from 1 to 36 for the 36 terms.
#' @examples eval.pol(polynomial, 1:5)

eval.pol <- function (pol, val) {

    x <- 0
    x_0 <- 1

    ## Assign values to corresponding variables

    for (i in 1:(ncol(pol)-1)) {
        assign(colnames(pol)[i+1],val[i])
    }

    ## Calculate the product for each term and add it to the final value

    for (i in 1:ncol(pol)) {
        for (j in 1:i) {
            x <- x +
                eval(as.symbol(colnames(pol)[i])) *
                eval(as.symbol(colnames(pol)[j])) *
                pol[i,j]
        }
    }

    return(x)
}
