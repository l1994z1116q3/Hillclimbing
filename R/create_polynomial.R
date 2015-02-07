#' Create a polynomial in matrix form
#'
#' @param n The number of variables.
#' @param c A string of the coefficients of the polynomial. The length of c should
#' equal to (n^2+3n+2)/2, otherwise NA will be produced.
#'
#' @return A matrix. Column and row names represent variables, and the value of
#' each individual slot represents the coefficient of the multiple of its
#' corresponding row and column names.
#' @details The first variable x_0 is set to be 1.
#' @examples
#' pol(5, 1:21)

pol <- function (n, c) {

    poly.mat <- matrix(0, ncol=(n+1), nrow=(n+1))
    k <- 0

    ## Count and select rows

    for (i in 1:(n+1)) {

        ## Count and select columns

        for (j in 1:i) {

            ## Put the next coefficient into the specified position

            k <- k+1
            poly.mat[i,j] <- c[k]
        }
    }

    ## Make variables and store them as row/column names

    colnames(poly.mat) <- paste("x", 0:n, sep="_")
    rownames(poly.mat) <- paste("x", 0:n, sep="_")

    return(poly.mat)
}
