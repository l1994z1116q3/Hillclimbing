#' Calculate the mean and standard deviation of many results
#'
#' @param n The number of variables (inputs) in the polynomial.
#' @param expen The total amount of expenditure.
#' @param times The number of times that you want to repeat for each of the functions
#' so that the result will be more precise.
#' @param connection The number of connections.
#' @param kind One of the three functions that you would like to choose.
#' @param cost The cost at each point.
#' @return a string of the form "aaaa-bbb".
#' @details In the return, "aaaa" represents the mean value of all the local maximums
#' and "bbb" represents the standard deviation of those maximums.
#' @examples
#' repeat.climb(n=5, expen=10, times=50, connection=2, kind="steep")

repeat.climb <- function (n,
                          expen,
                          times,
                          connection,
                          kind = c("steep","median","least"),
                          cost=0) {

    c <- NULL
    record <- vector("numeric", length=times)
    p <- (n^2+3*n+2)/2

    ## The way to select directions / type of stepping strategy

    if (kind == "steep") {FUN <- step.final.steep}
    if (kind == "median") {FUN <- step.final.med}
    if (kind == "least") {FUN <- step.final.least}

    for (i in 1:times) {

        ## Generate a set of random coefficients for each time

        c <- runif(p, -1, 1)
        c[1] <- 0

        pol <- pol(n,c)

        ## Generate a set of random inputs given total expenditure

        val <- random.input(n, expen)

        ## Calculate local maximum

        record[i] <- FUN (pol, val, connection, cost)
    }

    ## Calculate the average and standard deviation of local maximum

    avg.ret <- signif(mean(record, na.rm=TRUE), 4)
    std.err <- signif(sd(record),3)

    return(paste(avg.ret, std.err, sep="-"))
}
