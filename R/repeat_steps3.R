#' Calcilate the mean and standard deviation of the results from a specific case
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

## This function works in the same way as "repeat.climb", except that when a set
## of random coefficients are generated, this time the coefficients for linear
## terms are 0, for square terms are in [-19,0], and for corss products are in [0,2].

repeat.climb3 <- function (n, expen, times, connection,
                           kind = c("steep","median","least"), cost=0) {
    c <- NULL
    record <- vector("numeric", length=times)
    p <- (n^2+3*n+2)/2

    if (kind == "steep") {FUN <- step.final.steep}
    if (kind == "median") {FUN <- step.final.med}
    if (kind == "least") {FUN <- step.final.least}

    for (i in 1:times) {
        c <- runif(p, 0, 2)
        c[1] <- 0
        pol <- pol(n,c)
        for (k in 2:(n+1)) {
            pol[k,1]<-0
            pol[k,k]<-runif(1, -19, 0)
        }
        val <- random.input(n, expen)
        record[i] <- FUN (pol, val, connection, cost)
    }

    avg.ret <- signif(mean(record, na.rm=TRUE), 4)
    std.err <- signif(sd(record),3)

    return(paste(avg.ret, std.err, sep="-"))
}
