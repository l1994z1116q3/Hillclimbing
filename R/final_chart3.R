#' Calculate the average results of the three different methods with different numbers
#' of connections.
#'
#' @param n The number of variables (inputs) in the polynomial. Default: n=20
#' @param expen The total amount of expenditure. Default: expen=50
#' @param times The number of times that you want to repeat for each of the functions
#' so that the result will be more precise. Default: times=1000
#' @param cost The cost at each point. Default: cost=0
#' @return data.frame The result of calculations. See details.
#' @details This function works similarly as the function "final". However, this
#' function has different coefficients.
#'
#' Such function generates a whole chart of results. Each time a random set
#' of parameters will be generated. The coefficients for linear terms are 0, for
#' square terms are in [-19, 0], and for corss products are in [0,2].
#' Then a random set of inputs will be chosen to implement the function.
#'
#' The local maximum for each of the three strategies wil be calculated, and such
#' maximum will be divided into five cases. The number of connections increases from
#' 1 to 5. Each row of the chart represents the outcome of a strategy, and each column
#' represents the outcome of a specific number of connections.
#'
#' @examples
#' final()
#' final(n=5, expen=10, times=100, cost=0.1)

final3 <- function(n=20, expen=50, times=1000, cost=0) {

    Single <- NULL
    ALL <- NULL

    kind <- ""
    connection <- 0

    for (kind in c("steep","median","least")) {

        for (connection in 1:5) {
            Single[connection] <- repeat.climb3(n, expen, times, connection, kind, cost)
        }

        ALL <- rbind(ALL, Single)
        Single <- NULL
    }

    ALL <- as.data.frame(ALL)

    colnames(ALL) <- (c("C=1","C=2","C=3","C=4","C=5"))
    rownames(ALL) <- (c("Steepest Ascent", "Median Ascent", "Least Ascent"))

    return(ALL)
}
