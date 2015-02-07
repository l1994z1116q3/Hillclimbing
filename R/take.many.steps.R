#' Calculate the highest point when climbing along the steepest direction
#'
#' @param pol The objective polynomial represented in matrix form.
#' @param val The values of variables, from x_1 to x_n.
#' @param connection The maximum distance you can transfer a unit of expenditure from
#' one input into another.
#' @param cost = 0 The money you would spend on each points.
#' @return Numeric. The highest profit you can ever reach when you choose a
#' specific point and climb along the steepest direction.
#' @details There are three functions. When you apply the function step.final.steep,
#' everytime when you find a list of available choices, such function will choose the
#' one that increases the value of the polynomial most. The function step.final.med
#' will choose the median among the whole least, and step.final.least will choose the
#' least one.
#' @examples
#' step.final.steep(pol = pol(5,1:21), val = (1:5), connection = 2, cost = 0,1)

step.final.steep <- function (pol, val, connection, cost = 0) {

    one.step <- NULL
    steps <- 0
    calc <- eval.pol(pol, val)

    k <- TRUE

    ## Keep going until we reach the local maximum

    while (k) {

        ## Read in the neighbouring environment and select a direction

        step.res <- step.choice(pol, val, connection)
        step.good <- filter(step.res, result == max(step.res$result))

        ## Any directions or not / local maximum or not

        if (step.good$result > calc) {

            ## Keep the loop functioning

            k <- TRUE

            ## Enforce the selected step

            calc <- step.good$result
            val[step.good$from] <- val[step.good$from] - 1
            val[step.good$to] <- val[step.good$to] + 1
            steps <- steps + 1

        } else{
            k <- FALSE
        }
    }

    ## Calculate the cost for stepping (by default, cost=0).

    calc <- calc - steps * cost

    return(calc)
}

## The other two functions are the same, except that they select different directions
## rather than the steepest one.


step.final.med <- function (pol, val, connection, cost = 0) {
    one.step <- NULL
    steps <- 0
    calc <- eval.pol(pol, val)

    k <- TRUE

    while (k) {

        step.res <- step.choice(pol, val, connection)
        step.res <- filter(step.res, result > calc)

        ## Find the number that is equal to median
        ## In case there are an even number of results, choose the one that is larger

        step.good <- filter(step.res, result >= median(step.res$result))
        step.good <- filter(step.good, result == min(step.good$result))

        if (length(step.good$result) == 1) {

            k <- TRUE

            calc <- step.good$result
            val[step.good$from] <- val[step.good$from] - 1
            val[step.good$to] <- val[step.good$to] + 1

            steps <- steps + 1

        } else{
            k <- FALSE
        }
    }

    calc <- calc - steps * cost

    return(calc)
}

step.final.least <- function (pol, val, connection, cost = 0) {
    one.step <- NULL
    steps <- 0
    calc <- eval.pol(pol, val)

    k <- TRUE

    while (k) {

        step.res <- step.choice(pol, val, connection)
        step.res <- filter(step.res, result > calc)
        step.good <- filter(step.res, result == min(step.res$result))

        if (length(step.good$result) == 1) {

            k <- TRUE

            calc <- step.good$result
            val[step.good$from] <- val[step.good$from] - 1
            val[step.good$to] <- val[step.good$to] + 1

            steps <- steps + 1

        } else{
            k <- FALSE
        }
    }

    calc <- calc - steps * cost

    return(calc)
}

