#' Figure out all the results when you take a step forward.
#'
#' @param pol The objective polynomial represented in matrix form.
#' @param val The values of variables, from x_1 to x_n.
#' @param connection The maximum distance you can transfer a unit of expenditure from
#' one input into another.
#' @return A matrix. The first column represents the result of a transfer, the
#' second column represents the input you are reducing, and the third represents
#' the input you are increasing.
#' @examples
#' step.choice(pol = pol(5,1:21), val = (1:5), connection = 2)


step.choice <- function (pol, val, connection) {

    n <- length(val)
    step.results <- NULL
    temp <- NULL

    ## Make sure each of the variable gets a value and no excess

    if (n != (ncol(pol)-1)) {
        stop("The length of values does not equal to the number of variables.")
    }

    ## Delete excessive connections

    if (connection > n/2) {
        connection <- as.integer(n/2)
    }

    ## Count and select inputs

    for (i in 1:n) {

        ## Operate if this input is not zero

        if (val[i] > 0) {

            ## Count and select connections

            for (j in 1:connection) {

                ## Transfer a unit from one input to another

                if ((i+j) > n) {m <- i+j-n} else {m <- i+j}
                temp <- val
                temp[i] <- (temp[i] - 1)
                temp[m] <- (temp[m] + 1)

                ## Evaluate the revenue after the transferrence

                x <- eval.pol(pol, temp)

                ## Write down the transferrence and the revenue

                step.results <- rbind(step.results, c(x, i, m))
            }
        }
    }

    step.results <- as.data.frame(step.results)
    colnames(step.results) <- c("result", "from", "to")

    return(step.results)
}
