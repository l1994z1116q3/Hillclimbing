#' Create a random string of input
#'
#' @param n The number of different inputs.
#' @param expen The total amount of expenditures.
#' @return A string of numbers that represent different amount of inputs.
#' @examples
#' random.input (inp = 20, expen = 50)  #The total expenditure of 50 is assigned
#' to 20 different inputs.

random.input <- function (n, expen) {

    val <- vector("numeric", length=n)
    used <- 0

    ## Generate random input one by one

    for (i in 1:(n-1)) {

        ## Expenditure shrinks after each input is selected, and new input is chosen
        ## from the rest of the expenditure

        val[i] <- sample(0:(expen-used), 1)
        used <- (used + val[i])
    }

    ## The left-over of the expenditure is put into the last input

    val[n] <- (expen-used)
    return(val)
}
