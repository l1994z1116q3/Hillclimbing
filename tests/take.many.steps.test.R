##########################################
##
## Test for function "step.final.steep", "step.final.med" and "step.final.least"
## Test if the functions can find correct local maximums
##
##########################################

library("Hillclimbing")

load("take.many.steps.R")
load("create_polynomial.R")

pol <- pol(3,c(1,2,1,2,1,2,1,2,1,2))
val <- c(4,2,3)
connection <- 2

calc1 <- step.final.steep(pol, val, connection)
calc2 <- step.final.med(pol, val, connection)
calc3 <- step.final.least(pol, val, connection)

expect1 <- 120
expect2 <- 120
expect3 <- 97

stopifnot(
    isTRUE(calc1 == 120)
)

stopifnot(
    isTRUE(calc2 == 120)
)

stopifnot(
    isTRUE(calc3 == 97)
)
