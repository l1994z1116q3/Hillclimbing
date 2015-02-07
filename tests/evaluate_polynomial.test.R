##########################################
##
## Test for function "eval.pol"
## Test if polynomials can be evaluated correctly
##
##########################################

library("Hillclimbing")

load("evaluate_polynomial.R")
load("create_polynomial.R")

pol1 <- pol(2,1:6)
val1 <- c(1,2)

pol2 <- pol(3,c(1,2,1,2,1,2,1,2,1,2))
val2 <- c(4,2,3)

calc1 <- eval.pol(pol1, val1)
calc2 <- eval.pol(pol2, val2)

stopifnot(
    isTRUE(calc1 == 48)
)

stopifnot(
    isTRUE(calc2 == 96)
)

