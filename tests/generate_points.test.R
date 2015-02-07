##########################################
##
## Test for function "random.input"
## Test if random points can be generated correctly
##
##########################################

library("Hillclimbing")

load("generate_points.R")

set.seed(1)
val1 <- random.input(5,15)

set.seed(5)
val2 <- random.input(3,10)

expect1 <- c(4,4,4,3,0)
expect2 <- c(2,6,2)

stopifnot(
    isTRUE(val1 == expect1)
)

stopifnot(
    isTRUE(val2 == expect2)
)
