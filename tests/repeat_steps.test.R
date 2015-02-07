##########################################
##
## Test for function "repeat.climb"
## Test if the correct local maximum can be reached
##
##########################################

library("Hillclimbing")
library("dplyr")

load("repeat_steps.R")

set.seed(1)
max.steep <- repeat.climb(5, 10, 5, 2, kind="steep")

set.seed(16)
max.median <- repeat.climb(10, 25, 8, 3, kind="median")

set.seed(3)
max.least <- repeat.climb(7, 22, 13, 2, kind="median")

expect1 <- "51.83-33.3"
expect2 <- "-163.1-207"
expect3 <- "-45.25-173"

stopifnot(
    isTRUE(max.steep == expect1)
)

stopifnot(
    isTRUE(max.median == expect2)
)

stopifnot(
    isTRUE(max.least == expect3)
)
