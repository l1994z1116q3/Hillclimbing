##########################################
##
## Test for function ""
## Test if
##
##########################################

library("Hillclimbing")

load("final_chart.R")

set.seed(1)
result <- final(n=6, expen=20, times=5)

data("test.final")
expect <- chart

stopifnot(
    isTRUE(all.equal(result,chart))
)
