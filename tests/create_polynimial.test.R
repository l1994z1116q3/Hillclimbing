##########################################
##
## Test for function "pol"
## Test if polynomials can be generated correctly
##
##########################################

library("Hillclimbing")
load("create_polynomial.R")

pol1 <- pol(2,1:6)
expect1 <- matrix(c(1,2,4,0,3,5,0,0,6),nrow=3,ncol=3)
colnames(expect1) <- c("x_0","x_1","x_2")
rownames(expect1) <- c("x_0","x_1","x_2")

pol2 <- pol(2,1:4)
expect2 <- matrix(c(1,2,4,0,3,NA,0,0,NA),nrow=3,ncol=3)
colnames(expect2) <- c("x_0","x_1","x_2")
rownames(expect2) <- c("x_0","x_1","x_2")

stopifnot(
    isTRUE(pol1 == expect1)
)

stopifnot(
    isTRUE(pol2 == expect2)
)
