##########################################
##
## Test for function "step.choice"
## Test if the neighboring points can be calculated correctly
##
##########################################

library("Hillclimbing")

load("what.is.a.step.forward.R")

pol1 <- matrix(c(1,2,4,0,3,5,0,0,6),nrow=3,ncol=3)
    colnames(pol1) <- c("x_0","x_1","x_2")
    rownames(pol11) <- c("x_0","x_1","x_2")
val1 <- c(3,2)
connection1 <- 1
neighbor1 <- step.choice(pol1, val1, connection1)

pol2 <- matrix(c(1,3,2,4,0,5,3,4,0,0,7,1,0,0,0,5),nrow=4,ncol=4)
    colnames(pol2) <- c("x_0","x_1","x_2","x_3")
    rownames(pol2) <- c("x_0","x_1","x_2","x_3")
val2 <- c(3,0,4)
connection2 <- 2
neighbor2 <- step.choice(pol2, val2, connection2)

expect1 <- as.data.frame(matrix(c(113,87,1,2,2,1),ncol=3,nrow=2))
    colnames(expect1) <- c("result","from","to")
expect2 <- as.data.frame(matrix(c(174,198,1,3,2,1),ncol=3,nrow=2))
    colnames(expect2) <- c("result","from","to")

stopifnot(
    isTRUE(all.equal(neighbor1,expect1))
)

stopifnot(
    isTRUE(all.equal(neighbor2,expect2))
)
