# R tasks for Michael Finnegan's candicacy for the statitician/econometrician position at AIR.
# Tasks 1 through 5 are included in this script. Task 6 is in a seperate script in order to
# smoothly run the Shiny app.

# Task 1: create likelihood function for given pdf
likelihood <-function(theta,b){ 
  n <- 20       # declare value of n
  L <- 1/(1+exp(-((n*theta)-sum(b))))
  return(L)
}

# Task 2: test my likelihood function
theta <- 0    # set value of theta to 0
set.seed(12345)   #set seed to '12345' for sake of reproducibility
b <- runif(20, min = -4, max = 4)   # construct vector b from the uniform distribution

likelihood(theta,b)     # test likelihood function

# Task 3: create square matrix from normal distribution
sq.mtx <- matrix(rnorm(5*5,mean=0,sd=1), nrow=5, ncol=5) 
sq.mtx

# Task 4: compute column and row means
row.mean <- rowMeans(sq.mtx)   # similar to the apply function
row.mean                       
col.mean <- colMeans(sq.mtx)
col.mean

# Task 5: plot the vector of means
plot(row.mean)   # most simple plot
plot(col.mean)

barplot(row.mean)    # also simple
barplot(col.mean)

library(ggplot2)
ggplot(df, aes(index)) +      # slightly more advanced plot
  geom_line(aes(y = row.mean, color = "row.mean")) + 
  geom_line(aes(y = col.mean, color = "col.mean"))+
  xlab("Row/Column Number") +
  ylab("Row/Column Mean")+
  ggtitle("Plot of Row and Column Means")

