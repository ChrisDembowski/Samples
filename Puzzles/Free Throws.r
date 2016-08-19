# Bob has a probability of 0.3 to successfully make a free throw. What is the probability that Bob can make 5 free throws in a row in no more than 100 attempts?

n = 100

success = 5

p = 0.3


# Model the problem as a Markov chain with six states. State 1 is a current run of 0, given that a run of five has not yet been achieved. State 2 is a current run of 1, given that a run of five has not yet been achieved. We continue similarly to State 5: a current run of 4, given that a run of five has not yet been achieved. The State 6 is that a run of five has been achieved. No other state is accessible from State 6.

# First, build the row-stochastic Markov matrix. The entry in the ith row and jth column represents the probability of transitioning from the ith state to the jth state with a single free throw attempt.

probMatrix = matrix( 0, ncol = success + 1, nrow = success + 1)

probMatrix[ 1:success, 1] = 1 - p

probMatrix[ -(success + 1), -1] = diag( p, nrow = success)

probMatrix[ success + 1, success + 1] = 1



# Define a function to raise a matrix to a power

matrixPower = function( X, pow) { # pow should be integer > 1
    
    Y = X

    for (i in 2:pow) Y = X %*% Y
    
    return( Y)

}

# The 100-step transition matrix is found by raising probMatrix to the 100th power. The desired result, the probability of transitioning from the 1st state to the 6th state in 100 steps is found in the 1st row, and 6th column.

matrixPower( probMatrix, n)[ 1, success + 1]