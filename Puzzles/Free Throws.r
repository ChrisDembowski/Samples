# Bob has a probability of 0.3 to successfully make a free throw. What is the
#   probability that Bob can make 5 free throws in a row in no more than 100
#   attempts?


# To generalize the problem, we want the probability of at least k consecutive
#   successes in a series of n Bernoulli trials, where each success occurs with
#   probability p.

library( expm) # for matpow, to raise matrix to a power

n = 100

k = 5

p = 0.3


# Model the problem as a Markov chain with six states. State 1 is a run of k
#   successes has not yet been achieved and the current run is length 0. State 2
#   is a run of k has not yet been achieved and the current run is length 1. We
#   continue similarly to State k: a run of k has not yet been achieved and the
#   current run is length k-1. State k+1 is that a run of k has been achieved.
#   No other state is accessible from State k+1.

# First, build the row-stochastic Markov matrix. The entry in the ith row and
#   jth column represents the probability of transitioning from the ith state to
#   the jth state with a single free throw attempt.

probMatrix = matrix( 0, ncol = k + 1, nrow = k + 1)

probMatrix[ 1:k, 1] = 1 - p

probMatrix[ -(k + 1), -1] = diag( p, nrow = k)

probMatrix[ k + 1, k + 1] = 1

# The n-step transition matrix is found by raising probMatrix to the nth
#   power. The desired result, the probability of transitioning from the 1st
#   state to the (k+1)th state in n steps is found in the 1st row, and (k+1)th
#   column.

(probMatrix %^% n)[ 1, k + 1]
