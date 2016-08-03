# Dice Game
# Problem 205

# https://projecteuler.net/problem=205


# Peter has nine four-sided (pyramidal) dice, each with faces numbered 1, 2, 3, 4.
# Colin has six six-sided (cubic) dice, each with faces numbered 1, 2, 3, 4, 5, 6.

# Peter and Colin roll their dice and compare totals: the highest total wins. The result is a draw if the totals are equal.

# What is the probability that Pyramidal Pete beats Cubic Colin? Give your answer rounded to seven decimal places in the form 0.abcdefg





# I solved the problem first with Markov chains, then with brute force, to see the difference in speed



# 1
# Markov Chains
# mean execution time: 1.27 ms

# build transition probability matrices for each Peter & Colin
peterMarkov = matrix( 0L, nrow = 37, ncol = 41)

colinMarkov = matrix( 0L, nrow = 37, ncol = 43)
    # the extra columns of these matrices simplify populating them with
    #   probabilities

for (i in 1:37) { # This loop populates the matrices with appropriate
                  #   probability values
    
    peterMarkov[ i, (1:4) + i] = 1 / 4L
    
    colinMarkov[ i, (1:6) + i] = 1 / 6L
}

peterMarkov = peterMarkov[ , 1:37] # truncate matrix

colinMarkov = colinMarkov[ , 1:37] # truncate matrix


matrixPower = function( X, pow) { # define function to calculate power of a
                                  #     matrix
    
    Y = X

    for (i in 2:pow) Y = X %*% Y
    
    return( Y)

}

# vectors of probabilities for each possible outcome, for each Peter & Colin
peterOutcomeProbs = matrixPower( peterMarkov, 9)[ 1, -1]

colinOutcomeProbs = matrixPower( colinMarkov, 6)[ 1, -1]

peterWinsProb = 0L

for (i in 9:36) peterWinsProb = peterWinsProb + sum( peterOutcomeProbs[ i] *
        colinOutcomeProbs[ 6:(i - 1)])
    # sum probabilities of combinations that Peter wins

peterWinsProb












# 2
# Brute Force
# mean execution time: 127 ms

possibleOutcomes = function( sides, rolls) { # function to construct vector of
                                             #  outcomes
    
    n = sides ^ rolls
    
    outcome = rep( 0L, n)
    
    for (i in 1: rolls) outcome = outcome + rep( 1:sides,
        each = sides ^ (rolls - i), lenth.out = n)
    
    outcome

}

peterOutcomes = possibleOutcomes( 4L, 9L)

colinOutcomes = possibleOutcomes( 6L, 6L)

numOutcomesPeter = length( peterOutcomes)

numOutcomesColin = length( colinOutcomes)

outcomeProbsPeter = rep( 0L, 36L)

outcomeProbsColin = rep( 0L, 36L)

peterWinProb = 0L

for (i in 6:36) {

    outcomeProbsPeter[ i] = length( peterOutcomes[ peterOutcomes == i]) / 
        numOutcomesPeter # Calculate & record probability Peter's dice total i
    
    outcomeProbsColin[ i] = length( colinOutcomes[ colinOutcomes == i]) / 
        numOutcomesColin # Calculate & record probability Colin's dice total i
    
    peterWinProb = peterWinProb + sum( outcomeProbsPeter[ i] *
        outcomeProbsColin[ 1:(i - 1L)])
            # Probability Peter wins with any total <= i

}

peterWinProb
