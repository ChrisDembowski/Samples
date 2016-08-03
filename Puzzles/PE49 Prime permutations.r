
# https://projecteuler.net/problem=49

# Prime permutations
# Problem 49

# The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330, is unusual in two ways: (i) each of the three terms are prime, and, (ii) each of the 4-digit numbers are permutations of one another.

# There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, exhibiting this property, but there is one other 4-digit increasing sequence.

# What 12-digit number do you form by concatenating the three terms in this sequence?


library( gtools)



# We write a function to create a sieve of Eratosthenes to identify primes.
# sfsmisc::primes provides a faster algorithm, but we'll write our own.

primes.to.n = function( n) {

    primes = 2L
    
    candidates = 2:n
    
    maxTest = as.integer( floor( sqrt( n)))
        # We don't need to test any values greater than sqrt( n).
    
    test = primes
        # We will look among prime candidates for multiples of test
    
    while (test <= maxTest) {
    
        testMultiples = (2:floor( n / test)) * test
            # testMultiples is a vector of the multiples of test
        
        index = candidates %in% testMultiples
        
        candidates = candidates[ ! index]
            # remove multiples of test from candidate pool
        
        candidates = candidates[ -1]
            # remove first element of candidates, which has already been
            #   identified as prime in previous iteration
        
        primes = c( primes, candidates[ 1])
            # first element of candidates is prime
        
        
        test = tail( primes, n = 1)
            # the next iteration will identify multiples of the last element of
            #   primes
    
    }
    
    primes = c( primes, candidates[ -1])
    
    primes

}



primes = primes.to.n( 9999)

primes = primes[ primes > 1000]

numPrimes = length( primes)

for (i in 1:numPrimes) {

    digits = as.numeric( strsplit( as.character( primes[ i]), "")[[1]][1:4])
    
    perms = permutations( 4, 4, set = FALSE, digits)
        # set = FALSE allows us to keep repeated digits, e.g. 1009
    
    intPerms = unique( apply( perms, 1, paste, collapse = ""))
        # if we do have repeated digits, some permutations will be duplicated
    
    intPerms = as.integer( intPerms)
        # working w/ integers seems to be a bit faster than working w/ numerics.
    
    candidateIndex = intPerms %in% primes
        # identify the permutations that result in primes
    
    if (sum( candidateIndex) >= 3) {
        # If fewer than 3 permutations are prime, we move on.
    
        candidates = intPerms[ candidateIndex]
            # candidates are all prime
        
        candidates = sort( candidates)
        
        len = length( candidates)
        
        differences = as.matrix( dist( candidates, diag = T, upper = T))
            # We calculate the Euclidean distance between each element in our
            #   vector of candidate primes, each made up of the same digits. If
            #   the sequence we are trying to find are among them, the middle
            #   valued prime will be the same distance from two others in the
            #   group. By representing the differences as a matrix, it is easy
            #   to identify that middle vale.
        
        numUniqueValues =
                apply( differences, 1, function( x) length( unique( x)))
            # We count the number of unique differences
        
        if ( min( numUniqueValues) < len) {
        
            middleIndex = which.min( numUniqueValues)
                # We've identified the index of the middle number of sequence.
        
            primeSequence = c( NA, candidates[ middleIndex], NA)
                # We record the middle number of the sequence
        
            big = duplicated( differences[ middleIndex,])
                # big identifies the index of second repeated difference
        
            small = duplicated( differences[ middleIndex, len:1])[len:1]
                # small identifies the index of first repeated difference
        
            primeSequence[ -2] = candidates[ big | small]
                # We record the first and last primes of the sequence
        
            if (! identical( primeSequence, c( 1487L, 4817L, 8147L))) {
                # We want to be careful not to record the sequence given in the
                #   problem description.
        
                i = numPrimes
                    # exit our for loop
        
            }
    
        }
    
    }

}

primeSequence