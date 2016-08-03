# https://projecteuler.net/problem=24

# Lexicographic permutations
# Problem 24

# A permutation is an ordered arrangement of objects. For example, 3124 is one possible permutation of the digits 1, 2, 3 and 4. If all of the permutations are listed numerically or alphabetically, we call it lexicographic order. The lexicographic permutations of 0, 1 and 2 are:

# 012   021   102   120   201   210

# What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?




unusedDigits = 0:9
    # our unused digits

usedDigits = vector( length = 10)
    # our used digits

x = 1000000
    # we are looking for the 1 millionth lexicographic permutation

for (i in 1:10) {

    use = ceiling( x / factorial( 10 - i))
        # use gives us the index value of the ith digit of our purmutation 
    
    usedDigits[ i] = unusedDigits[ use]
    
    unusedDigits = unusedDigits[ -use]
    
    x = x - factorial( 10 - i) * (use - 1)
        # the xth purmutation is the position of the last permutation before
        #   usedDigit[ i] starts being used in the ith position in the
        #   permutations.

}

as.numeric( paste( as.character( digits), collapse = ""))
    # Rather than a vector containing the individual digits, we'd prefer a
    #   single number. This works because our leading digit is not 0.
