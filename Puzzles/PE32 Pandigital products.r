# https://projecteuler.net/problem=32

# Pandigital products
# Problem 32

# We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1 through 5 pandigital.

# The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing multiplicand, multiplier, and product is 1 through 9 pandigital.

# Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.

# HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.







# First some math

# Assuming we test every permutation of 123456789,
#   ab * cde = fghi and cde * ab = fghi will both be possible to find, but will result in duplicate products. Therefore, we can restrict our search to cases in which the multicand is less than the multiplier. In other words, for x * y = z, we want x < y.

# 9 * 999 < 10,000, the smallest five digit integer, so if we place our * after the first digit, the equals sign must go after the fifth digit.

# 11 * 1111 > 999, the largest three digit integer, and 99 * 99 < 10,000, so if we place our * after the second digit, the equals sign must go after the fifth digit.

# Thus, we can have a 1 or 2 digit multicand, but only a 4 digit product.

library( gtools)

last = factorial( 9)

permMatrix = permutations( 9, 9)
    # all 9! permutations of 1:9 in ascending order
    
products = vector()

for (i in 1:last) {

    u = permMatrix[ i, 1]
    
    v = as.numeric( paste( as.character( permMatrix[ i, 2:5]), collapse = ""))
    
    x = as.numeric( paste( as.character( permMatrix[ i, 1:2]), collapse = ""))
    
    y = as.numeric( paste( as.character( permMatrix[ i, 3:5]), collapse = ""))
    
    z = as.numeric( paste( as.character( permMatrix[ i, 6:9]), collapse = ""))
    
    if (u * v == z || x * y == z) products = c( products, z)
    
}

sum( unique( products))