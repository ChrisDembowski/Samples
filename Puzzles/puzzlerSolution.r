# "A farmer had a 40-pound stone which he could use to weigh 40 pounds of feed; he would sell feed in 40 pounds, or bales of hay, or whatever. He had a balance scale; he put the stone on one side and pile the other side with feed or hay, and when it balanced, that's it. … A neighbor borrows the stone, but he had to apologize when he returned it, broken into four pieces. The farmer who owned the stone later told the neighbor that he actually had done him a favor. The pieces of the broken stone could now be used to weigh any item, assuming those items were in one-pound increments, from one pound to 40. … What were the weights of the four individual stones?" (Assume the weight of each stone is an integer value of pounds)

# From CarTalk Puzzler, July 13, 2013
# <http://www.cartalk.com/content/stone-temple-farmers-0?question>






# For each stone, it can be placed on the right side of the balance ( coefficient = 1), the left side of the balance, with the feed (coefficient = -1), or left off (coefficient = 0). coeffs is a 4x81 matrix, with each column representing one of the 81 possible combinations of coefficients. Note: Suppose we have a 5 pound stone and a 1 pound stone, and we want to measure 4 pounds of feed. We's place the five pound stone on the right side of the balance, the 1 pound stone on the left side of the balance, and add feed until the scale balances, at which point there will be 4 pounds of feed on the left side of the scale. That combination is represented by 1*5 + (-1)*1 = 4.

coeffs = matrix( c( rep( -1:1, each = 27), 
                    rep( -1:1, each = 9, times = 3),
                    rep( -1:1, each = 3, times = 9),
                    rep( -1:1, times = 27)
                  ),
                 ncol = 81,
                 nrow = 4,
                 byrow = TRUE
               )


# We need a way to validate each possible distinct partition of the forty pound stone. validate is a function that verifies that some combination of stones placed on the scale can be used to measure any integer value weight from 1 pound to 40 pounds. The maximum weight, 40 pounds, occurs when all coefficients equal 1, so we can count the number of unique positive values instead of checking for each value 1 thru 40 separately. validate returns TRUE if this is possible, FALSE otherwise.

validate = function( vec, mat) {

    possibleValues = apply( vec * mat, 2, sum)
    
    positiveValues = possibleValues[ possibleValues > 0]
    
    if (length( unique( positiveValues)) == 40) {
        
        return( TRUE)
        
    } else {
    
        return( FALSE)
    
    }

} 



# We initialize a matrix to record all of our valid partitions of the stone.
valid = matrix( ncol = 4)

# We need an index for valid
index = 1




# We are only interested in the 4-tuples (a, b, c, d) such that a+b+c+d = 40 and a ≤ b ≤ c ≤ d.

# a = i can take any value from 1 to 10. Any larger and b, c, or d, must be < a.
for (i in 1:10) {

    # b = j-i can take any value from a to floor( (40-a)/3)
    for (j in (2 * i):(i + floor( (40 - i) / 3))) {
    
        # c = k-j can take any value from b to floor( (40-a-b)/2)
        for (k in (i + 2 * (j - i)):(j + floor( (40 - j) / 2))) {
            
            # Given a, b, c, the value of d is fixed at 40-a-b-c.
            stones = c( i, j - i, k - j, 40 - k)
            
            if (validate( stones, coeffs)) {
            
                valid[ index,] = stones
                
                index = index + 1
            
            }
        
        }
    
    }

}

valid