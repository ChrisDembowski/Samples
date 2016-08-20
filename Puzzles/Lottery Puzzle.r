# The Lottery Scheme


# http://wordplay.blogs.nytimes.com/2016/06/27/jordan-ellenberg-the-lottery-scheme/

# Starting in 2005, a group of M.I.T. students executed a daring lottery scheme,
#   eventually winning over $3.5 million in a game called Massachusetts Cash
#   WinFall. Central to their plan was an ingenious mechanism of avoiding risk
#   by guaranteeing themselves a large payoff on every drawing. How did they do
#   it? The basic idea can already be grasped in the following simplified
#   example:

# Suppose you were playing a lottery where each drawing picked 3 out of 7
#   numbered balls. When a ball is picked, it isn’t replaced, so a drawing
#   always consists of three DIFFERENT numbers, like 2,3,7 or 3,4,5. The order
#   of the numbers doesn’t matter.

# If you get all three numbers right, you win the jackpot, $6. If your ticket
#   has two correct numbers out of three, you win a $2 consolation prize. One or
#   fewer correct numbers and you get nothing.

# Tickets cost 80 cents each. I want to buy a set of 7 tickets which guarantees
#   me a profit on every drawing. (Since my tickets cost $5.60, this means that
#   either one of my tickets needs to be a jackpot or I need to win consolation
#   prizes with at least three of my tickets.) How should I pick my tickets?


# accessed 2016 July 01










# Solution

# There are choose( 7, 2) == 21 different possible pairs of numbers that could
#   potentially win a consolation prize. Each ticket specifies three distinct
#   numbers, or choose( 3, 2) == 3 pairs of numbers. So with 7 tickets, we can
#   have 7 * 3 == 21 pairs of numbers. Can we choose our numbers such that we
#   have 21 distinct pairs of numbers?



# First we'll create a matrix of all possible pairs

possPairs = vector()

for (i in 1:6) {

    for (j in (i + 1):7) {
    
        possPairs = c( possPairs, c( i, j))
    
    }

}

possPairs = matrix(possPairs, ncol = 2, nrow = 21, byrow = TRUE)





remainPairs = possPairs
    # Let's keep track of which pairs we haven't used, while keeping possPairs
    #   intact




# We will purchase 7 tickets, with 3 numbers each

tickets = matrix( ncol = 3, nrow = 7)


# We want to systematically fill out our tickets. First, we'll write a function
#   to check to see if a pair of numbers remains.

check = function( pair, remain = remainPairs) {

    for (i in 1:dim( remain)[ 1]) {
    
        if (all( pair %in% remain[ i,])) return( 0)
            # return 0 if pair has not yet been used
    
    }
    return( 1)

}


# We want to be able to easily delete rows from our remainPairs matrix to keep
#   it updated.

rowToDelete = function( pair, remain = remainPairs) {
    
    if (! is.matrix( remain)) remain = matrix( remain, ncol = 2)
        # prevent R from silently converting 1 x 2 matrix to vector

    i = 1
   
    while ( i <= dim( remain)[ 1]) {
   
        if (all( pair %in% remain[ i,])) return( i)
        
        i = i + 1
   
   }
   return( 0)

}

# Now we can begin systematically filling out our tickets.
for (i in 1:7) {

    tickets[ i, 1:2] = remainPairs[ 1,]
        # Since it is in remainPairs vector, this pair has not been used yet, so
        #   it can be used for the first two pick on the ith ticket.
            
    remainPairs = remainPairs[ -1,]
        # But we want to delete that pair now that we've used it.
    
    index = 1
            
    while (NA %in% tickets[ i,]) {
        
        if (check( c( tickets[ i, 2], remainPairs[ index, 2])) == 0) {
            # We want to be careful that our 2nd and third numbers have not been
            #   paired yet.
            
            tickets[ i, 3] = remainPairs[ index, 2]
                # select third number for ith ticket
            
            remainPairs = remainPairs[ -index,]
            
            a = rowToDelete( c( tickets[ i, 2], tickets[ i, 3]))
            
            if ( is.matrix( remainPairs)) {
            
                remainPairs = remainPairs[ -a,]
            
            } else {
            
                remainPairs = matrix( remainPairs, ncol = 2)[ -a,]
                    # prevent error from converting 1 x 2 matrix to vector
            
            }
        
        } else {
        
            index = index + 1
        
        }
        
    }
    
}

tickets

remainPairs

# We have accomplished our goal of picking every possible pair of numbers 1-7
#   with seven tickets. Our solution is not unique.
