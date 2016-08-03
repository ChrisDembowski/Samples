
# Special Pythagorean triplet
# Problem 9

# A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

# a2 + b2 = c2
# For example, 32 + 42 = 9 + 16 = 25 = 52.
#
# There exists exactly one Pythagorean triplet for which a + b + c = 1000.
# Find the product abc.




# Note: by triangle inequality, a + b > c (equality not possible, since a
#   triangle is formed)
    

for (C in 334:499) {
    
    for (A in 1:(ceiling( (1000 - C) / 2) - 1)) {
    
        B = 1000 - A - C
        
        if (A ^ 2 + B ^ 2 == C ^ 2) {
        
            triplet = c( A, B, C)
            
            # We are given that only one solution exists
            C = 499
            
            break
        
        }
    
    }

}

prod( triplet)