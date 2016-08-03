# https://ProjectEuler.net/problem=54

# Poker hands
# Problem 54

# In the card game poker, a hand consists of five cards and are ranked, from lowest to highest, in the following way:

# High Card: Highest value card.
# One Pair: Two cards of the same value.
# Two Pairs: Two different pairs.
# Three of a Kind: Three cards of the same value.
# Straight: All cards are consecutive values.
# Flush: All cards of the same suit.
# Full House: Three of a kind and a pair.
# Four of a Kind: Four cards of the same value.
# Straight Flush: All cards are consecutive values of same suit.
# Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.
# The cards are valued in the order:
# 2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.

# If two players have the same ranked hands then the rank made up of the highest value wins; for example, a pair of eights beats a pair of fives (see example 1 below). But if two ranks tie, for example, both players have a pair of queens, then highest cards in each hand are compared (see example 4 below); if the highest cards tie then the next highest cards are compared, and so on.

# The file, poker.txt <https://projecteuler.net/project/resources/p054_poker.txt>, contains one-thousand random hands dealt to two players. Each line of the file contains ten cards (separated by a single space): the first five are Player 1's cards and the last five are Player 2's cards. You can assume that all hands are valid (no invalid characters or repeated cards), each player's hand is in no specific order, and in each hand there is a clear winner.

# How many hands does Player 1 win?



setwd( "~/Documents/Programming/R/R functions and scripts/Project Euler/54 Poker Hands")

getwd()

    
    
    
id = function( cards) {
    
    numCards = length( cards)
        
    cards = strsplit(cards, "")
        
    suits = vector( "character", length = numCards)
        
    ranks = vector( "character", length = numCards)
        
    for (i in 1:numCards) {
        
        suits[ i] = cards[[ i]][ 2]
            
        ranks[ i] = cards[[ i]][ 1]
        
    }
        
    rm( cards)
        
        
    suits = sub( "C", "1", suits, fixed = TRUE)
            
    suits = sub( "D", "2", suits, fixed = TRUE)
            
    suits = sub( "H", "3", suits, fixed = TRUE)
            
    suits = sub( "S", "4", suits, fixed = TRUE)
            
    suits = as.integer( suits)
        
        
    ranks = sub( "T", "10", ranks, fixed = TRUE)
        
    ranks = sub( "J", "11", ranks, fixed = TRUE)
            
    ranks = sub( "Q", "12", ranks, fixed = TRUE)
            
    ranks = sub( "K", "13", ranks, fixed = TRUE)
            
    ranks = sub( "A", "14", ranks, fixed = TRUE)
        
    ranks = as.integer( ranks)
        
    ranks = ranks - 1L
        # Deuce = 1 thru Ace = 13
        
        
    cards = vector( "integer", length = numCards)
        
    for ( i in 1:numCards) cards[ i] = (suits[ i] - 1L) * 13L + ranks[ i]
        
    cards = sort( cards, decreasing = TRUE)
        
    return( cards)
         #   Each element of cards is a unique integer in 1:52, such that
         #   rank = card[ i] %% 13 (2-A represented as 1-13) and 
         #   suit = floor( card[ i] - 1 / 13) (C, D, H, S -> 0, 1, 2, 3)
        
}





bestHand = function( cards = CARDS) {
    
    numCards = 5
        
    suits = as.integer( floor( (cards - 1) / 13) + 1)
        # (suits %in% 1:4) == TRUE
        
    ranks = cards %% 13L
        # Deuce == 1, K == 12, A == 0
        
    ranks[ ranks == 0] = 13L
        # A == 13
        
    ranks = sort( ranks, decreasing = TRUE)
        
    value = 0
        
    tiebreaker = ranks
        
    straight = FALSE
        
    flush = FALSE
        
    rankTable = tabulate( ranks)
        # count number of each rank
        
    suitTable = tabulate( suits)
        # count number of each suit
            
        
        
    if (max( suitTable) == 5) {
        # test for flush
            
        flush = TRUE
            
        value = 5
            
        tiebreaker = max( ranks)
            
    }
        
    if (identical( ranks - min( ranks), 4:0)) {
        # check for straight, but miss A - 5 (Wheel)
            
        straight = TRUE
            
        value = 4
            
        tiebreaker = max( ranks)
        
    } else {
            
        ranks = ranks %% 13L
            # Change A from 13 to 0
            
        ranks = sort( ranks, decreasing = TRUE)
            
        if (identical( ranks, 4:0)) {
            # check for Wheel
            
            straight = TRUE
                 
            value = 4
                 
            tiebreaker = max( ranks)
                 
        }
            
        ranks[ ranks == 0] = 13L
            # change A back to 13
        
    }
        
    if (flush && straight) {
        # test for straight/royal flush
            
        value = 8
                
    } else if (max( rankTable) == 4) {
            # test for 4 of a kind
            
        value = 7
            
        tiebreaker = ranks[ duplicated( ranks)]
            
    } else if (max( rankTable) == 3 && length( which( rankTable != 0)) == 2) {
            # test for full house
            
        value = 6
            
        tiebreaker = which( rankTable == 3)
            
    } else if (straight || flush) {
                
    } else if (max( rankTable) == 3 && length( which( rankTable != 0)) == 3) {
        # test for 3 of a kind
            
        value = 3
            
        tiebreaker = which( rankTable == 3)
            
    } else if (max( rankTable) == 2 && length( which( rankTable != 0)) == 3) {
            # test for 2 pair
            
        value = 2
            
        tiebreaker = c( max( which( rankTable == 2)),
            min( which( rankTable == 2)), which( rankTable == 1))
            
        tiebreaker           
            
    } else if (max( rankTable) == 2 && length( which( rankTable != 0)) == 4) {
            # test for pair
            
        value = 1
            
        tiebreaker = c( which( rankTable == 2), which( rankTable == 1)[ 3:1])
            
    }
        
    c( value, tiebreaker) 
    
}





filePath = "./p054_poker.txt"
		
fileUrl = "https://projecteuler.net/project/resources/p054_poker.txt"
		
if (! file.exists( "p054_poker.txt")) {
		
	download.file( fileUrl, filePath, method = "curl")
		
}

hands = read.table( filePath, colClasses = "character")

player1hands = as.matrix( hands[ , 1:5])

player2hands = as.matrix( hands[ , 6:10])

rm( hands, filePath, fileUrl)


player1wins = 0

for (i in 1:1000) {

    player1 = bestHand( id( player1hands[ i,]))
    
    player2 = bestHand( id( player2hands[ i,]))
    
    for (j in 1:6) {
    
        if (player1[ j] > player2[ j]) {
        
            player1wins = player1wins + 1
            
            break
        
        } else if (player1[ j] < player2[ j]) {
        
            break
        
        }
    
    }

}

player1wins