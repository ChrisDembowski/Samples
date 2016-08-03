
# https://projecteuler.net/problem=19

# Counting Sundays
# Problem 19

# You are given the following information, but you may prefer to do some research for yourself.

# 1 Jan 1900 was a Monday.
# Thirty days has September,
# April, June and November.
# All the rest have thirty-one,
# Saving February alone,
# Which has twenty-eight, rain or shine.
# And on leap years, twenty-nine.
# A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
# How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?








# There are 14 possible calendars. A year can have 365 days (common) or 366 days
#   (leap). Additionally, a year can begin on any of the 7 days of the week.
#   2 x 7 = 14. We need only count the number of time the first of the month
#   falls on Sunday for each of the 14 calendar versions.



year = 1901:2000
    # We are interested in years 1901 thru 2000

monthLengths = matrix( c( 31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L,
    31L), ncol = 2, nrow = 12)

monthLengths[ 2, 2] = 29L

firstDayOfMonth = matrix( nrow = 14, ncol = 12)
    # Columns: months, Rows: calendar versions

for (version in 1:14) {
    
    firstDayOfMonth[ version, 1] = (version - 1L) %% 7L
        # 0 = Sun, 1 = Mon, ..., 6 = Sat.

    for (Month in 2:12) {
    
        firstDayOfMonth[ version, Month] =
            (firstDayOfMonth[ version, Month - 1L] +
            monthLengths[ Month - 1L, ceiling( version / 7.5)]) %% 7L
    
    }

}
# colnames( firstDayOfMonth) = c( "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# rownames( firstDayOfMonth) = c( "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", "v10", "v11", "v12", "v113", "v14")

# We want to count the number of firsts of the month that fall on Sunday for
#   each calendar version

numFirstsOnSundayByVersion = vector( length = 14)

for (i in 1:14) {

    numFirstsOnSundayByVersion[ i] =
        length( firstDayOfMonth[ i, firstDayOfMonth[ i,] == 0L])

}

# To determine which calendar version fits a given year, we must first identify
#   the year type, common or leap.

yearType = function( x) {

    if (x %% 4L != 0L) {

        type = 1L
            # type 1 is common year
    
    } else if ((x %% 100L == 0L) & (x %% 400L != 0L)) {

        type = 1L

    } else {

        type = 2L
            # type 2 is leap year
    
    }

}
firstDay = vector( length = length( year))
    # The day of week on which Jan 1 falls

type = firstDay
    # common (1) or leap (2) year

calendarVersion = firstDay
    # calendar version for each year during period of interest

numFirstsOnSundayByYear = firstDay
    # tghe number of firsts of the month that fall on Sun each year
    
########################################################
# THE FOLLOWING VARIABLE IS SPECIFIC TO STARTING YEAR! #
########################################################
firstDay[ 1] = 2L
    # Jan 1, 1901 was Tuesday. Sun = 0, Mon = 1, ..., Sat = 6, so 01/01/01 = 2

# Now we combine type and day of Jan 1 to determine calendar version. 
type[ 1] = yearType( year[ 1])

calendarVersion[ 1] = (type[ 1] - 1) * 7L + firstDay[ 1] + 1L

numFirstsOnSundayByYear[ 1] = numFirstsOnSundayByVersion[ calendarVersion[ 1]]

for (i in 2:length( year)) {

    type[ i] = yearType( year[ i])
    
    firstDay[ i] = (firstDay[ i - 1L] + type[ i - 1L]) %% 7L
    
    calendarVersion[ i] = (type[ i] - 1L) * 7L + firstDay[ i] + 1L
    
    numFirstsOnSundayByYear[ i] =
        numFirstsOnSundayByVersion[ calendarVersion[ i]]

}

# Count the Sundays that fall on 1st of month
sum( numFirstsOnSundayByYear)

