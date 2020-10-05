# Here is a sequence submitted by Jan Ritsema Van Eck in 2010 (to OEIS)


# 0, 0, 1, 0, 2, 0, 2, 2, 1, 6, 0, 5, 0, 2, 6, 5, 4, 0, 5, 3, 0, 3, . . . 

# The rule here is that you start with 0, and whenever you get to a number you have not seen before, the following term is a 0. But if the number k has appeared previously in the sequence, then you count the number of terms since the last appearance of k, and that number is the following term. In more detail:

# Term 1: The first term is 0 by definition.
# Term 2: Since we havent seen 0 before, the second term is 0.
# Term 3: Since we have seen a 0 before, one step back, the third term is 1
# Term 4: Since we havent seen a 1 before, the fourth term is 0
# Term 5: Since we have seen a 0 before, two steps back, the fifth term is 2.
# And so on.
# 'This is one of my top favourites of all time,' said Neil. 'The question is, how fast does it grow? And does every number eventually appear?' These are open questions.

#now allowing user to "seed" with previously generated sequence.  Usual GIGO warnings apply.
vaneck <-function(howlong = 100, ve = NULL, ...){
howlong = round(howlong[1])
if (!length(ve)) {
	ve = c(0,0)
}
# make sure we start at the right place if a sequence was supplied:
istart <- length(ve)
# prefill
ve <- c(ve,rep(NA,(howlong-istart)))
for (jj in istart:(howlong)) { 
	thefind <- which(ve[1:(jj-1)] == ve[jj])
	if (length(thefind)){
		ve[jj+1] <- jj-thefind[length(thefind)]
		} else ve[jj+1] <- 0
	}
uniqs <- unique(ve)
return(invisible(list(ve= ve, uniqs = uniqs)) )
}
