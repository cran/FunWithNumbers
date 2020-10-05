# aliquot: subtract the sum all proper divisors of a number from that number to get the next number. This
# is closely tied in w/ sociable and amicable numbers.  Remember 1 is a proper divisor
# 

 aliquot<-function(x, maxiter=100) {
  # Need some sort of input validation, but might well want to feed a bigZ input.
  x <-as.bigz(x)
 aliseq<-rep(as.bigz(NA),maxiter)
 aliseq[1]<- as.bigz(x)
 for (j in 2: maxiter) {
 #  In case sfsmisc is loaded, get the right one
 #  gmp::factorize() returns x when x is prime. 
	primfacs <- (gmp::factorize(aliseq[j-1]))
# if it's prime, then primfacs is of length one, so can kill the loop off.
	if (length(primfacs) <= 2) {
	# if length is 2, there's only one possible sum; if length==1, it's prime
	# added that "unique" for perfect squares of primes 
		aliseq[j] <- (1*(!(length(primfacs)-1)) + (length(primfacs)-1)*(sum(unique(primfacs))+1))
		} else { 
			allfacs<-unique(primfacs) #just save the primes once
			for (k in 2:(length(primfacs)-1) ) {
# calc all possible factors for all "depths" of combinations
# sadly, combn(..simplify=TRUE) can't handle nonatomic types
# fudgefix (submitted request to CRAN for fix to combn) - 
				combtmp <- combn(primfacs,k,FUN = prod,simplify=FALSE)
				newfacs <- rep(as.bigz(0),length(combtmp))
				for (jcom in 1:length(combtmp)){
					newfacs[jcom] <- prod(combtmp[[jcom]])
				}
#	<use when combn upgraded>	newfacs <- unique(combn(as.numeric(primfacs),k,prod)) 
				allfacs<-c(allfacs,unique(newfacs))
				}
#	browser()
			aliseq[j] <- sum(c(allfacs,1))
#			aliseq[j] <- aliseq[j-1] - sum(c(allfacs,1))
			}
		if (aliseq[j] <= 1) return(invisible(aliseq[!is.na(aliseq)])) 
#check HERE for repetition 
		if (aliseq[j] == aliseq[j-1]){
			return(invisible(aliseq[!is.na(aliseq)]))
		}
#TODO: do i want to try to find cyclic pattern reps? 
	}
# if get here, either incomplete or periodicity happened. 
warning("cyclic or not converged")
return(invisible(aliseq[!is.na(aliseq)]))
}

# Hailstone / Collatz sequence
# If the current number is even, divide it by two; else if it is odd, multiply it by three and add one.   Convergence is < 200 for starts < 10 million or so.
collatz<-function(x, maxiter = 1000) {
x <- as.bigz(x)
if (length(x) > 1) x<- x[1] #silent dumping
y<-rep(as.bigz(0),maxiter)
y[1]<-x
for (jj in 2:maxiter) {
	y[jj] <- if( as.integer( y[jj-1]%%2 ) ) y[jj-1] * 3 +1 else y[jj-1]/2
	if (y[jj] <= 2) return(invisible(y[1:jj]))
	}
	warning('not converged (yet)')
	return(invisible(y))
}


