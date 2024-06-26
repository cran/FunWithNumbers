# aliquot:  the sum all proper divisors of a number from that number to get the next number.  A[j+1] = sum(divisors(A[j]))
# is closely tied in w/ sociable and amicable numbers.  Remember 1 is a proper divisor
# n.b. cycle of 2 is "amicable" and cycle of 3 or more is "sociable" . If takes a few terms to fall into  cycle, "aspiring" 
 aliquot<-function(x, maxiter=100) {
   x <- gmp::as.bigz(x)
 aliseq<-rep(gmp::as.bigz(NA),maxiter)
 aliseq[1]<- gmp::as.bigz(x)
 converged = FALSE
 thecycle = NULL
 for (j in 2: maxiter) {
 #  In case sfsmisc is loaded, get the right one
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
				newfacs <- rep(gmp::as.bigz(0),length(combtmp))
				for (jcom in 1:length(combtmp)){
					newfacs[jcom] <- prod(combtmp[[jcom]])
				}
#	<use when combn upgraded>	newfacs <- unique(combn(as.numeric(primfacs),k,prod)) 
				allfacs<-c(allfacs,unique(newfacs))
				}
#	browser()
			aliseq[j] <- sum(c(allfacs,1))
			}
		if (aliseq[j] <= 1){
			converged = TRUE
			thecycle = aliseq[j]
			return(invisible(list(theseq = aliseq[!is.na(aliseq)], converged=converged, thecycle = thecycle)) )
		} #return(invisible(aliseq[!is.na(aliseq)])) 
# calculate how long the cycle is.  E.g.,starting with 6 or 220 or 1,264,460
		if(aliseq[j] %in% aliseq[1:(j-1)]){
			converged = TRUE
			cycidx <- which(aliseq[1:(j-1)] == aliseq[j])
			thecycle <- aliseq[cycidx:j]
			return(invisible(list(theseq=aliseq[!is.na(aliseq)],converged=converged, thecycle = thecycle )) )

		}
	} #end of j-loop
# if get here, either incomplete or periodicity happened. 
#warning("cyclic or not converged")
return(invisible(list(theseq=aliseq[!is.na(aliseq)],converged=converged, thecycle = thecycle )) )
}

# Hailstone / Collatz sequence
# If the current number is even, divide it by two; else if it is odd, multiply it by three and add one.   Convergence is < 200 for starts < 10 million or so.
# Fixes Nov 2022:  return truncated 'y' since nobody wants to see zeros; 
# Also, as with the "collatz" library, allow the 3,2,1 parameters to be adjusted if desired.
# Add a check whether we fell into a cycle, i.e. a value y[k] is repeated 
# and finally, 'maxKnown' lets you select an integer for which you know 1:maxKnown all converge, so you can stop there
collatz<-function(x, div=2, mul=3, add= 1, maxKnown=1, maxiter = 1000) {
x <- as.bigz(x[1])  #silent dumping
maxKnown <- as.bigz(floor(maxKnown))
cyclic = FALSE

y <- x
# unlikely but could a while( jj<maxiter && !cyclic)  work better?
for (jj in 2:maxiter) {
	y[[jj]] <- collatz_fun(y[[jj-1]],div,mul,add)
	if (y[[jj]] <= maxKnown   ) break 
# now check for cycles, keeeping in mind bigz doesn't play nice with %in%  . BUT, since
# y[jj] is  a single value, any() will work here
	if(any(y[[jj]] == y[[1:(jj-1)]]))	{
		cyclic = TRUE
		break
	}
}
	if (jj >= maxiter) 	warning('not converged (yet)')
	return(invisible(list(y = y, div = div, mul=mul, add= add, cyclic = cyclic) ) )
}

collatz_fun <- function(n, d, m, a){
	   # bigz/bigz returns a bigq even if that bigq has denominator 1
    # so we do a divq, "%/%", instead of div, to just get the bigz.

	if (n%%d ==0 ) ( n %/% d ) else (m*n) +a
}

