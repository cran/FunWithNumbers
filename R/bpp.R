# BPP algo for finding pi. 
# pi the default coefficient set

# P(s,b,m,A) where
# s is the power of the collection of denominators
# m is the coeff of k in the denom of all the "A" fractios
# b is the lead fraction denom
# A is the set of numerators in the internal sum:
#  sum(j=1:m){a[j]/(mk+j)^s}
# so use lots of zeros;
# pi is P{1,16,8,c(4,0,0,-2,-1,-1,0,0)}  <- must be as long as m

bpp <- function(k,pdat = c(1,16,8,4,0,0,-2,-1,-1,0,0),mpbits = 400){
	if(length(pdat) != pdat[3]+3){
		stop('The number of "A" coeffs does not match value of "m" ')
	}
	pdat = as.bigq(floor(pdat))  #gets rid of decimal crap, too
	
	s = pdat[1]
	b = pdat[2]
	m = pdat[3]
	A = pdat[-(1:3)]
	mm = as.numeric(m) # for loop indexing
	bpp  = 0
	if (length(k) > 1) {
		warning('Only first value of k is used')
		k <- k[1]
	}
#really should memoize so can just add terms instead of starting over on every call
	for (jk in 0:k){
		msum <- 0
		for(jm in 1:mm){
			msum <- msum + A[jm]/(m*jk + jm)^s
		}
		bpp <- bpp + msum * 1/b^jk
	}
# let's return the 'raw' bigq and also a mpfr decimal equivalents
	mnum <- mpfr(numerator(bpp), mpbits)
	mdenom <- mpfr(denominator(bpp), mpbits)
	thedec <- mnum/mdenom
	return(invisible(list(bigq = bpp, mdec = thedec)))
}
	