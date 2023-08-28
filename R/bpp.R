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
#
# the ellipsis are passed to .bigq2mpfr for use if desired
# set chunk as large as your RAM is happy with to speed performance

bpp <- function(k,pdat = c(1,16,8,4,0,0,-2,-1,-1,0,0), init = 0, chunk = 1e4,...){
if(length(pdat) != pdat[3]+3){
	stop('The number of "A" coeffs does not match value of "m" ')
}
pdat = as.bigq(floor(pdat))  #gets rid of decimal crap, too
s = pdat[1]
b = pdat[2]
m = pdat[3]
A = pdat[-(1:3)]
Alen = length(A) # for loop indexing
bpp  = init
if (length(k) < 2 ) {
	K <- 0:k
} else{
	K <- k[1] : k[2]
	}
nchunk  <- as.numeric( ceiling(length(K)/chunk)) 
for (jchunk in 1:nchunk){
#  m isn't likely to be very big
	bjk <-  ( K[ (1 + chunk*(jchunk-1) ) : min( chunk +chunk*(jchunk-1),length(K)) ] )
	#do the calculation
	for (ja in 1:Alen){
		bpp <- bpp + sum(A[ja]/(m*bjk + ja)^s * 1/b^(bjk))
	}
}
thedec <- .bigq2mpfr(bpp, ...) 
return(invisible(list(bppgmp = bpp, bppval = thedec, kvals = k )))
}
