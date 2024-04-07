#In number theory, a perfect digital invariant (PDI) is a number in a given number base () that is the sum of its own digits each raised to a given power ().
#happy numbers are those for which the PDI(PDI(PDI....PDI(x))) either is 1 or cyclic

#More generally, a 
# b}-happy number is a natural number in a given number base 
#b} that eventually reaches 1 when iterated over the perfect digital invariant function for 
# p=2}
   
dontWorry <- function(x, pwr = 2, base = 10, maxiter = 100){
	xin <- x[[1]]  #retain in original base
	x <- base2base(xin,base,10)[[1]]  #a bigz value
	theseq <- as.bigz(NULL)
	idx = 1
	cyclic = FALSE
	while (x > 1 && !(x %in% theseq) && idx <= maxiter) {
		theseq <- c(theseq, x)
		x <- pdi(x, pwr, base)
		idx = idx + 1
	}

	if(x %in% theseq) cyclic = TRUE
#	browser()
	tmp <- base2base(c(theseq, x), 10, base, classOut='character')
	seqvec <- vector(length=length(tmp))
	for (jt in 1:length(tmp)) seqvec[jt] <- tmp[jt][[1]]
	return(list(isHappy = (x == 1), theseq = seqvec ,cyclic = cyclic)  )	
}


pdi <- function( x, pwr = 2, base = 10) {
#	x <- floor(x) #no fractions; but not needed as this is not to be used standalone
	out <- as.bigz(0)
	while (x > 0) {
		out <- out + (x%%base)^pwr
		x <- floor(x/base)  #probably faster than 'as.bigz(x/base)'
	}
	return(out)
}