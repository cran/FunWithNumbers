# factorion:  x ?= sum(factorial(digit_j))
# there are some cyclic values where sum(xdigit!) = y, sum(ydigit!) = z, ... and end up back at x again.   
# examples
# base 8 3 6 1320 12
# base 10 871 45361 871
# base6  noncyclic  41, 42
factorion <- function(x, base = 10 , findcycle = FALSE, maxiter = 100){
xin <- x[[1]]  #vector input not allowed
# Here's how to avoid repeat code in loop:
if (!findcycle) maxiter = 2
theseq <- as.bigz(rep(x,times= maxiter) ) #later truncate if converged
iter = 2
converge = FALSE
while(iter <= maxiter && converge == FALSE)	{


	digdec <-as.bigz( unlist( base2base(unlist(strsplit(as.character(x),'')), base, 10, classOut = 'numeric')) )  
	digfact <- sum(factorial(digdec))
	theseq[iter] <- digfact
	if(length(intersect(theseq[iter] , theseq[1:(iter-1)]) ) ) converge = TRUE
	iter = iter + 1
#convert back to 'base' to get the characters, then reconvert to base10
	x <- base2base(digfact, 10, base, classOut = 'character') 
}
theseq <- theseq[1:(iter-1)]
isit <- !(theseq[1] == theseq[2])

return(invisible(list(x = xin, isfactorion = isit, theseq = theseq)))
}