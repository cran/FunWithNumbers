# Kaprekar's constant

# any N-digit number w/ at least 2 dif't integers
# If N= 4, take abs(sort(orig) - reverse(sort(orig))) and repeat , sooner or later end up w/ 6174
# other things happen with dif't length inputs.

# notice that if you ever get to a repdigit it'll immediately converge to zero.


kaprekar <- function(x, base = 10, addZeros = FALSE, maxiter = 1000){
# limit to single input value.  data classes/types handled with as.character
	xin <- x[[1]]  #retain in original base
# keep this for use when addZeros is TRUE
numdig <- length(unlist(strsplit(as.character(x),'')))
theseq <- base2base(x,base,10)[[1]]
iter = 2
converge = FALSE
while(iter <= maxiter && converge == FALSE)	{

# sort the input digits in descending order and then reverse that.  
# have to do this in the specified base
	tmpin <- base2base(theseq[iter-1],10,base)[[1]]
	sortin <- sort(unlist(strsplit(as.character(tmpin),'')) )
# check for length if desired
	if(addZeros) {
		inlen <- length(sortin)
		sortin <- c(rep('0',times = numdig - inlen), sortin)
	}  
	sortrev <- rev(sortin)
	numdec <- base2base(paste0(sortin,collapse=''),base,10)[[1]]
	revdec <- base2base(paste0(sortrev,collapse=''),base,10)[[1]]
	theseq[iter] <- abs(numdec - revdec)  # is bigz now
# browser()
	if(length(intersect(theseq[iter] , theseq[1:(iter-1)]) ) ) converge = TRUE
	iter <- iter + 1
}
theseq <- theseq[1:(iter-1)]
#convert back to 'base' and de-listify
theseq <- base2base(theseq,10,base)	
	seqvec <- as.bigz(NULL)
	for (jt in 1:length(theseq)) seqvec[jt] <- theseq[jt][[1]]

return(invisible(list(theseq=seqvec, converged = converge)) )
	
}