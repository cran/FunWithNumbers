# Find best fraction approximation to a supplied decimal value,
#  given some restriction on the size of integers to use in the fraction
bestFrac <- function(x, intrange) {
# note -  if intrange doesn't start with 1 , test the final result for reduction
if (is.character(x)) {
# find the decimal point, calculate desired denominator....
	splitit <- unlist(strsplit(x,'') )
# check locale	
	thedot <-  Sys.localeconv()['decimal_point']
	dotloc <- grep (paste0('[',thedot,']',sep='',collapse =''), splitit)
	if (length(dotloc)) {
# found a decimal point; 
# check for lead zeros AND watch for no integer part
		xint <- NULL
		if (dotloc > 1) {
			xint <- paste0(splitit[1:(dotloc-1)],sep='',collapse='')
			xint <- gsub('^[0]{1,}','', xint)
		}
		xint <- paste0(splitit[1:(dotloc-1)],sep='',collapse='')
		xint <- gsub('^[0]{1,}','', xint)
		xfilt <- paste0( c(xint, splitit[(dotloc+1):length(splitit)]), collapse= '', sep= '')
		xz <- as.bigz(xfilt)
# length(splitit) includes dec. point, so subtract 1 for that, and then
# subtract the number of digits to right of dot
		xdenom <- 10^as.bigz(length(splitit)- dotloc  )  
		xq <- as.bigq(xz,xdenom) 
		xfloat <- as.double(xq) # crude approx
	} else return(invisible( c(as.double(x),matcherr=0) ) ) #cheap trix
} else {
	stop('x must be a charstring of numerals presumably a with decimal point')
	}
# fix various kinds of inputs for intrange
if (length(intrange) == 1) {
	intrange = 1:intrange
} else {
	if (length(intrange) ==2) intrange = intrange[1]: intrange[2]
	} 	#else leave intrange alone and hope the user wasn't an idiot
goodmatch <- vector(length=2)
matcherr <- 100 # match error initializer -- smaller is better 
for (jmat in intrange) {
 # be careful! clamp lowest possible value at '1' 
		tryf <- max(floor(div.bigq(as.bigz(jmat), xq)),1 )
		tryc <- tryf + 1
		thisdiv = as.bigq(jmat,tryf)
		thistry <- abs(as.double(div.bigq((thisdiv-xq),xq)) ) # want 0.00000
		 if (thistry < matcherr[length(matcherr)] ) {  
			 goodmatch <- rbind(goodmatch,c(jmat,as.double(tryf))) 
			 matcherr <- c(matcherr,thistry )
			} 
		thisdiv = as.bigq(jmat,tryc)
		thistry <- abs(as.double(div.bigq((thisdiv-xq),xq)) )  # want 0.00000
		 if (thistry < matcherr[length(matcherr)]) {
			 goodmatch <- rbind(goodmatch,c(jmat,as.double(tryc)))
			 matcherr <- c(matcherr, thistry )
		}
}
# needed when intrange has gaps or doesn't start at 1
bestmatch <- goodmatch[nrow(goodmatch),]
# This ensures reduced fraction is returned
bestmatch <- bestmatch/gcd(bestmatch[1],bestmatch[2])
return(invisible(list(bestmatch = bestmatch, goodmatch = goodmatch, matcherr = matcherr) ))
}


