#2) look-and-say, aka Morris  1 11, 21, 1112 ... 
# what if we start with any other integer? not much
# let the input be a previous numeric vector. 
morris <- function(x,reps) {
x <- floor(x)
if (any(x > 9) ) stop('Single-digit integer values only')
 
out <- vector('list',length=reps+1) 
out[[1]] <- x  
for (jj in 1:reps){
	rtmp <- rle(out[[jj]])
	out[[jj+1]] <- c(as.vector(rbind(rtmp$lengths, rtmp$values))) 
}
return(invisible(out))
}


# this calculates number of unique values in cross-table of sums and products for any 
# input set of numbers.  Theory is that they should sum to near N^2 . 
sptable <- function (x) {
#cleaning up
x <- floor(as.numeric(x))
uniqsum <- unique(as.vector(gmp::outer(x,x,'+')))
uniqprod <- unique(as.vector(gmp::outer(x,x)))
spratio <- length(uniqsum)/length(uniqprod)
# calculate the actual exponent. 
exponentOfN <- log(length(uniqsum) + length(uniqprod) )/log(length(x))
return(invisible(list(uniqsum= length(uniqsum), uniqprod= length(uniqprod), spratio = spratio, exponentOfN = exponentOfN)))
}
  

#Juggler: the product version of collatz:  
#  x[j+1] = floor( if even, x[j]^0.5; if odd x[j]^1.5) 
juggatz <-function(x, maxiter = 1000, prec = 1e2) {
# only want one value
if (length(x) > 1) {
	x <- x[[1]]
	warning('Only first element of x will be used')
}
# as.bigz(bigq) acts as floor function

x <- floor(Rmpfr::mpfr(x[1], prec) )
if (length(x) > 1) x<- x[1] #silent dumping
y<-rep(Rmpfr::mpfr(0,prec),maxiter)
y[1]<-x
# gmp doesn't support irrationals, so have to do x^k via mpfr
for (jj in 2:maxiter) {
# check both up and down.  check log(y) < prectmp/100 to decrease....
	 prectmp <- Rmpfr::getPrec(y[jj-1])
	 if (log(y[jj-1]) > prectmp/10 ){
		 y[jj-1] <- Rmpfr::roundMpfr(y[jj-1],prectmp*10)
		} else {
			if(log(y[jj-1]) < prectmp/100) y[jj-1] <- Rmpfr::roundMpfr(y[jj-1],prectmp / 10)
	 		}
	y[jj] <- floor(y[jj-1]^(0.5 + y[jj-1]%%2))  # cool, huh
	if (y[jj] < 2 ) return(invisible(y[1:jj]))
	}
warning(c('Seq init with ',x,' not converged (yet)'))
return(invisible(y))
}
