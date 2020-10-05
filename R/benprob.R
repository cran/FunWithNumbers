# benford, for decimal:
#  prob(d) = log10(d+1) - log10(d)  aka  log10(1+1/d)
# d is 1:(base-1) in the general case
# Then  log_base(1+1/d) will hold true
# don't let base be < 2 and make it be an integer. Ya wanna do hex, be my guest.

benprob <- function(numsamp=100, numbase=10){
numbase <- floor(numbase)
if (numbase < 2) stop ('Number base must be realistic')
digs <- 1:(numbase-1) # 
probs <- log(1+1/(1:(numbase-1)), numbase)
samps <- sample(digs, numsamp, replace =TRUE, prob= probs)
return(invisible(samps))
}

