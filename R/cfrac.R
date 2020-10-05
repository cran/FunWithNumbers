# An analytic continued fraction generator
#  you can calculate irrationals to arbitrary precision by setting
#  the input num and denom to  (irat_num)*1eN , 1eN for N some nice bigZ integer

# xy.coords will not do what I want for a simple vector.
cfrac <-function(num,denom,...) {
# clean out roundoffs for numerics
if (is.numeric(num) && is.numeric(denom) ) {
	numdenom = c(as.double(floor(num)), as.double(floor(denom)) ) 
} else numdenom = c(num, denom) # for mpfr, bigz, etc.

if (numdenom[2] == 0 || numdenom[1] == 0 ) stop('zeros not allowed')
if (numdenom[1] == 1) return (invisible(c(0,denom)) )

intvec = NULL # initialize list of "integer parts" of each level
jlev = 1 # start vector index
# numerator == 0 means there was a common factor; this is easiest way to catch it.
while (!is.element(numdenom[1], c(0,1)) && numdenom[2] != 0 ) {
	intvec[jlev] = floor(numdenom[1]/numdenom[2] )
	num = numdenom[1] - (intvec[jlev] * numdenom[2] )
	numdenom = c(numdenom[2],num) # reciprocalling
	 jlev = jlev + 1
	}
return(invisible(intvec))
}


# build the latex formula and the inline equation for the continued fraction 
# generated with cfrac. 
cf2latex <-function(vals,...) {
# convert numeric input
vals <- as.character(vals)
eqn <- paste0(vals[1],' + ')

for (jj in 2:(length(vals) ) ) {
	eqn <- paste0(eqn, '1/(',vals[jj],' + ',collapse='')
	}
# clean up trailing "+" and unwanted parenthesis
eqn <- sub('[(]([0-9]{1,}) [+] $', '\\1', eqn, perl=TRUE)
#finish the inline version 
clospar <-paste0(rep(')',jj-2),collapse='')
eqn <- paste0(eqn, clospar,collapse='' )
# Now a Q&D conversion to LaTeX.  It's essentially impossible to make a regex that will find a backslash
# and replace with two backslashes, because "\f" is viewed as an excaped char by all the tools.
# (I believe package stringr has functions to work around this, but I don't want to have Depends)
# So first make the string for use in grqphics, then reduce double\\ to single \\ for use in LaTeX compilers.
texeqn <- gsub('1/[(]', '\\\\\\frac{1}{' , eqn)
texeqn <- gsub('[)]', '}', texeqn)
#cleanup
texeqn <- gsub('1/', '\\\\\\frac{1}{', texeqn)
texeqn <- paste0(texeqn,'}',collapse='')
texexpr <- paste0('$',texeqn,'$')
# now remove the doubles....
texeqn<- gsub('\\f','\f',texeqn,fixed=TRUE)
return(invisible(list(eqn=eqn, texeqn = texeqn, texexpr = texexpr)) )
}


