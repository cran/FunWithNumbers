# Harshad numbers:  pick a base.  Sum the digits of a number and
# if that number divided by the  sum is an integer, you win.
# equivalently  mod(sum(A0:An)),base) = 0  
# but, math being math, xbase(y)/zbase(y) is integral iff xbase10/zbase10 is,
# so far as I can tell.

harshad <- function (x, base = 10) {
x <- x[[1]]  #base2base will take care of 'floor' and the like
xdec <- base2base(x,base,10,classOut='character')[[1]]

# convert each digit to be able to sum, then convert the original number as a
# whole to base 10; divide that value by the sum of digits (in original base). 
digdec <- sum(as.bigz( unlist( base2base(unlist(strsplit(as.character(x),'')), base, 10, classOut = 'numeric')) )  )  #bigz

if(denominator(as.bigq(as.bigz(xdec),digdec)) ==1) foo = 1 else foo = 0
return(foo)

}