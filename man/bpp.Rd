\name{bpp}
\alias{bpp}
\title{
Function which calculates pi, or other irrationals,  using the BaileyBorweinPlouffe formula  ~~
}
\description{
THe BPP algorithm consists of a double summation over specified fractions. Rather than go into the gory details here, please refer to the link in the References section.  
}
\usage{
bpp(k,pdat = c(1,16,8,4,0,0,-2,-1,-1,0,0),mpbits = 400)
}
\arguments{
   \item{k}{
The number of terms in the series to calculate.  Note that zero is a valid entry. 
}
  \item{pdat}{
The parameter \code{P} which is used to define the coefficients used in all fractions in each term of the series.  In brief, pdat contains the following BPP parameters:  \code{pdat(s,b,m,A)} where \code{A}comprises all elements of the vector \code{pdat} after the first three.  There are strict rules about the length of \code{A}; see the Details section.   
}
  \item{mpbits}{
This specifies the number of binary digits of precision to use when the function converts \code{gmp::bigq} fractions to \code{mpfr} extended precision decimal representation.  Failure to use a large enough value may result in a limit to the true output precision. 
}
}
\details{
The BPP algorithm calculates the sum{K=0,k, 1/(b^K) * FracSum} , where FracSum is defined by the sum(M=1,m, A[M]/(m*K + M)^s) .  This means that the number of elements of \code{A} must equal \code{m}. Zero values are legal and are used to reject fractions not wanted in the inner sum. 

The default values for \code{pdat} correspond to the coefficients used to generate pi (the sum to infinity is mathematically equal to pi).
Other values have been found to calculate a few other irrationals but there is as yet no known procedure to generate the \code{pdat} set for any given number.
}
\value{
A list containing \code{bigq} , the \code{gmp} fraction calculated, and \code{mdec}, the \code{mpfr} decimal representation of said fraction. 
}

\references{
\url{https://en.wikipedia.org/wiki/Bailey-Borwein-Plouffe_formula}   and references cited there. 
 }
 
\author{
Carl Witthoft,  \email{carl@witthoft.com} 
}

\examples{
# Compare the decimal outputs to the first 130 digits of pi, which are:
#  [1] 3 . 1 4 1 5 9 2 6 5 3 5 8 9 7 9 3 2 3 8 4 6 2 6 4
#  [26] 3 3 8 3 2 7 9 5 0 2 8 8 4 1 9 7 1 6 9 3 9 9 3 7 5
#  [51] 1 0 5 8 2 0 9 7 4 9 4 4 5 9 2 3 0 7 8 1 6 4 0 6 2
#  [76] 8 6 2 0 8 9 9 8 6 2 8 0 3 4 8 2 5 3 4 2 1 1 7 0 6
# [101] 7 9 8 2 1 4 8 0 8 6 5 1 3 2 8 2 3 0 6 6 4 7 0 9 3
# [126] 8 4 4 6 0

# Lots of precision, but most of the digits are inaccurate.
 (bpp(5))
# $bigq
# Big Rational ('bigq') :
# [1] 40413742330349316707/12864093722915635200
# 
# $mdec
# 1 'mpfr' number of precision  400   bits 
# [1] 3.14159265322808753473437803553620446955
# 852801219780193481442230321585101644290504893051
# 16201439239799241252867682875513665

# extend the series.
(bpp(20))
# $bigq
# Big Rational ('bigq') :
# [1] 6978810534836185743790248010742839687036
# 9348283327260905420704007804969465293
# 222142438704194558751308818610402859379
# 08911653929817878006825259792072704000
# 
# $mdec
# 1 'mpfr' number of precision  400   bits 
# [1] 3.1415926535897932384626433832513
# 62615881909316518417908555365030283
# 2142940981052934064597204233958787472300102294523391693

# Accurate but low precision
 bpp(20,mpbits=20)
# $bigq
# Big Rational ('bigq') :
# [1] 6978810534836185743790248010742839
# 6870369348283327260905420704007804969465293
# 2221424387041945587513088186104028593790891
# 1653929817878006825259792072704000
# 
# $mdec
# 1 'mpfr' number of precision  20   bits 
# [1] 3.1415939
}


