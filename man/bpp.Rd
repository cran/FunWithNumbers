\name{bpp}
\alias{bpp}
\title{
Function which calculates pi, or other irrationals,  using the BaileyBorweinPlouffe formula  ~~
}
\description{
THe BPP algorithm consists of a double summation over specified fractions. Rather than go into the gory details here, please refer to the link in the References section.  
}
\usage{
bpp(k,pdat = c(1,16,8,4,0,0,-2,-1,-1,0,0), init = 0, chunk = 1e4,...)
}
\arguments{
   \item{k}{
The number of terms in the series to calculate.  Note that zero is a valid entry. If a single value, the terms \code{0:k} are used.  If two values are provided (see information for the input parameter \code{init}), then the terms \code{k{1}:k[2]} are run.
}
  \item{pdat}{
The parameter \code{P} which is used to define the coefficients used in all fractions in each term of the series.  In brief, pdat contains the following BPP parameters:  \code{pdat(s,b,m,A)} where \code{A}comprises all elements of the vector \code{pdat} after the first three.  There are strict rules about the length of \code{A}; see the Details section.   
The default value will calculate pi. 
}
\item{init}{
If there's a previous value calculated with \code{bpp} for a certain value of \code{k}, this term allows the user to continue the calculation. Assign the previous output's \code{bppgmp} value to  \code{init} . Note that one must set up the input \code{k} to start at one more than the previous run's maximum "k" value.  
}
\item{chunk}{ 
There is a call to \code{sum} in the main loop of this function. Use \code{chunk} to specify how many terms to pass to the sum call at a time, thus reducing the peak memory requirements of this function.  The more RAM available on your machine, the larger this number can be.  Set to a value greater than the argument \code{k} to run a single "chunk," which is the fastest approach if sufficient memory is available. 
}
\item{...}{
Optional arguments to pass to \code{\link[Rmpfr]{.bigq2mpfr}} . 
}
}
\details{
The BPP algorithm calculates the sum{K=0,k, 1/(b^K) * FracSum} , where FracSum is defined by the sum(M=1,m, A[M]/(m*K + M)^s) .  This means that the number of elements of \code{A} must equal \code{m}. Zero values are legal and are used to reject fractions not wanted in the inner sum. 

The default values for \code{pdat} correspond to the coefficients used to generate pi (the sum to infinity is mathematically equal to pi).
Other values have been found to calculate a few other irrationals but there is as yet no known procedure to generate the \code{pdat} set for any given number.
}
\value{
A list containing \code{bppgmp} , the \code{gmp} fraction calculated; \code{bppval}, the \code{mpfr} decimal representation of said fraction; and \code{kvals}, echoing the input \code{k}. 
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

# extend the series.
(bpp(20))

}


