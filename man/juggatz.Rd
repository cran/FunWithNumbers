\name{juggatz}
\alias{juggatz}
\title{
Function which calculates the "Juggler" sequence ~~
}
\description{
The "Juggler" sequence is similar to the Collatz sequence, but generates exponential changes rather than multiplicative changes to calculate each term. See Details for the algorithm. 
}
\usage{
juggatz(x, maxiter = 1000, prec = 100)
}
\arguments{
   \item{x}{
The numeric, \code{mpfr}, or \code{bigz} integer to start with. 
}
  \item{maxiter}{
A "safety switch" to avoid possible lengthy runtimes (when starting with very very large numbers), terminating the function prior to convergence. 
}
  \item{prec}{
This specifies the number of binary digits of precision to use when the function converts numeric input \code{x} to a \code{mpfr} object. 
}
}
\details{
The Juggler algorithm uses the following rules: x[j+1] = floor( if even, x[j]^0.5; if odd x[j]^1.5).  
Since the \code{mpfr}-class objects represent approximations to the various powers and roots calculated, \code{juggatz} dynamically adjusts the number of bits of precision for the next value in the sequence. This ensures that the correct decision as to even or odd is made at each step. 

}
\value{
A vector of \code{mpfr} integers representing the sequence, either to convergence or as limited by \code{maxiter}
}
\author{
Carl Witthoft,  \email{carl@witthoft.com} 
}

\examples{
 (juggatz(10))
# 8 'mpfr' numbers of precision  10 .. 100  bits 
# [1] 10  3  5 11 36  6  2  1
(juggatz(37))
# 18 'mpfr' numbers of precision 10 .. 1000 bits 
# [1]  37  225  3375  196069 86818724  9317
# [7]  899319 852846071 24906114455136 4990602  2233  105519
# [13] 34276462  5854  76  8  2  1
}

