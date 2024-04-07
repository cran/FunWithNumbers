\name{factorion}
\alias{factorion}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find  Factorion Numbers. ~~
}
\description{
This function calculates the factorion value for the specified input integer. A number is a 'factorion' if it equals the sum of the factorials of its digits in a given base.   
}
\usage{
factorion(x, base = 10 , findcycle = FALSE, maxiter = 100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
The integer, or \code{bigz} integer, or character string representing an integer. 
}
\item{base}{
The the base of the input integer \code{x}. Bases 2 through 36 are supported. Default is 10.
}
\item{findcycle}{
Logical value indicating whether or not to test for cyclic values.  See Details for more information.
}

\item{maxiter}{
A "safety switch" to avoid possible infinite loops, terminating the function prior to convergence. Only applicable when \code{findcycle} is TRUE.
}
}
\details{
Most numbers are neither factorion nor part of a factorion cycle. A cycle occurs if the sequence \code{factorion(factorion(...factorion(x)))} repeats.  
}
\value{
A list containing:
\code{x} echoing back the input argument.
\code{isfactorion} value 1 if \code{x} is a factorion and 0 otherwise.
\code{theseq} the sequence of values as each output is fed back to the input. 
}
\author{
Carl Witthoft,  \email{carl@witthoft.com} 
}
\references{
 \url{https://en.wikipedia.org/wiki/Factorion}
 \url{https://oeis.org/A014080}
}
\examples{
(factorion(3,base=8,findcycle= TRUE))
# 3   6   1320   12; isit = 0
(factorion(41, base = 6))
# isit = 1

}