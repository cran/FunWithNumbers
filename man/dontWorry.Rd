\name{dontWorry}
\alias{dontWorry}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Test for Happy Numbers ~~
}
\description{
This function calculates the sequence sum((digits_of_x)^p) to determine whether the value \code{x} is a "happy" number in the specified base.  
}
\usage{
dontWorry(x, pwr = 2, base = 10, maxiter = 100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
The integer,  \code{bigz} or character string representing an integer .
}
\item{pwr}{
The exponent defining the power to which each digit is raised.
}
\item{base}{
The base of the input \code{x} . Bases 2 through 36 are supported. 
}

  \item{maxiter}{
A "safety switch" to avoid possible lengthy runtimes (when starting with very very large numbers), terminating the function prior to convergence. 
}
}
\details{
The dontWorry sequence follows simple rules:
Take each digit in the current base and raise it to the specified power.  Take the sum as the next input.  If the sequence converges to 1, the number is happy.  If the sequence ends in a cycle, the number is not happy. 
}
\value{
A list containing:
isHappy:  a logical value indicating whether or not \code{x} is happy given the specified base and power. 
theseq: the sequence of values found
cyclic: TRUE if a cycle is found. (it should always be the case that \code{cyclic = !isHappy})
}
\author{
Carl Witthoft,  \email{carl@witthoft.com} 
}
\references{
 \url{https://en.wikipedia.org/wiki/Happy_number}
}
\examples{
(dontWorry(20))
# $isHappy
#[1] FALSE
#$theseq
#[1] "20"  "4"   "16"  "37"  "58"  "89"  "145" "42"  "20" 
#$cyclic
#[1] TRUE 
(dontWorry('2254', base = 6))
# $isHappy
# [1] TRUE
# 
# $theseq
# [1] "2254" "121"  "10"   "1"   
# 
# $cyclic
# [1] FALSE
}