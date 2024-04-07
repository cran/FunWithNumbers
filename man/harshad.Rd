\name{harshad}
\alias{harshad}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find Harshad Numbers ~~
}
\description{
This function tests whether the input is a Harshad number in the specified base.  If the input divided by the sum of the digits (in the base) is an integer, then it is a Harshad number.   
}
\usage{
harshad(x, base = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
The integer,  \code{bigz} or character string representing the integer to test. 
}
\item{base}{
The base of the input \code{x} . Bases 2 through 36 are supported. 
}

}
\details{
The harshad sequence follows simple rules:
Take the digits of a number in the specified base and sum them. Divide the input value by that sum. If the result is an integer, i.e. the sum divides the input, it's a Harshad number. 
}
\value{
A single item equal to one if the input is Harshad and equal to zero if not. 
}
\author{
Carl Witthoft,  \email{carl@witthoft.com} 
}
\references{
 \url{https://en.wikipedia.org/wiki/Harshad_number}
}
\examples{
harshad(20)
# [1] 1
# 2 + 0 = 2, divides  20
harshad(37,base = 8)
#[1] 0
# 3 + 7 = 12 base8 , does not divide 37 base8 (in base 10, 10 does not divide 31)
}