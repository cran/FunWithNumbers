\name{cfrac}
\alias{cfrac}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Generate the continued-fraction form of an input number
}
\description{
This function takes as input the numerator and denominator, as integers or bigz values, of a value to be converted into continued-fraction form.  Irrationals can be processed to arbitrary precision by choosing a "closely-approximating" fraction. 
}
\usage{
cfrac(num, denom, ...)
}
\arguments{
  \item{num}{
Numerator of the fraction to be converted. If a double is provided, the \code{floor(num)} will be used internally.  \code{bigz}  and \code{mpfr} values are allowed. 
}
  \item{denom}{
Denominator of the fraction to be converted. Same rules as for the numerator. 
}
  \item{\dots}{
Reserved for future upgrades
}
}
\details{
Quoting from \url{https://en.wikipedia.org/wiki/Continued_fraction},  "In mathematics, a continued fraction is an expression obtained through an iterative process of representing a number as the sum of its integer part and the reciprocal of another number, then writing this other number as the sum of its integer part and another reciprocal, and so on."
}
\value{
A vector of integers of the same class as the inputs (int, bigz, etc) representing the values in each level of the continued fraction. 
}

\author{
Carl Witthoft,  \email{carl@witthoft.com} 
}

\seealso{
 \code{\link{cf2latex}} to generate both an inline text representation of the continued fractdion and LaTeX code for the continued fraction. 
}
\examples{
355/113 - pi
# small number
cfrac(355,113)
#[1]  3  7 16

}