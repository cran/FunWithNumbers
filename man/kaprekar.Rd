\name{kaprekar}
\alias{kaprekar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate the Kaprekar Sequence. ~~
}
\description{
This function calculates the Kaprekar  sequence based on the selected starting integer.  
}
\usage{
kaprekar(x, base = 10, addZeros = FALSE, maxiter = 1000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
The integer,  \code{bigz}, or character string representing the integer to start with. 
}
\item{base}{
The base of \code{x}. Bases 2 through 36 are supported. 
}
\item{addZeros}{
If a calculated term has fewer digits than the original \code{x}, \code{addZeros}, when TRUE, prepends zeros to maintain the number of digits. 
}

  \item{maxiter}{
A "safety switch" to avoid possible lengthy runtimes (when starting with very very large numbers), terminating the function prior to convergence. 
}
}
\details{
The kaprekar sequence follows simple rules:
In the given base, sort the digits in ascending order.  Reverse that order, and calculate the absolute value of the difference. Feed that result back to the algorithm.  
In some cases, the sequence will converge to a value which produces itself.  In others, the sequence may fall into a repeating cycle. 
}
\value{
A list, containing:
theseq: a vector of \code{bigz} integers representing the sequence, either to convergence or as limited by \code{maxiter}
converged:  a logical value indicating whether the sequence reached a value or a cycle.  This is useful primarily if \code{maxiter} is reached and it's not immediately clear whether convergence occurred. 
}
\author{
Carl Witthoft,  \email{carl@witthoft.com} 
}
\references{
 \url{https://en.wikipedia.org/wiki/Kaprekar's_routine}
}
\examples{
(kaprekar(2031))
# $theseq
# Big Integer ('bigz') object of length 5:
# [1] 2031 3087 8352 6174 6174
# $converged
# [1] TRUE

(kaprekar('0099'))
# $theseq
# Big Integer ('bigz') object of length 3:
# [1] 99 0  0 
# $converged
# [1] TRUE

 (kaprekar('0099', addZeros = TRUE) )
# $theseq
# Big Integer ('bigz') object of length 6:
# [1] 99   9801 9621 8352 6174 6174
# $converged
# [1] TRUE
}