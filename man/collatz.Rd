\name{collatz}
\alias{collatz}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Test the Collatz Conjecture. ~~
}
\description{
This function calculates the Collatz (aka Hailstone) sequence based on the selected starting integer.  
}
\usage{
collatz(x, div=2, mul=3, add= 1, maxKnown=1, maxiter = 1000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
The integer, or \code{bigz} integer to start with. 
}
\item{div}{
The integer to divide by. Default is \code{2} per the original Collatz formula.
}
\item{mul}{
The integer to multiply by. Default is \code{3} per the original Collatz formula.
}
\item{add}{
The integer to add after multiply. Default is \code{1} per the original Collatz formula.
}
\item{maxKnown}{
An integer to use as a "shortcut" if you know that said value converges. This allows the user to avoid repeating previous calculations.  Default value is \code{1}, i.e. no previous knowledge of converging numbers. 
}
  \item{maxiter}{
A "safety switch" to avoid possible lengthy runtimes (when starting with very very large numbers), terminating the function prior to convergence. 
}
}
\details{
The Collatz sequence follows simple rules:
If the current number is even, divide it by two; else if it is odd, multiply it by three and add one.   Convergence occurs in  < 200 cycles for initial values < 10 million or so.
Note: a serious Collatz generator would memoize previous successful sequences, thus greatly reducing the calculation time required to test new numbers.  This function is provided "for amusement only."
}
\value{
A vector of \code{bigz} integers representing the sequence, either to convergence or as limited by \code{maxiter}
}
\author{
Carl Witthoft,  \email{carl@witthoft.com} 
}

\examples{
(collatz(20))
# 20 10 5  16 8  4  2 
(collatz(234568))
# [1] 234568 117284 58642  29321  87964  43982  21991  65974  32987  98962
#  49481  148444 74222  37111 
# [15] 111334 55667 167002 83501 250504 125252 62626 31313 93940 46970 23485
# 70456 35228 17614 
# [29] 8807  26422 13211 39634 19817 59452 29726 14863 44590 22295 66886
# 33443 100330 50165 
# [43] 150496 75248 37624 18812 9406  4703  14110 7055  21166 10583 
#31750 15875 47626 23813 
# [57] 71440 35720 17860 8930  4465  13396 6698  3349  10048 5024  
#  2512  1256  628  314  
# [71] 157  472  236  118  59   178  89   268  134  67   202  101  304  152  
# [85] 76   38   19   58   29   88   44   22   11   34   17   52   26   13  
# [99] 40   20   10   5   16   8   4   2  
}