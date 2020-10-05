\name{sptable}
\alias{sptable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate the number of unique values in the cross-table of sums and products for the input set of numbers
}
\description{
This function tests the proposition that the sum of all unique values in the cross-table of sums and products for a set of N input values is "close" to N^2 .
}
\usage{
sptable(x)
}

\arguments{
  \item{x}{
A vector of integer values.
}
}

\value{
.
\item{uniqsum}{
vector of the unique values of the outer sum \code{outer(x,x,'+')}
}.
\item{uniqprod}{
vector of the unique values of the outer product \code{outer(x,x)}
}.
\item{spratio}{
The ratio  \code{uniqsum/uniquprod}
}
\item{exponentOfN}{
The (numeric) solution to N^(exponentOfN) = uniqsum+uniqprod. If Erdos is right, this will always be "close" to 2. 
}
}
\references{
This conjecture is discussed in \url{https://www.quantamagazine.org/the-sum-product-problem-shows-how-addition-and-multiplication-constrain-each-other-20190206/}
}
\author{
Carl Witthoft,  \email{carl@witthoft.com} 
%%  ~~who you are~~
}

\examples{
(sptable(1:10))
# $uniqsum
# [1] 19
# $uniqprod
# [1] 42
# $spratio
# [1] 0.452381
# $exponentOfN
# [1] 1.78533
set.seed(42)
sptable(sample(1:100,20,rep=FALSE))
# $uniqsum
# [1] 123
# $uniqprod
# [1] 202
# $spratio
# [1] 0.6089109
# $exponentOfN
# [1] 1.930688
}

