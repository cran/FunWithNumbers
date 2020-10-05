\name{vaneck}
\alias{vaneck}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate a sequence 'invented' by Jan Ritsema Van Eck
}
\description{
This function generates an interesting (to the author, at least) sequence listed as number A181391 in the \url{http://oeis.org/}. See Details for a full description. 
}
\usage{
vaneck(howlong = 100, ve = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{howlong}{
How many terms to generate.
}
  \item{ve}{
Optional argument. Enter a previously generated ("VanEck") sequence here as a numeric vector, or a single integer to use as an initiator.
}
  \item{\dots}{
reserved for possible future use. 
}
}
\details{
The rule here is that you start with 0, and whenever you get to a number you have not seen before, the following term is a 0. But if the number k has appeared previously in the sequence, then you count the number of terms since the last appearance of k, and that number is the following term. In more detail:

Term 1: The first term is 0 by definition.
Term 2: Since we havent seen 0 before, the second term is 0.
Term 3: Since we have seen a 0 before, one step back, the third term is 1
Term 4: Since we havent seen a 1 before, the fourth term is 0
Term 5: Since we have seen a 0 before, two steps back, the fifth term is 2.
And so on.
As of this release of this R-package, how fast max(sequence) grows, and  whether every number eventually appears,  are open questions. The latest investigations and theorems related to this sequence can be found at \url{http://oeis.org/A181391/}
}
\value{
\item{ve}{
The vector (ve for "VanEck") of the sequence values calculated
}
\item{uniqs}{
a vector of the unique values in \code{ve}
}
}
\references{
\url{http://oeis.org/A181391/}
%% ~put references to the literature/web site here ~
}
\author{
Carl Witthoft,  \email{carl@witthoft.com} 
%%  ~~who you are~~
}

\examples{
(vaneck(20))
# $ve
#  [1] 0 0 1 0 2 0 2 2 1 6 0 5 0 2 6 5 4 0 5 3 0
# $uniqs
# [1] 0 1 2 6 5 4 3

}

