\name{benprob}
\alias{benprob}
\title{
Generate random numbers based on the Benford distribution
}
\description{
This function produces numbers whose distribution is based on  Benford's Law of the occurrence of the values 1 through 9 in the first digit of numbers.  
}
\usage{
benprob(numsamp = 100, numbase = 10)
}

\arguments{
\item{numsamp}{
How many values to generate.
}
\item{numbase}{
Specify the base system (binary, octal, decimal, or whatever is desired) in which to apply the Benford distribution.  The default is "10," i.e. decimal. 
}

}

\details{
"Benford's Law," \url{https://en.wikipedia.org/wiki/Benford\%27s_law} can be used to assess the "true" randomness of demographic data. Probably its most well-known use has been to detect fraudulent patterns in voting and investment returns claimed by various fund operators.  The probability function is \code{prob(d) = log(d+1) - log(d)}, where \code{d} can take on the values 1:(log_base_in_use -1) .The data generated with this function can be used to calculate various statistics such as variance, skew, etc., which can then be compared with the real-world sample set being analyzed.
}

\value{
A vector of random values.
}
\references{
\url{https://en.wikipedia.org/wiki/Benford\%27s_law}
\url{https://projecteuclid.org/euclid.ss/1177009869/}
}
\author{
Carl Witthoft,  \email{carl@witthoft.com} 
}

\examples{
samps <- benprob(1000)
 sd(samps)
 hist(samps)
}

