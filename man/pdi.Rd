\name{pdi}
\alias{pdi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate Perfect Digital Invariant ~~
}
\description{
This function calculates PDI, defined as the sum of a number's digits raised to a given power.   
}
\usage{
pdi(x, pwr = 2, base = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
The \code{bigz} integer to start with. 
}
\item{pwr}{
The power to which digits are raised. Default is 2.
}
\item{base}{
The base of the source integer. Note that calculations within this function are performed in base 10.  Default is 10.
}

}
\details{
This is a helper function for \code{dontWorry}.  Use at your own risk.
}
\value{
The PDI of the input value and base, as a \code{bigz} integer. 
}
\author{
Carl Witthoft,  \email{carl@witthoft.com} 
}
