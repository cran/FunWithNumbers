\name{morris}
\alias{morris}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate the Morris sequence
}
\description{
The Morris sequence, aka "Look-Say," is an old puzzler sequence. 
}
\usage{
morris(x, reps)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Either a starting value from 1 to 9, or a numeric vector containing a Morris sequence previously generated.  
}
  \item{reps}{
Specifies the number of new Morris sequences to generate, starting with the input \code{x}
}
}
\details{
The Morris sequence is built by taking the verbal description of a number sequence and converting every number or named numeral to a number in order.  Typically, starting with the integer 1, the spoken description is "One 1," so the next sequence is c(1,1).  Read that out loud as "Two ones", so the next sequence is c(2,1) and so on. 
}
\value{
A list variable containing all the sequences generated as numeric vectors. ...
}

\author{
Carl Witthoft,  \email{carl@witthoft.com} 
%%  ~~who you are~~
}

