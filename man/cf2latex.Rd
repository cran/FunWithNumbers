\name{cf2latex}
\alias{cf2latex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate readable equations from the output of \code{\link{cfrac}}
}
\description{
This function takes a vector of integers representing the values in a continued fraction and generates readable equations in two forms: inline as a character string, and LaTeX code. 
}
\usage{
cf2latex(vals, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vals}{
A vector of integers (or bigz, mpfr integer values)
}
  \item{\dots}{
Reserved for future upgrades
}
}

\value{

\item{eqn }{The continued fraction as an inline equation}
\item{texeqn }{LaTeX source code for presenting the continued fraction}
\item{texexpr }{Markdown-ish string for use in plotting, typically like \code{text(x,y,TeX(texexpr))} } ...
}

\author{
Carl Witthoft,  \email{carl@witthoft.com} 
}

\seealso{
 \code{\link{cfrac}} to generate continued fractions. 
}
\examples{
355/113 - pi
# small number
 foo <- cfrac(355,113)
#[1]  3  7 16
 bar <- cf2latex(foo)
# $eqn
# [1] "3 + 1/(7 + 1/16)"
# $texeqn  #Paste into your LaTeX source file
# [1] "3 + \frac{1}{7 + \frac{1}{16}}"
# $texexpr  # use in an R plot window
# [1] "$3 + \\frac{1}{7 + \\frac{1}{16}}$"
##not run
# library( latex2exp)
# plot(NA,NA,xlim = c(1,10),ylim=c(1,5),axes=FALSE,xlab='',ylab='')
# text(2,4,TeX(bar$texexpr))
}
