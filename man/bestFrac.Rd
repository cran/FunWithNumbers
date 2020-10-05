\name{bestFrac}
\alias{bestFrac}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate a fraction close to the input value.
}
\description{
Inspired by the well-known approximations to \code{pi}, i.e. 22/7 and 355/113, this function allows the user to find the best-match fraction for any number, within the specified maximum magnitude of the numerator and denominator
}
\usage{
bestFrac(x, intrange)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A character string representing a number to be "converted" to a fraction of nearly equal value. 
}
  \item{intrange}{
If a single value, the function tests all combinations of numerator and denominator between one and \code{intrange} . If two values, the 'testing range' is \code{intrange[1]:intrange[2]}. Otherwise, whatever vector of values is supplied will be used. 
}
}
\details{
For irrationals and the like, the simplest way to generate the input parameter string \code{x} is to use \code{\link{sprintf}} with as many digits to the right of the decimal point as desired. 
The returned values are in reduced form, i.e. the numerator and denominator are relatively prime. 
}
\value{

\item{bestmatch }{
The numerator and denominator of the best-matching fraction
}
\item{goodmatch }{
An N-by-2 array of the progressively better matches found (numerators and denominators in the columns)
}
\item{matcherr }{
A vector of the  differences between the 'matcherr' fractions and the input value. This is limited in precision to the machine limit for doubles (floats). 
}
%% ...
}

\author{
Carl Witthoft,  \email{carl@witthoft.com} 
}


\examples{
gpi <- sprintf("\%1.30f", pi) 
bestFrac(gpi, 100)
# $bestmatch
# [1] 22  7
# $goodmatch
#           [,1] [,2]
# goodmatch    0    0
#              1    1
#              2    1
#              3    1
#             13    4
#             16    5
#             19    6
#             22    7
# $matcherr
# [1] 1.000000e+02 6.816901e-01 3.633802e-01 4.507034e-02 3.450713e-02
#     1.859164e-02 7.981306e-03 4.024994e-04
bestFrac(gpi, 100:400)
# $bestmatch
# [1] 355 113
# $goodmatch
#           [,1] [,2]
# goodmatch    0    0
#            100   31
#            100   32
#            101   32
#            104   33
#            107   34
#            110   35  # notice this is 22/7 
#            179   57
#            201   64
#            223   71
#            245   78
#            267   85
#            289   92
#            311   99
#            333  106
#            355  113
# $matcherr
#  [1] 1.000000e+02 2.680608e-02 5.281606e-03 4.665578e-03 3.158429e-03
#      1.739936e-03 4.024994e-04
#  [8] 3.952697e-04 3.080137e-04 2.379631e-04 1.804857e-04 1.324752e-04 
#      9.177057e-05 5.682219e-05
# [15] 2.648963e-05 8.491368e-08)
}
