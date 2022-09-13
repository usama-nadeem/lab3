\name{euclidean}
\alias{euclidean}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{euclidean}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
euclidean(Num1, Num2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Num1}{
%%     ~~Describe \code{Num1} here~~
}
  \item{Num2}{
%%     ~~Describe \code{Num2} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
https://en.wikipedia.org/wiki/Euclidean_algorithm
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (Num1, Num2) 
{
    if (!is.numeric(Num1) || !is.numeric(Num2)) {
        stop("Input is non-numeric")
    }
    else if (Num1 == 0 && Num2 == 0) {
        return(0)
    }
    else if (Num1 != 0 && Num2 == 0) {
        return(Num1)
    }
    else if (Num1 == 0 && Num2 != 0) {
        return(Num2)
    }
    else if (Num1 != 0 && Num2 != 0) {
        while (Num1 != Num2) {
            if (Num1 > Num2) {
                Num1 <- Num1 - Num2
            }
            else {
                Num2 <- Num2 - Num1
            }
        }
    }
    return(Num1)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
