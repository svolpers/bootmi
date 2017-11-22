\name{summary.simpleslopes}
\alias{summary.simpleslopes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
summary.simpleslopes(slopes)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{slopes}{
%%     ~~Describe \code{slopes} here~~
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
%% ~put references to the literature/web site here ~
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
function (slopes) 
{
    cat("\nAnalysis of the simple slopes\n")
    cat("Dependent variable:", slopes$info$Y, "\n")
    cat("Independent variable:", slopes$info$X, "\n")
    cat("Moderator:", slopes$info$M, "\n\n")
    cat("coefficients at values of the moderator:\n")
    matr = t(sapply(slopes$original, function(x) {
        return(round(x, 4))
    }))
    rownames(matr) = as.numeric(matr[, 1])
    print(matr[, c(-1, -2)])
    cat("\n", (slopes$info$Confidence_Interval * 100), "\% Confidence Intervals\nLLCI = Lower Level Confidence Interval / ULCI = Upper Level Confidence Interval\n", 
        sep = "")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
