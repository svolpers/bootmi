\name{print.bootmi}
\alias{print.bootmi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
print.bootmi(bootmi)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bootmi}{
%%     ~~Describe \code{bootmi} here~~
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
function (bootmi) 
{
    cat("$formula\n")
    print(bootmi$formula)
    cat("\nOverview of data values\n")
    print(summary(bootmi$data))
    cat("\n$data (head of original data)\n")
    print(head(bootmi$data))
    cat("\n$bootstraps[[1]] (head of first bootstrap sample)\n")
    print(head(bootmi$bootstraps[[1]]))
    cat("\n$bootstraps[[2]] (head of second bootstrap sample)\n")
    print(head(bootmi$bootstraps[[2]]))
    cat("\n$bootstraps[[3]] (head of third bootstrap sample)\n")
    print(head(bootmi$bootstraps[[3]]))
    cat("\n$replics\n", bootmi$replics, "\n")
    cat("\n$imputation\n", bootmi$imputation, "\n")
    cat("\n$seed\n", bootmi$seed, "\n")
    cat("\n$parallel\n", bootmi$parallel, "\n")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
