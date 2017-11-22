\name{bootmi}
\alias{bootmi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
bootmi(formula, data, R, impute = c("none", "norm.predict", "pmm", "mean"), center_mods = FALSE, seed = FALSE, parallel = FALSE, resint = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
%%     ~~Describe \code{formula} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{R}{
%%     ~~Describe \code{R} here~~
}
  \item{impute}{
%%     ~~Describe \code{impute} here~~
}
  \item{center_mods}{
%%     ~~Describe \code{center_mods} here~~
}
  \item{seed}{
%%     ~~Describe \code{seed} here~~
}
  \item{parallel}{
%%     ~~Describe \code{parallel} here~~
}
  \item{resint}{
%%     ~~Describe \code{resint} here~~
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
function (formula, data, R, impute = c("none", "norm.predict", 
    "pmm", "mean"), center_mods = FALSE, seed = FALSE, parallel = FALSE, 
    resint = FALSE) 
{
    UseMethod("bootmi")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
