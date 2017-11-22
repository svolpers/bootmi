\name{summary.bootmi.lm}
\alias{summary.bootmi.lm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
summary.bootmi.lm(bootmi.lm, ci_type = c("basic", "norm", "stud", "perc", "bca", "all"), ci = 95)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bootmi.lm}{
%%     ~~Describe \code{bootmi.lm} here~~
}
  \item{ci_type}{
%%     ~~Describe \code{ci_type} here~~
}
  \item{ci}{
%%     ~~Describe \code{ci} here~~
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
function (bootmi.lm, ci_type = c("basic", "norm", "stud", "perc", 
    "bca", "all"), ci = 95) 
{
    ci = as.integer(ci)
    if (!(ci > 0 && ci < 100)) {
        stop("Please enter a confidence interval of ]0;100[. Only natural numbers are allowed.")
    }
    bootmi.lm$ci = ci/100
    bootmi.lm$ci_type = match.arg(ci_type)
    summary_model = summary.lm(bootmi.lm$original)
    summary_model$coefficients = bootmi_results(bootmi.lm)
    frml = as.character(bootmi.lm$formula)
    frml = paste0(frml[[2]], frml[[1]], frml[[3]])
    summary_model$call = paste("lm(formula=", frml, ", data= bootmi$data")
    print(summary_model)
    cat((bootmi.lm$ci * 100), "\% ", bootmi.lm$ci_type, " bootstrapped confidence intervals calculated with ", 
        bootmi.lm$replics, " bootstrap samples.\n", sep = "")
    cat("BLLCI = Bootstrap Lower Level Confidence Interval / BULCI = Bootstrap Upper Level Confidence Interval\n")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
