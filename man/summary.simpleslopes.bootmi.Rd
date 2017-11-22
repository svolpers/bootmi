\name{summary.simpleslopes.bootmi}
\alias{summary.simpleslopes.bootmi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
summary.simpleslopes.bootmi(bootmi.slopes, ci_type = c("basic", "norm", "stud", "perc", "bca", "all"), ci = 95)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bootmi.slopes}{
%%     ~~Describe \code{bootmi.slopes} here~~
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
function (bootmi.slopes, ci_type = c("basic", "norm", "stud", 
    "perc", "bca", "all"), ci = 95) 
{
    ci = as.integer(ci)
    if (!(ci > 0 && ci < 100)) {
        stop("Please enter a confidence interval of ]0;100[. Only natural numbers are allowed.")
    }
    bootmi.slopes$ci = ci/100
    bootmi.slopes$ci_type = match.arg(ci_type)
    output = bootmi_results(bootmi.slopes)
    colnames(output) <- c("Slope Original", "Bias", "Slope Bias Corrected", 
        "BLLCI", "BULCI")
    cat("\nAnalysis of the simple slopes\n")
    cat("Dependent variable:", bootmi.slopes$info$Y, "\n")
    cat("Independent variable:", bootmi.slopes$info$X, "\n")
    cat("Moderator:", bootmi.slopes$info$M, "\n\n")
    cat("Coefficients at values of the moderator:\n")
    print(output)
    cat("\n", (bootmi.slopes$ci * 100), "\% ", bootmi.slopes$ci_type, 
        " bootstrapped confidence intervals calculated with ", 
        bootmi.slopes$replics, " bootstrap samples.\n", "BLLCI = Bootstrap Lower Level Confidence Interval / BULCI = Bootstrap Upper Level Confidence Interval\n", 
        sep = "")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
