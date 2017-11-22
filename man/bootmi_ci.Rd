\name{bootmi_ci}
\alias{bootmi_ci}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
bootmi_ci(boot_object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{boot_object}{
%%     ~~Describe \code{boot_object} here~~
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
function (boot_object) 
{
    if ((class(boot_object) != "simpleslopes.bootmi") && (class(boot_object) != 
        "bootmi.lm")) {
        stop("Object of class bootmi.lm or simpleslopes.bootmi required.")
    }
    sample_size = nrow(boot_object$data)
    set.seed(boot_object$seed)
    boot_ci = list(t0 = boot_object$original$coef, t = boot_object$bootstraps, 
        R = boot_object$replics, data = boot_object$data, seed = .Random.seed, 
        sim = "ordinary", call = as.call(quote(boot(data = boot_object$data, 
            statistic = bootmi, R = boot_object$replics, formula = boot_object$formula))), 
        stype = "i", strata = sapply((1:sample_size), function(x) 1), 
        weights = sapply((1:sample_size), function(x) 1/sample_size))
    return(boot_ci)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
