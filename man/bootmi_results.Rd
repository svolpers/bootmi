\name{bootmi_results}
\alias{bootmi_results}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
bootmi_results(bootmi.lm)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bootmi.lm}{
%%     ~~Describe \code{bootmi.lm} here~~
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
function (bootmi.lm) 
{
    bias = colMeans(bootmi.lm$bootstraps) - bootmi.lm$original$coef
    bias_corrected_estimate = bootmi.lm$original$coef - bias
    output = matrix(c(bootmi.lm$original$coef, bias, bias_corrected_estimate), 
        nrow = length(bootmi.lm$original$coef))
    boot_ci = bootmi_ci(bootmi.lm)
    boot::boot.ci(boot_ci, conf = bootmi.lm$ci, type = bootmi.lm$ci_type, 
        index = 1)
    cis = c()
    for (i in 1:length(bootmi.lm$original$coef)) {
        val = boot::boot.ci(boot_ci, conf = bootmi.lm$ci, type = bootmi.lm$ci_type, 
            index = i)[[bootmi.lm$ci_type]]
        if (bootmi.lm$ci_type == "norm") 
            cis = c(cis, val[2], val[3])
        else cis = c(cis, val[4], val[5])
    }
    output = cbind(output, matrix(cis, ncol = 2, byrow = TRUE))
    output = apply(output, c(1, 2), function(x) round(x, digits = 4))
    colnames(output) <- c("Estimate Original", "Bias", "Estimate Bias corrected", 
        "BLLCI", "BULCI")
    rownames(output) <- names(bootmi.lm$original$coef)
    return(output)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
