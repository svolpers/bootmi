\name{bootmi.default}
\alias{bootmi.default}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
bootmi.default(formula, data, R, impute = c("none", "norm.predict", "pmm", "mean"), center_mods = FALSE, seed = FALSE, parallel = FALSE, resint = FALSE)
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
    impute = match.arg(impute)
    if (!is.data.frame(data)) {
        stop("Data must be a data.frame")
    }
    R = as.integer(R)
    if (seed != FALSE) {
        seed = as.integer(seed)
        set.seed(seed)
    }
    bootstraps = boot_samples(data, R)
    if (impute != "none" || center_mods != FALSE) {
        res = resimpcen(frmla = formula, dat = data, res_int = resint, 
            imputation = impute, center_mods = center_mods, bootstraps = FALSE)
        no_cores = parallel::detectCores() - 1
        if ((parallel == TRUE) && (no_cores > 1)) {
            cl = parallel::makeCluster(no_cores)
            invisible(parallel::clusterEvalQ(cl, {
                library(mice)
                library(bootmi)
            }))
            parallel::clusterExport(cl = cl, varlist = c("formula", 
                "impute", "resint", "center_mods"), envir = environment())
            parallel::clusterSetRNGStream(cl, seed)
            bootstraps = parallel::parLapply(cl, bootstraps, 
                function(x) {
                  resimpcen(frmla = formula, dat = x, res_int = resint, 
                    imputation = impute, center_mods = center_mods, 
                    bootstraps = TRUE)
                })
            parallel::stopCluster(cl)
        }
        else {
            set.seed(seed)
            bootstraps = lapply(bootstraps, function(x) {
                resimpcen(frmla = formula, dat = x, res_int = resint, 
                  imputation = impute, center_mods = center_mods, 
                  bootstraps = TRUE)
            })
        }
        formula = res$formula
        data = res$data
    }
    if (resint == TRUE) 
        center_mods = TRUE
    object <- list(formula = formula, data = data, bootstraps = bootstraps, 
        replics = R, imputation = impute, seed = seed, parallel = parallel, 
        center_mods = center_mods)
    class(object) = "bootmi"
    return(object)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
