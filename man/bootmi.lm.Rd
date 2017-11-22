\name{bootmi.lm}
\alias{bootmi.lm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
bootmi.lm(bootmi)
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
    if (class(bootmi) != "bootmi") {
        stop("Object of class \"bootmi\" needed.")
    }
    frml = as.character(bootmi$formula)
    val_dep_var = matrix(bootmi$data[[frml[[2]]]], ncol = 1)
    rownames(val_dep_var) = rownames(bootmi$data)
    colnames(val_dep_var) = frml[[2]]
    frml = paste0(frml[[2]], frml[[1]], frml[[3]])
    lm_org_data = lm(frml, bootmi$data)
    bootmi$data = merge(as.data.frame(model.matrix(lm_org_data)), 
        as.data.frame(val_dep_var), by = 0, all.x = TRUE)
    rownames(bootmi$data) = as.numeric(bootmi$data[["Row.names"]])
    no_cores <- parallel::detectCores() - 1
    if ((bootmi$parallel == TRUE) && (no_cores > 1)) {
        cl <- parallel::makeCluster(no_cores)
        parallel::clusterExport(cl = cl, varlist = c("frml"), 
            envir = environment())
        boot_lm <- parallel::parLapply(cl, bootmi$bootstraps, 
            function(x) lm(frml, x)$coef)
        parallel::stopCluster(cl)
    }
    else {
        boot_lm <- lapply(bootmi$bootstraps, function(x) lm(frml, 
            x)$coef)
    }
    bootstrap_coeff = do.call(rbind, boot_lm)
    object = list(formula = bootmi$formula, original = lm_org_data, 
        bootstraps = bootstrap_coeff, data = bootmi$data[, c(-1, 
            -2)], replics = bootmi$replics, center_mods = bootmi$center_mods, 
        seed = bootmi$seed)
    class(object) = "bootmi.lm"
    return(object)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
