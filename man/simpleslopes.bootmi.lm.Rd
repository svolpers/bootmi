\name{simpleslopes.bootmi.lm}
\alias{simpleslopes.bootmi.lm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
simpleslopes.bootmi.lm(bootmi_lm_obj, x_var, m_var, mod_values_type = "sd", mod_values = c(-1, 0, 1))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bootmi_lm_obj}{
%%     ~~Describe \code{bootmi_lm_obj} here~~
}
  \item{x_var}{
%%     ~~Describe \code{x_var} here~~
}
  \item{m_var}{
%%     ~~Describe \code{m_var} here~~
}
  \item{mod_values_type}{
%%     ~~Describe \code{mod_values_type} here~~
}
  \item{mod_values}{
%%     ~~Describe \code{mod_values} here~~
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
function (bootmi_lm_obj, x_var, m_var, mod_values_type = "sd", 
    mod_values = c(-1, 0, 1)) 
{
    if (class(bootmi_lm_obj) != "bootmi.lm") {
        stop("Object of class \"bootmi.lm\" needed.")
    }
    si_sl = simpleslopes(lm_object = bootmi_lm_obj$original, 
        x_var = x_var, m_var = m_var, mod_values_type = mod_values_type, 
        mod_values = mod_values, centered = bootmi_lm_obj$center_mods)
    coeff = extract_coeff(si_sl)
    si_sl$original = list(slopes = si_sl$original, coef = coeff)
    bscoef = matrix(0, nrow = bootmi_lm_obj$replics, ncol = length(mod_values))
    for (i in 1:length(mod_values)) {
        bootstrcoeff = (bootmi_lm_obj$bootstraps[, si_sl$info$X] + 
            bootmi_lm_obj$bootstraps[, si_sl$info$XM] * si_sl$original$slopes[[i]][["m_val_data"]])
        bscoef[, i] = bootstrcoeff
    }
    colnames(bscoef) = mod_values
    object = append(si_sl, list(bootstraps = bscoef, data = bootmi_lm_obj$data, 
        formula = bootmi_lm_obj$formula, replics = bootmi_lm_obj$replics))
    class(object) = "simpleslopes.bootmi"
    return(object)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
