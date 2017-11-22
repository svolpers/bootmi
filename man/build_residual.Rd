\name{build_residual}
\alias{build_residual}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
build_residual(variable, data, new_varname)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{variable}{
%%     ~~Describe \code{variable} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{new_varname}{
%%     ~~Describe \code{new_varname} here~~
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
function (variable, data, new_varname) 
{
    nterms = length(strsplit(variable, ":")[[1]])
    if (nterms == 1) 
        return(NULL)
    y = paste0("I(", gsub(":", "*", variable, fixed = TRUE), 
        ")")
    xs = paste0("(", gsub(":", "+", variable, fixed = TRUE), 
        ")")
    if (nterms > 2) 
        xs = paste0(xs, "^", nterms - 1)
    reg_form = as.formula(paste0(y, "~", xs))
    rcterms = attr(terms(reg_form), "term.labels")
    rcterms = gsub(":", ".RX.", rcterms, fixed = TRUE)
    finalFormula = paste0(y, "~", paste(rcterms, collapse = "+"))
    aReg = lm(finalFormula, data)
    res = data.frame(resid(aReg), stringsAsFactors = FALSE)
    colnames(res) = c(new_varname)
    return(res)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
