\name{simpleslopes.lm}
\alias{simpleslopes.lm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
simpleslopes.lm(object, x_var, m_var, ci, mod_values_type, mod_values, centered)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
%%     ~~Describe \code{object} here~~
}
  \item{x_var}{
%%     ~~Describe \code{x_var} here~~
}
  \item{m_var}{
%%     ~~Describe \code{m_var} here~~
}
  \item{ci}{
%%     ~~Describe \code{ci} here~~
}
  \item{mod_values_type}{
%%     ~~Describe \code{mod_values_type} here~~
}
  \item{mod_values}{
%%     ~~Describe \code{mod_values} here~~
}
  \item{centered}{
%%     ~~Describe \code{centered} here~~
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
function (object, x_var, m_var, ci, mod_values_type, mod_values, 
    centered) 
{
    modelt = terms(object)
    y_var = as.character(modelt[[2L]])
    dat = model.matrix(object)
    coeff = coefficients(object)
    cov_matrix = vcov(object)
    slopes = calc.simpleslopes(coeff, dat, cov_matrix, x_var, 
        m_var, y_var, ci, mod_values_type, mod_values, centered)
    return(slopes)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
