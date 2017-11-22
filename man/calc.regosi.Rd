\name{calc.regosi}
\alias{calc.regosi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
calc.regosi(coeff, dat, cov_matrix, x_var, m_var, ci = 95)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{coeff}{
%%     ~~Describe \code{coeff} here~~
}
  \item{dat}{
%%     ~~Describe \code{dat} here~~
}
  \item{cov_matrix}{
%%     ~~Describe \code{cov_matrix} here~~
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
function (coeff, dat, cov_matrix, x_var, m_var, ci = 95) 
{
    ci = as.integer(ci)
    if (!(ci > 0 && ci < 100)) {
        stop("Please enter a confidence interval of ]0;100[. Only natural numbers are allowed.")
    }
    ci = ci/100
    degfreedm = nrow(dat) - length(coeff) - 1
    if (degfreedm < 1) {
        stop("Error: Wrong data or coefficients.")
    }
    xm_var = paste0(x_var, ":", m_var)
    if (!(xm_var \%in\% names(coeff))) {
        xm_var = paste0(m_var, ":", x_var)
    }
    if (!(xm_var \%in\% names(coeff))) {
        xm_var = paste0(x_var, ".RX.", m_var)
        if (!(xm_var \%in\% names(coeff))) {
            xm_var = paste0(m_var, ".RX.", x_var)
        }
    }
    if (!(xm_var \%in\% names(coeff))) {
        xm_var = paste0(x_var, ".XX.", m_var)
        if (!(xm_var \%in\% names(coeff))) {
            xm_var = paste0(m_var, ".XX.", x_var)
        }
    }
    crit_t_val = qt((1 - (1 - (ci))/2), df = degfreedm)
    a = (coeff[[xm_var]]^2) - (crit_t_val^2) * cov_matrix[xm_var, 
        xm_var]
    b = 2 * coeff[[x_var]] * coeff[[xm_var]] - (crit_t_val^2) * 
        2 * cov_matrix[x_var, xm_var]
    c = (coeff[[x_var]]^2) - (crit_t_val^2) * cov_matrix[x_var, 
        x_var]
    root_term = (b^2) - 4 * a * c
    obj = list(a = a, b = b, c = c, root_term = root_term, m_var = m_var, 
        x_var = x_var)
    class(obj) = "regosi"
    return(obj)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
