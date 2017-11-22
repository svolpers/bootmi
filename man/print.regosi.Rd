\name{print.regosi}
\alias{print.regosi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
print.regosi(regosi)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{regosi}{
%%     ~~Describe \code{regosi} here~~
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
function (regosi) 
{
    cat("\n")
    if (regosi$root_term < 0) {
        cat("There are no real values of ", regosi$m_var, " that represent regions of significance.", 
            sep = "")
        if (regosi$a > 0) {
            cat("The slope of ", regosi$x_var, " is statistically significant for ALL values of the moderator ", 
                regosi$m_var, ".", sep = "")
        }
        else {
            cat("The slope of ", regosi$x_var, " is NEVER statistically significant.", 
                sep = "")
        }
    }
    else {
        crit_mod_val_high = round(((-regosi$b + sqrt(regosi$root_term))/(2 * 
            regosi$a)), 3)
        crit_mod_val_low = round(((-regosi$b - sqrt(regosi$root_term))/(2 * 
            regosi$a)), 3)
        if (regosi$a > 0) {
            cat("Values of the moderator ", regosi$m_var, " OUTSIDE the interval\n", 
                "]", crit_mod_val_low, ":", crit_mod_val_high, 
                "[\n", "cause the slope of (b1 + b2*", regosi$m_var, 
                ")*", regosi$x_var, " to be statistically significant.", 
                sep = "")
        }
        else {
            cat("Values of the moderator ", regosi$m_var, " INSIDE the interval\n", 
                "[", crit_mod_val_low, ":", crit_mod_val_high, 
                "]\n", "cause the slope of (b1 + b2*", regosi$m_var, 
                ")*", regosi$x_var, " to be statistically significant.", 
                sep = "")
        }
    }
    cat("\n\n")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
