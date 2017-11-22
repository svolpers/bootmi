\name{plot.simpleslopes}
\alias{plot.simpleslopes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.simpleslopes(slopes)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{slopes}{
%%     ~~Describe \code{slopes} here~~
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
function (slopes) 
{
    mod_vals = round(slopes$info$Values_of_Moderator, 2)
    x = slopes$plot$x
    y = slopes$plot$y
    title = paste0(slopes$info$Y, " ~ ", slopes$info$X, " * ", 
        slopes$info$M, "(+ ivs)")
    x11()
    par(mar = c(5, 4, 4, 9))
    plot(x, y, main = title, xlab = slopes$info$X, ylab = slopes$info$Y, 
        ylim = c((min(y)), (max(y))), xlim = c((min(x)), (max(x))), 
        bty = "L")
    i = c(1:length(x))[c(T, F)]
    segments(x[i], y[i], x[i + 1], y[i + 1], lty = 1:length(i))
    legend(x = "topright", legend = mod_vals, inset = c(-0.4, 
        0), lty = 1:length(i), cex = 0.8, ncol = 1, title = "Moderator Values", 
        xpd = TRUE)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
