\name{centering}
\alias{centering}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
centering(data_set, centered_vars)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data_set}{
%%     ~~Describe \code{data_set} here~~
}
  \item{centered_vars}{
%%     ~~Describe \code{centered_vars} here~~
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
function (data_set, centered_vars) 
{
    data_set = as.data.frame(data_set)
    y = data.frame(scale(data_set[, centered_vars], center = TRUE, 
        scale = FALSE))
    final_data = merge(data_set[, !(names(data_set) \%in\% centered_vars)], 
        y, by = 0, all.x = TRUE)
    final_data = transform(final_data, Row.names = as.numeric(Row.names))
    final_data = final_data[order(final_data$Row.names), ]
    final_data = final_data[-1]
    return(final_data)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
