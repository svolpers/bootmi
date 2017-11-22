\name{add_interactions}
\alias{add_interactions}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
add_interactions(formula, data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
%%     ~~Describe \code{formula} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
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
function (formula, data) 
{
    data$id = as.numeric(rownames(data))
    model = lm(formula, data = data)
    dat = model.matrix(model)
    if (colnames(dat)[1] == "(Intercept)") 
        dat = dat[, -1]
    ivs = gsub(":", ".XX.", colnames(dat), fixed = TRUE)
    modelt = terms(model)
    for (i in seq_along(ivs)) {
        if (grepl(".XX.", ivs[i], fixed = TRUE)) {
            x_m_var = strsplit(ivs[i], ".XX.")[[1]]
            data[ivs[i]] = data[x_m_var[1]] * data[x_m_var[2]]
            if (length(x_m_var) > 2) {
                for (j in 3:length(x_m_var)) {
                  data[ivs[i]] = data[ivs[i]] * data[x_m_var[j]]
                }
            }
        }
    }
    depvar = as.character(modelt[[2L]])
    frmla = paste0(ivs, collapse = "+")
    frmla = as.formula(paste0(depvar, "~", frmla))
    final_data = data[c(ivs, depvar)]
    return(list(data = final_data, formula = frmla))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
