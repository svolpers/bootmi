\name{resimpcen}
\alias{resimpcen}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
resimpcen(frmla, data, res_int, imputation, center_mods, bootstraps = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{frmla}{
%%     ~~Describe \code{frmla} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{res_int}{
%%     ~~Describe \code{res_int} here~~
}
  \item{imputation}{
%%     ~~Describe \code{imputation} here~~
}
  \item{center_mods}{
%%     ~~Describe \code{center_mods} here~~
}
  \item{bootstraps}{
%%     ~~Describe \code{bootstraps} here~~
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
function (frmla, data, res_int, imputation, center_mods, bootstraps = FALSE) 
{
    if (res_int == TRUE) {
        res = add_residual_interactions(frmla, data)
        frmla = res$formula
        data = res$data
    }
    else {
        int = add_interactions(frmla, data)
        frmla = int$formula
        data = int$data
    }
    ids = as.numeric(rownames(data))
    if ((imputation != "none") && ((sum(is.na(data))) > 0)) {
        id = as.numeric(rownames(data))
        mids_data = mice::mice(data, method = imputation, m = 1, 
            print = FALSE)
        data = mice::complete(mids_data, action = "long", include = FALSE)
        data = data[, -c(1, 2)]
        rownames(data) = id
    }
    if (center_mods == TRUE) {
        terms = attr(terms(as.formula(frmla)), "term.labels")
        centered_vars = sapply(terms, function(x) {
            if (grepl(".RX.", x, fixed = TRUE) || grepl(".XX.", 
                x, fixed = TRUE) || grepl(":", x, fixed = TRUE)) {
                return(x)
            }
        })
        print(unlist(centered_vars))
        data = centering(data, unlist(centered_vars))
    }
    rownames(data) = ids
    if (bootstraps) {
        return(data)
    }
    else {
        return(list(formula = frmla, data = data))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
