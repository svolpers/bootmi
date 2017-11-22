\name{add_residual_interactions}
\alias{add_residual_interactions}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
add_residual_interactions(formula, data)
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
    data$lmresid = as.numeric(rownames(data))
    model = lm(formula, data = data)
    dat = model.matrix(model)
    if (colnames(dat)[1] == "(Intercept)") 
        dat = dat[, -1]
    dat_col_names = colnames(dat)
    for (i in seq_along(dat_col_names)) {
        if (grepl(":", dat_col_names[i], fixed = TRUE)) {
            newname = gsub(":", ".RX.", dat_col_names[i], fixed = TRUE)
            res = build_residual(dat_col_names[i], data = data, 
                new_varname = newname)
            if (!is.null(res)) {
                res$lmresid = as.numeric(rownames(res))
                data = merge(data, res, by = "lmresid", all.x = TRUE)
                rownames(data) = data$lmresid
            }
            else {
                stop("Error creating residual interactions: Unable to calculate!")
            }
        }
    }
    modelt = terms(model)
    ivs = gsub(":", ".RX.", dat_col_names, fixed = TRUE)
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
