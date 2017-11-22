\name{calc.simpleslopes}
\alias{calc.simpleslopes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
calc.simpleslopes(coeff, dat, cov_matrix, x_var, m_var, y_var, ci = 95, mod_values_type = c("sd", "val"), mod_values = c(-1, 0, 1), centered = FALSE)
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
  \item{y_var}{
%%     ~~Describe \code{y_var} here~~
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
function (coeff, dat, cov_matrix, x_var, m_var, y_var, ci = 95, 
    mod_values_type = c("sd", "val"), mod_values = c(-1, 0, 1), 
    centered = FALSE) 
{
    mod_values_type = match.arg(mod_values_type)
    ci = as.integer(ci)
    if (!(ci > 0 && ci < 100)) {
        stop("Error: Please enter a confidence interval of ]0;100[. Only natural numbers are allowed.")
    }
    user_ci = ci/100
    degfreedm = nrow(dat) - length(coeff) - 1
    if (degfreedm < 1) {
        stop("Error: Wrong data or coefficients.")
    }
    if (centered == TRUE) {
        dat = data.frame(apply(dat, 2, function(x) scale(x, scale = FALSE)))
    }
    if (names(coeff)[1] == "(Intercept)") {
        intrcpt_coeff = coeff[[1]]
    }
    else {
        intrcpt_coeff = 0
    }
    xm_var = paste0(x_var, ":", m_var)
    if (!xm_var \%in\% names(coeff)) {
        xm_var = paste0(m_var, ":", x_var)
    }
    if (!xm_var \%in\% names(coeff)) {
        xm_var = paste0(x_var, ".RX.", m_var)
        if (!xm_var \%in\% names(coeff)) {
            xm_var = paste0(m_var, ".RX.", x_var)
        }
    }
    if (!xm_var \%in\% names(coeff)) {
        xm_var = paste0(x_var, ".XX.", m_var)
        if (!xm_var \%in\% names(coeff)) {
            xm_var = paste0(m_var, ".XX.", x_var)
        }
    }
    if (mod_values_type == "sd") {
        value_x_h = mean(dat[, x_var]) + sd(dat[, x_var])
        value_x_l = mean(dat[, x_var]) - sd(dat[, x_var])
    }
    else {
        value_x_l = quantile(dat[, x_var], probs = c(0.25))
        value_x_h = quantile(dat[, x_var], probs = c(0.75))
    }
    simple_slopes = list()
    x = c()
    y = c()
    for (i in 1:length(mod_values)) {
        if (mod_values_type == "sd" && mod_values[i] != 0) {
            value_m = mean(dat[, m_var]) + (mod_values[i]) * 
                sd(dat[, m_var])
        }
        else {
            value_m = mod_values[i]
        }
        slope = coeff[[x_var]] + coeff[[xm_var]] * value_m
        w1 = eval(cov_matrix[x_var, x_var] + 2 * value_m * cov_matrix[x_var, 
            xm_var] + value_m * value_m * cov_matrix[xm_var, 
            xm_var])
        SE = sqrt(w1)
        t_value = eval(slope/SE)
        p_value = 2 * (1 - pt(abs(t_value), df = degfreedm))
        crit_t_val = qt((1 - (1 - (user_ci))/2), df = degfreedm)
        margin_of_error = crit_t_val * SE
        sisl = c(mod_values[i], value_m, slope, SE, t_value, 
            p_value, eval(slope - margin_of_error), eval(slope + 
                margin_of_error))
        names(sisl) = list("m_val_user", "m_val_data", "slope", 
            "SE", "t_value", "p_value", "LLCI", "ULCI")
        simple_slopes[[i]] = sisl
        y_l = eval(intrcpt_coeff + (coeff[[x_var]] + coeff[[xm_var]] * 
            value_m) * value_x_l + coeff[[m_var]] * value_m)
        y_h = eval(intrcpt_coeff + (coeff[[x_var]] + coeff[[xm_var]] * 
            value_m) * value_x_h + coeff[[m_var]] * value_m)
        x <- c(x, value_x_l, value_x_h)
        y <- c(y, y_l, y_h)
    }
    plotvalues = list(x = x, y = y)
    info = list(y_var, x_var, m_var, xm_var, user_ci, mod_values_type, 
        mod_values)
    names(info) = c("Y", "X", "M", "XM", "Confidence_Interval", 
        "Type_of_moderator_values", "Values_of_Moderator")
    object = list(original = simple_slopes, info = info, plot = plotvalues)
    class(object) = "simpleslopes"
    return(object)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
