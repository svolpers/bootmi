#' @title regosi: Regions of significance
#' @description
#' Calculates regions of significance for moderation effects
#' @details
#' The method is an implementation of regions of significance  
#' as proposed by Bauer and Curran (2005). The default takes 
#' parameters needed to calculate regions of significance. For objects 
#' of class "lm", "lmerMod" and "mira" exist helper functions 
#' that extract parameters "coeff", "dat", "cov_matrix" from the object.
#' @param coeff Coefficients from fitted model, just for default
#' @param dat Data used to fit model, just for default
#' @param cov_matrix Variance-Covariance-Matrix of fitted model, 
#' just for default
#' @param object Object, if availiable but NOT for default 
#' @param x_var Name of the independend variable
#' @param m_var Name of the moderating variable
#' @param ci Confidence interval, default 95 
#' @return object of class "regosi" including
#' \itemize{
#'   \item First Indicator for Interval
#'   \item Second Value of slope
#'   \item Third Indicator for significant regions
#'   \item Fourth Name of the moderating variable
#'   \item Fifth Name of the independend variable
#' }
#' @name regosi
#' @author Stephan Volpers <stephan.volpers@plixed.de>
#' @references Bauer, Daniel J.; Curran, Patrick J. (2005): 
#' Probing Interactions in Fixed and Multilevel Regression: 
#' Inferential and Graphical Techniques. In: Multivariate Behavioral Research
#' 40 (3), S. 373–400. DOI: 10.1207/s15327906mbr4003_5.
#' @export

regosi <-
function( object, x_var, m_var, ci = 95) {
    UseMethod("regosi")
}

#' @rdname regosi
#' @export
regosi.default <-
function( coeff, dat, cov_matrix, x_var, m_var, ci=95) {

  # set confidence interval value
  ci = as.integer(ci)
  if(!(ci > 0 && ci < 100)) {
      stop("Please enter a confidence interval of ]0;100[. Only natural numbers are allowed.")
  }
  ci = ci/100

  degfreedm = nrow(dat) - length(coeff) - 1
  if(degfreedm < 1) {
      stop("Error: Wrong data or coefficients.")
  }

  # identify moderating variable
  # CASE: non residual moderator
  xm_var = paste0(x_var,":",m_var)
  if(!(xm_var %in% names(coeff))) {
    xm_var = paste0(m_var,":",x_var)
  }
  # CASE: residual moderator
  if(!(xm_var %in% names(coeff))) {
    xm_var = paste0(x_var,".RX.",m_var)
    if(!(xm_var %in% names(coeff))) {
      xm_var = paste0(m_var,".RX.",x_var)
    }
  }
  # CASE: centered moderator
  if(!(xm_var %in% names(coeff))) {
    xm_var = paste0(x_var,".XX.",m_var)
    if(!(xm_var %in% names(coeff))) {
      xm_var = paste0(m_var,".XX.",x_var)
    }
  }

  # compute critical t_val
  crit_t_val = qt((1-(1-(ci))/2), df=degfreedm) # two sided
  
  # Bauer and Curran (2005)
  a = (coeff[[xm_var]]^2) - (crit_t_val^2) * cov_matrix[ xm_var, xm_var]
  b = 2 * coeff[[x_var]] * coeff[[xm_var]] - (crit_t_val^2) * 2 * cov_matrix[ x_var, xm_var]
  root_term = (b^2) - 4 * a * (coeff[[x_var]]^2) - (crit_t_val^2) * cov_matrix[ x_var, x_var]

  #return
  obj = list( a=a, b=b, root_term=root_term, m_var=m_var, x_var=x_var)
  class(obj) = "regosi"
  return(obj)
}

#' @section Helper functions exist:
#' Uses object of classes lm, lmerMod or mira 
#' and extracts coefficients, data, and variance covariance matrix
#' @rdname regosi
#' @param object Object
#' @param x_var Name of the independend variable
#' @param m_var Name of the moderating variable
#' @param ci Confidence interval 
#' @export
regosi.lmerMod <-
function( object, x_var, m_var, ci) {

  # get data used in regression
  dat = model.matrix( object)
  # get regression coefficients
  coeff = coefficients( object)[[1]]
  coeff = colMeans( coeff)
  # get covariance matrix of parameter estimates (ACOV-matrix)
  cov_matrix = vcov( object)
  #calculate regions of significance
  regosi = regosi.default( coeff, dat, cov_matrix, x_var, m_var, ci)
  return( regosi)
}

#' @rdname regosi
#' @export
regosi.lm <-
function( object, x_var, m_var, ci) {
  # get data used in regression
  dat = model.matrix( object)
  # get regression coefficients
  coeff = coefficients(object)
  # get covariance matrix of parameter estimates (ACOV-matrix)
  cov_matrix = vcov(object)
  #calculate regions of significance
  regosi = regosi.default( coeff, dat, cov_matrix, x_var, m_var, ci)
  return( regosi)
}

#' @rdname regosi
#' @export
regosi.mira <-
function( object, x_var, m_var, ci) {
  # get mean imputed data used in regression
  dats = lapply( object$analyses, model.matrix)
  dat = Reduce("+", dats) / length(dats)
  # get regression coefficients
  coeff = mice::pool( object)$qbar
  # get covariance matrix of parameter estimates (ACOV-matrix)
  vcovs = lapply( object$analyses, vcov)
  cov_matrix = Reduce("+", vcovs) / length(vcovs)
  #calculate regions of significance
  regosi = regosi.default( coeff, dat, cov_matrix, x_var, m_var, ci)
  return( regosi)
}