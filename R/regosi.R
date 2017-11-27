#' @title Calculates regions of significance for moderation effects
#' @description
#' The method is an implementation of regions of significance  
#' as proposed by Bauer and Curran (2005). The default takes 
#' parameters needed to calculate regions of significance. For objects 
#' of class "lm", "lmerMod" and "mira" exist helper functions 
#' that extract parameters "coeff", "dat", "cov_matrix" from the object.
#' @param object Object containing $coeff, $dat, $cov_matrix
#' @param x_var Name of the independend variable
#' @param m_var Name of the moderating variable
#' @param ci Confidence interval, default 95 
#' @return object of class "regosi" including
#' \itemize{
#'   \item $a, Indicator for Interval
#'   \item $b, Value of slope
#'   \item $root_term, Indicator for significant regions
#'   \item $m_var, Name of the moderating variable
#'   \item $x_var, Name of the independend variable
#' }
#' @name regosi
#' @author Stephan Volpers \email{stephan.volpers@@plixed.de}
#' @references Bauer, Daniel J.; Curran, Patrick J. (2005): 
#' Probing Interactions in Fixed and Multilevel Regression: 
#' Inferential and Graphical Techniques. In: Multivariate Behavioral Research
#' 40 (3), S. 373-400.
#' @export
regosi <- function( object, x_var, m_var, ci = 95) {
    UseMethod("regosi")
}

#' @rdname regosi
#' @export
regosi.default <- function( object, x_var, m_var, ci=95) {

  # set confidence interval value
  ci = as.integer(ci)
  if(!(ci > 0 && ci < 100)) {
      stop("Please enter a confidence interval of ]0;100[. Only natural numbers are allowed.")
  }
  ci = ci/100

  degfreedm = nrow( object$dat) - length( object$coeff) - 1
  if(degfreedm < 1) {
      stop("Error: Wrong data or coefficients.")
  }

  # identify moderating variable
  # CASE: non residual moderator
  xm_var = paste0(x_var,":",m_var)
  if(!(xm_var %in% names( object$coeff))) {
    xm_var = paste0(m_var,":",x_var)
  }
  # CASE: residual moderator
  if(!(xm_var %in% names( object$coeff))) {
    xm_var = paste0(x_var,".RX.",m_var)
    if(!(xm_var %in% names( object$coeff))) {
      xm_var = paste0(m_var,".RX.",x_var)
    }
  }
  # CASE: centered moderator
  if(!(xm_var %in% names( object$coeff))) {
    xm_var = paste0(x_var,".XX.",m_var)
    if(!(xm_var %in% names( object$coeff))) {
      xm_var = paste0(m_var,".XX.",x_var)
    }
  }

  # compute critical t_val
  crit_t_val = qt((1-(1-(ci))/2), df=degfreedm) # two sided
  
  # Bauer and Curran (2005)
  a = ( object$coeff[[xm_var]]^2) - (crit_t_val^2) * object$cov_matrix[ xm_var, xm_var]
  b = 2 * object$coeff[[x_var]] * object$coeff[[xm_var]] - (crit_t_val^2) * 2 * object$cov_matrix[ x_var, xm_var]
  root_term = (b^2) - 4 * a * ( object$coeff[[x_var]]^2) - (crit_t_val^2) * object$cov_matrix[ x_var, x_var]

  #return
  obj = list( a=a, b=b, root_term=root_term, m_var=m_var, x_var=x_var)
  class(obj) = "regosi"
  return(obj)
}

#' @section Helper functions exist:
#' Uses object of classes lm, lmerMod or mira 
#' and extracts coefficients, data, and variance covariance matrix
#' @rdname regosi
#' @export
regosi.lmerMod <- function( object, x_var, m_var, ci) {
  # get data used in regression
  data_set = model.matrix( object)
  # get regression coefficients
  coeff = colMeans( coefficients( object)[[1]])
  # get covariance matrix of parameter estimates (ACOV-matrix)
  cov_matrix = vcov( object)
  # merge to obj
  obj = list( coeff= coeff, cov_matrix= cov_matrix, dat= data_set)
  #calculate regions of significance
  regosi = regosi.default( obj, x_var, m_var, ci)
  return( regosi)
}

#' @rdname regosi
#' @export
regosi.lm <- function( object, x_var, m_var, ci) {
  # get data used in regression
  data_set = model.matrix( object)
  # get regression coefficients
  coeff = coefficients(object)
  # get covariance matrix of parameter estimates (ACOV-matrix)
  cov_matrix = vcov(object)
  # merge to obj
  obj = list( coeff= coeff, cov_matrix= cov_matrix, dat= data_set)
  #calculate regions of significance
  regosi = regosi.default( obj, x_var, m_var, ci)
  return( regosi)
}

#' @rdname regosi
#' @export
regosi.mira <- function( object, x_var, m_var, ci) {
  # get mean imputed data used in regression
  dats = lapply( object$analyses, model.matrix)
  data_set = Reduce("+", dats) / length(dats)
  # get regression coefficients
  coeff = mice::pool( object)$qbar
  # get covariance matrix of parameter estimates (ACOV-matrix)
  vcovs = lapply( object$analyses, vcov)
  cov_matrix = Reduce("+", vcovs) / length(vcovs)
  # merge to obj
  obj = list( coeff= coeff, cov_matrix= cov_matrix, dat= data_set)
  #calculate regions of significance
  regosi = regosi.default( obj, x_var, m_var, ci)
  return( regosi)
}


#' @rdname regosi
#' @details
#' CAUTION: 
#' For calculation of regions of significance, the ACOV-matrix
#' is used, instead of bootstrapped confidence intervals.
#' Hence, this method omits bootstraps from bootmi.lm and calculates 
#' regions of significance for imputed orginal model only. 
#' I am searching for a solution to the problem. 
#' Any suggestions are welcome:
#' \href{https://github.com/svolpers/bootmi/issues}{Suggest Solution}
#' For more reliable regions, use \code{\link{simslop}} with dicrete
#' values of the moderating variable, which reflect the whole range 
#' of the variable. 
#' @export
regosi.bootmi.lm <- function( object, x_var, m_var, ci) {
  # get data used in regression
  data_set = model.matrix( object$original)
  # get regression coefficients
  coeff = coefficients(object$original)
  # get covariance matrix of parameter estimates (ACOV-matrix)
  cov_matrix = vcov(object$original)
  # merge to obj
  obj = list( coeff= coeff, cov_matrix= cov_matrix, dat= data_set)
  # calculate regions of significance for original data
  regosi = regosi.default( obj, x_var, m_var, ci)
  return( object)
}