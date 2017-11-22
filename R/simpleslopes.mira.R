simpleslopes.mira <-
function( object, x_var, m_var, ci=95, mod_values_type=c("sd","val"), mod_values=c(-1,0,1), centered=FALSE) {
  
  # get dependend variable
  modelt = terms( object$analyses[[1]])
  y_var = as.character( modelt[[2L]])
  # get regression coefficients
  coeff = mice::pool( object)$qbar
  # get mean covariance matrix of parameter estimates (ACOV-matrix)
  vcovs = lapply( object$analyses, vcov)
  cov_matrix = Reduce("+", vcovs) / length(vcovs)
  # get mean imputed data used in regression
  dats = lapply( object$analyses, model.matrix)
  dat = Reduce("+", dats) / length(dats)

  # dflvl1 = nrow(lm_object@frame) - ncol(lm_object@frame) - 1

  # stop("noerro#r")
  slopes = calc.simpleslopes( coeff, dat, cov_matrix, x_var, m_var, y_var, ci, mod_values_type, mod_values, centered)
  return( slopes)
}
