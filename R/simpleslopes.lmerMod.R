simpleslopes.lmerMod <-
function( object, x_var, m_var, ci=95, mod_values_type=c("sd","val"), mod_values=c(-1,0,1), centered=FALSE) {
  # get dependend variable
  modelt = terms( object)
  y_var = as.character( modelt[[2L]])
  # get data used in regression
  dat = model.matrix( object)
  # get regression coefficients
  coeff = coefficients( object)[[1]]
  coeff = colMeans( coeff)
  # get covariance matrix of parameter estimates (ACOV-matrix)
  cov_matrix = vcov( object)

  # dflvl1 = nrow(lm_object@frame) - ncol(lm_object@frame) - 1

  slopes = calc.simpleslopes( coeff, dat, cov_matrix, x_var, m_var, y_var, ci, mod_values_type, mod_values, centered)
  return( slopes)
}
