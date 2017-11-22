simpleslopes.lm <-
function( object, x_var, m_var, ci, mod_values_type, mod_values, centered) {

  # get dependend variable
  modelt = terms( object)
  y_var = as.character( modelt[[2L]])
  # get data used in regression
  dat = model.matrix( object)
  # get regression coefficients
  coeff = coefficients( object)
  # get covariance matrix of parameter estimates (ACOV-matrix)
  cov_matrix = vcov( object)

  slopes = calc.simpleslopes( coeff, dat, cov_matrix, x_var, m_var, y_var, ci, mod_values_type, mod_values, centered)
  return( slopes)
}
