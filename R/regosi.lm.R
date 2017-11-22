regosi.lm <-
function( object, x_var, m_var, ci) {
  # get data used in regression
  dat = model.matrix( object)
  # get regression coefficients
  coeff = coefficients(object)
  # get covariance matrix of parameter estimates (ACOV-matrix)
  cov_matrix = vcov(object)
  #calculate regions of significance
  regosi = calc.regosi( coeff, dat, cov_matrix, x_var, m_var, ci)
  return( regosi)
}
