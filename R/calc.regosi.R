calc.regosi <-
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
  c = (coeff[[x_var]]^2) - (crit_t_val^2) * cov_matrix[ x_var, x_var]
  root_term = (b^2) - 4 * a * c

  #return
  obj = list( a=a, b=b, c=c, root_term=root_term, m_var=m_var, x_var=x_var)
  class(obj) = "regosi"
  return(obj)

}
