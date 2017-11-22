calc.simpleslopes <-
function( coeff, dat, cov_matrix, x_var, m_var, y_var, ci=95, mod_values_type=c("sd","val"), mod_values=c(-1,0,1), centered=FALSE) {
  
  mod_values_type = match.arg( mod_values_type) 
  
  # set confidence interval value
  ci = as.integer(ci)
  if(!(ci > 0 && ci < 100)) {
      stop("Error: Please enter a confidence interval of ]0;100[. Only natural numbers are allowed.")
  }
  user_ci = ci/100

  degfreedm = nrow(dat) - length(coeff) - 1
  if(degfreedm < 1) {
      stop("Error: Wrong data or coefficients.")
  }
  
  # if moderators are mean-centered, x and m var also have to be mean-centered
  if( centered==TRUE ) {
    dat = data.frame( apply( dat, 2, function(x) scale(x, scale = FALSE)))
  }

  # check if intercept exists or ommited by formula
  if (names(coeff)[1] == "(Intercept)") {
    intrcpt_coeff = coeff[[1]]
  } else {
    intrcpt_coeff = 0
  }
  
  # identify moderating variable
  # CASE: non residual moderator
  xm_var = paste0(x_var,":",m_var)
  if(!xm_var %in% names(coeff)) {
    xm_var = paste0(m_var,":",x_var)
  }
  # CASE: residual moderator
  if(!xm_var %in% names(coeff)) {
    xm_var = paste0(x_var,".RX.",m_var)
    if(!xm_var %in% names(coeff)) {
      xm_var = paste0(m_var,".RX.",x_var)
    }
  }
  # CASE: centered moderator
  if(!xm_var %in% names(coeff)) {
    xm_var = paste0(x_var,".XX.",m_var)
    if(!xm_var %in% names(coeff)) {
      xm_var = paste0(m_var,".XX.",x_var)
    }
  }
  
  if(mod_values_type == "sd") {
    value_x_h = mean(dat[,x_var])+sd(dat[,x_var])
    value_x_l = mean(dat[,x_var])-sd(dat[,x_var])
  } else {
    value_x_l = quantile(dat[,x_var], probs = c(.25))
    value_x_h = quantile(dat[,x_var], probs = c(.75))
  }

  simple_slopes = list()
  x = c()
  y = c()

  for(i in 1:length( mod_values)) {
    # create values for moderator
    if( mod_values_type == "sd" && mod_values[i] != 0) {
      value_m = mean( dat[ ,m_var]) + (mod_values[i]) * sd( dat[ ,m_var])
    } else {
      value_m = mod_values[i]
    }
    
    # calculate slope value (=w1)
    # see Preacher, Curran, and Bauer (2006)
    #     y = b0+b1x+b2z+b3xz
    # <=> y = (b0+b2*z)+(b1+b3z)*x
    # <=> y = w0+w1*x
    slope = coeff[[x_var]] + coeff[[xm_var]] * value_m
    # SE and t-value
    w1 = eval(cov_matrix[x_var,x_var] +
                2*value_m*cov_matrix[x_var,xm_var] +
                value_m*value_m*cov_matrix[xm_var,xm_var])
    SE = sqrt(w1)
    t_value = eval(slope/SE)
    # p -value
    p_value = 2 * (1 - pt(abs(t_value), df=degfreedm))
    # confidence intervals
    crit_t_val = qt((1-(1-(user_ci))/2), df=degfreedm) # two sided
    margin_of_error = crit_t_val*SE

    # output 
    sisl = c( mod_values[i], value_m, slope, SE, t_value, p_value, eval(slope-margin_of_error), eval(slope+margin_of_error))
    names(sisl) = list("m_val_user","m_val_data","slope","SE","t_value", "p_value", "LLCI", "ULCI")
    simple_slopes[[i]] = sisl
        
    # Plot Values
    y_l = eval(intrcpt_coeff+
                 (coeff[[x_var]]+coeff[[xm_var]]* value_m)*value_x_l
               +coeff[[m_var]]*value_m)
    
    y_h = eval(intrcpt_coeff+
                 (coeff[[x_var]]+coeff[[xm_var]]* value_m)*value_x_h
               +coeff[[m_var]]*value_m)

    # output
    x <- c(x, value_x_l, value_x_h)
    y <- c(y, y_l, y_h)
  }
  
  plotvalues = list( x=x, y=y)

  # # # general information for simple slopes test
  info = list( y_var, x_var, m_var, xm_var, user_ci, mod_values_type, mod_values)
  names(info) = c( "Y", "X", "M", "XM", "Confidence_Interval", "Type_of_moderator_values", "Values_of_Moderator")
  
  object = list( original=simple_slopes, info=info, plot=plotvalues)
  class(object) = "simpleslopes"
  return( object)
}
