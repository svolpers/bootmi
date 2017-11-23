#' @title Simple slopes: Calculates simple slopes
#' @description
#' Conducts analysis of the simple slopes
#' @details
#' The method is an implementation of analysis of the simple slopes  
#' as proposed by Preacher, Curran, and Bauer (2006). The default takes 
#' parameters needed to conduct analysis of the simple slopes. For objects 
#' of class "bootmi.lm", "lm", "lmerMod" and "mira" exist helper functions 
#' that extract parameters "coeff", "dat", "cov_matrix" from the object.
#' @param coeff Coefficients from fitted model, just for default
#' @param dat Data used to fit model, just for default
#' @param cov_matrix Variance-Covariance-Matrix of fitted model, 
#' just for default
#' @param object Object, if availiable but NOT for default 
#' @param x_var Name of the independend variable
#' @param m_var Name of the moderating variable
#' @param y_var Name of the dependend variable
#' @param ci Confidence interval, default 95 
#' @param mod_values_type Either sd (=standard deviation, default) or 
#' val (=values of data used)
#' @param mod_values Vector of values of the moderator, default c(-1,0,1)
#' @param centered Interaction coefficent mean centered? TRUE or FALSE
#' @return object of class "simpleslopes"
#' @name simpleslopes
#' @author Stephan Volpers <stephan.volpers@plixed.de>
#' @references Preacher, Kristopher J.; Curran, Patrick J.; Bauer, Daniel J. 
#' (2006): Computational Tools for Probing Interactions in Multiple Linear 
#' Regression, Multilevel Modeling, and Latent Curve Analysis. In: Journal of 
#' Educational and Behavioral Statistics 31 (4), S. 437–448. 
#' DOI: 10.3102/10769986031004437.
#' @export

simpleslopes <-
function( coeff, dat, cov_matrix, x_var, m_var, y_var, ci=95, mod_values_type=c("sd","val"), mod_values=c(-1,0,1), centered=FALSE) {
    UseMethod("simpleslopes")
}

#' @rdname simpleslopes
#' @export
simpleslopes.default <-
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
    
    # calculate slope value w1
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

  # general information for simple slopes test
  info = list( y_var, x_var, m_var, xm_var, user_ci, mod_values_type, mod_values)
  names(info) = c( "Y", "X", "M", "XM", "Confidence_Interval", "Type_of_moderator_values", "Values_of_Moderator")
  
  object = list( original=simple_slopes, info=info, plot=plotvalues)
  class(object) = "simpleslopes"
  return( object)
}

#' @section Helper functions exist: Uses object to extract coefficients, 
#' data, dependend variable and variance covariance matrix
#' @rdname simpleslopes
#' @export
simpleslopes.lm <-
function( object, x_var, m_var, ci=95, mod_values_type=c("sd","val"), mod_values=c(-1,0,1), centered=FALSE) {
	# get dependend variable
	modelt = terms( object)
	y_var = as.character( modelt[[2L]])
	# get data used in regression
	dat = model.matrix( object)
	# get regression coefficients
	coeff = coefficients( object)
	# get covariance matrix of parameter estimates (ACOV-matrix)
	cov_matrix = vcov( object)
	# calculate simple slopes
	slopes = simpleslopes.default( coeff, dat, cov_matrix, x_var, m_var, y_var, ci, mod_values_type, mod_values, centered)
	return( slopes)
}

#' @rdname simpleslopes
#' @export
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
	# calculate simple slopes
	slopes = simpleslopes.default( coeff, dat, cov_matrix, x_var, m_var, y_var, ci, mod_values_type, mod_values, centered)
	return( slopes)
}

#' @rdname simpleslopes
#' @export
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
	# calculate simple slopes
	slopes = simpleslopes.default( coeff, dat, cov_matrix, x_var, m_var, y_var, ci, mod_values_type, mod_values, centered)
	return( slopes)
}


#' @rdname simpleslopes
#' @export
simpleslopes.bootmi.lm <-
function( bootmi_lm_obj, x_var, m_var, ci=95, mod_values_type = "sd", mod_values = c(-1,0,1), centered=TRUE) {

	# calculate simple solpes for original data set
	si_sl = simpleslopes( lm_object=bootmi_lm_obj$original, x_var=x_var, m_var=m_var, mod_values_type=mod_values_type, mod_values=mod_values, centered=bootmi_lm_obj$center_mods)

	# extract coefficients for later use of boot.ci helper functions
	coeff = extract_coeff( si_sl)
	si_sl$original = list( slopes=si_sl$original, coef=coeff )

	# create empty matrix for bootstrapped coefficients
	bscoef = matrix(0, nrow = bootmi_lm_obj$replics, ncol = length( mod_values))

	# loop through values of the moderator
	for(i in 1:length( mod_values)) {
	# calculate bootstrap coefficients for the moderator
	bootstrcoeff = ( bootmi_lm_obj$bootstraps[ , si_sl$info$X] 
	        + bootmi_lm_obj$bootstraps[ , si_sl$info$XM]
	        * si_sl$original$slopes[[i]][["m_val_data"]] )
	# insert values in matrix
	bscoef[ ,i] = bootstrcoeff
	}
	# name cols of matrix
	colnames(bscoef) = mod_values

	# create object, name class and return object
	object = append( si_sl, list(bootstraps=bscoef, data=bootmi_lm_obj$data, formula=bootmi_lm_obj$formula, replics=bootmi_lm_obj$replics) )
	class(object) = "simpleslopes.bootmi"
	return( object)
}