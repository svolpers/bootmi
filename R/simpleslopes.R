#' @title Conducts analysis of the simple slopes
#' @description
#' The method is an implementation of analysis of the simple slopes  
#' as proposed by Preacher, Curran, and Bauer (2006). The default takes 
#' parameters needed to conduct analysis of the simple slopes. For objects 
#' of class "bootmi.lm", "lm", "lmerMod" and "mira" exist helper functions 
#' that extract parameters "coeff", "dat", "cov_matrix" from the object.
#' @param object Object containing $coeff, $dat, $cov_matrix
#' @param x_var Name of the independend variable
#' @param m_var Name of the moderating variable
#' @param ci Confidence interval, default 95 
#' @param mod_values_type Either sd (=standard deviation, default) or 
#' val (=values of data used)
#' @param mod_values Vector of values of the moderator, default c(-1,0,1)
#' @param centered Interaction coefficent mean centered? TRUE or FALSE
#' @return object of class "simpleslopes"
#' @name simpleslopes
#' @author Stephan Volpers \email{stephan.volpers@@plixed.de}
#' @references Preacher, Kristopher J.; Curran, Patrick J.; Bauer, Daniel J. 
#' (2006): Computational Tools for Probing Interactions in Multiple Linear 
#' Regression, Multilevel Modeling, and Latent Curve Analysis. In: Journal of 
#' Educational and Behavioral Statistics 31 (4), S. 437-448.
#' @export

simpleslopes <- function( object, x_var, m_var, ci=95, mod_values_type=c("sd","val"), mod_values=c(-1,0,1), centered=FALSE) {
    UseMethod("simpleslopes")
}

#' @rdname simpleslopes
#' @export
simpleslopes.default <- function( object, x_var, m_var, ci=95, mod_values_type=c("sd","val"), mod_values=c(-1,0,1), centered=FALSE) {
  
  mod_values_type = match.arg( mod_values_type) 
  
  # set confidence interval value
  ci = as.integer(ci)
  if(!(ci > 0 && ci < 100)) {
      stop("Error: Please enter a confidence interval of ]0;100[. Only natural numbers are allowed.")
  }
  user_ci = ci/100

  degfreedm = nrow( object$dat) - length( object$coeff) - 1
  if(degfreedm < 1) {
      stop("Error: Wrong data or coefficients.")
  }
  
  # if moderators are mean-centered, x and m var also have to be mean-centered
  if( centered==TRUE ) {
    object$dat = data.frame( apply( object$dat, 2, function(x) scale(x, scale = FALSE)))
  }

  # check if intercept exists or ommited by formula
  if (names( object$coeff)[1] == "(Intercept)") {
    intrcpt_coeff = object$coeff[[1]]
  } else {
    intrcpt_coeff = 0
  }
  
  # identify moderating variable
  # CASE: non residual moderator
  xm_var = paste0(x_var,":",m_var)
  if(!xm_var %in% names( object$coeff)) {
    xm_var = paste0(m_var,":",x_var)
  }
  # CASE: residual moderator
  if(!xm_var %in% names( object$coeff)) {
    xm_var = paste0(x_var,".RX.",m_var)
    if(!xm_var %in% names( object$coeff)) {
      xm_var = paste0(m_var,".RX.",x_var)
    }
  }
  # CASE: centered moderator
  if(!xm_var %in% names( object$coeff)) {
    xm_var = paste0(x_var,".XX.",m_var)
    if(!xm_var %in% names( object$coeff)) {
      xm_var = paste0(m_var,".XX.",x_var)
    }
  }
  
  if(mod_values_type == "sd") {
    value_x_h = mean( object$dat[,x_var])+sd( object$dat[,x_var])
    value_x_l = mean( object$dat[,x_var])-sd( object$dat[,x_var])
  } else {
    value_x_l = quantile( object$dat[,x_var], probs = c(.25))
    value_x_h = quantile( object$dat[,x_var], probs = c(.75))
  }

  simple_slopes = list()
  x = c()
  y = c()

  for(i in 1:length( mod_values)) {
    # create values for moderator
    if( mod_values_type == "sd" && mod_values[i] != 0) {
      value_m = mean( object$dat[ ,m_var]) + (mod_values[i]) * sd( object$dat[ ,m_var])
    } else {
      value_m = mod_values[i]
    }
    
    # calculate slope value w1
    # see Preacher, Curran, and Bauer (2006)
    #     y = b0+b1x+b2z+b3xz
    # <=> y = (b0+b2*z)+(b1+b3z)*x
    # <=> y = w0+w1*x
    slope = object$coeff[[x_var]] + object$coeff[[xm_var]] * value_m
    # SE and t-value
    w1 = eval( object$cov_matrix[x_var,x_var] +
                2*value_m*object$cov_matrix[x_var,xm_var] +
                value_m*value_m*object$cov_matrix[xm_var,xm_var])
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
                 (object$coeff[[x_var]]+object$coeff[[xm_var]]* value_m)*value_x_l
               +object$coeff[[m_var]]*value_m)
    
    y_h = eval(intrcpt_coeff+
                 (object$coeff[[x_var]]+object$coeff[[xm_var]]* value_m)*value_x_h
               +object$coeff[[m_var]]*value_m)

    # output
    x <- c(x, value_x_l, value_x_h)
    y <- c(y, y_l, y_h)
  }
  
  plotvalues = list( x=x, y=y)

  # general information for simple slopes test
  info = list( object$y_var, x_var, m_var, xm_var, user_ci, mod_values_type, mod_values)
  names(info) = c( "Y", "X", "M", "XM", "Confidence_Interval", "Type_of_moderator_values", "Values_of_Moderator")
  
  object = list( original=simple_slopes, info=info, plot=plotvalues)
  class(object) = "simpleslopes"
  return( object)
}

#' @section Helper functions exist: Uses object to extract coefficients, 
#' data, dependend variable and variance covariance matrix
#' @rdname simpleslopes
#' @export
simpleslopes.lm <- function( object, x_var, m_var, ci=95, mod_values_type=c("sd","val"), mod_values=c(-1,0,1), centered=FALSE) {
	# get dependend variable
	modelt = terms( object)
	y_var = as.character( modelt[[2L]])
	# get data used in regression
	data_set = model.matrix( object)
	# get regression coefficients
	coeff = coefficients( object)
	# get covariance matrix of parameter estimates (ACOV-matrix)
	cov_matrix = vcov( object)
  # merge to obj
  obj = list( coeff= coeff, y_var= y_var, cov_matrix= cov_matrix, dat= data_set)
	# calculate simple slopes
	slopes = simpleslopes.default( obj, x_var, m_var, ci, mod_values_type, mod_values, centered)
	return( slopes)
}

#' @rdname simpleslopes
#' @export
simpleslopes.mira <- function( object, x_var, m_var, ci=95, mod_values_type=c("sd","val"), mod_values=c(-1,0,1), centered=FALSE) {
	
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
	data_set = Reduce("+", dats) / length(dats)
  # merge to obj
  obj = list( coeff= coeff, y_var= y_var, cov_matrix= cov_matrix, dat= data_set)
  # calculate simple slopes
	slopes = simpleslopes.default( obj, x_var, m_var, ci, mod_values_type, mod_values, centered)
	return( slopes)
}

#' @rdname simpleslopes
#' @export
simpleslopes.lmerMod <- function( object, x_var, m_var, ci=95, mod_values_type=c("sd","val"), mod_values=c(-1,0,1), centered=FALSE) {
	# get dependend variable
	modelt = terms( object)
	y_var = as.character( modelt[[2L]])
	# get data used in regression
	data_set = model.matrix( object)
	# get regression coefficients
	coeff = coefficients( object)[[1]]
	coeff = colMeans( coeff)
	# get covariance matrix of parameter estimates (ACOV-matrix)
	cov_matrix = vcov( object)
	# merge to obj
  obj = list( coeff= coeff, y_var= y_var, cov_matrix= cov_matrix, dat= data_set)
  # calculate simple slopes
	slopes = simpleslopes.default( obj, x_var, m_var, ci, mod_values_type, mod_values, centered)
	return( slopes)
}


#' @rdname simpleslopes
#' @export
simpleslopes.bootmi.lm <- function( object, x_var, m_var, ci=95, mod_values_type = "sd", mod_values = c(-1,0,1), centered=TRUE) {

  modelt = terms( object$original)
  object$original$y_var = as.character( modelt[[2L]])

	# calculate simple solpes for original data set
	si_sl = simpleslopes( object=object$original, x_var=x_var, m_var=m_var, mod_values_type=mod_values_type, mod_values=mod_values, centered=object$center_mods)

	# extract coefficients for later use of boot.ci helper functions
  # extract moderator and slope values
  vals = sapply(si_sl$original, function(x) {
    return( c( x[["m_val_user"]], x[["slope"]] ))
  })
  # save slope values
  coeff = vals[2,]
  # name slope values with moderator values
  names(coeff) = (vals[1,])
  # append to simple slopes output
  si_sl$original = list( slopes=si_sl$original, coef=coeff )

	# create empty matrix for bootstrapped coefficients
	bscoef = matrix(0, nrow = object$replics, ncol = length( mod_values))

	# loop through values of the moderator
	for(i in 1:length( mod_values)) {
	# calculate bootstrap coefficients for the moderator
	bootstrcoeff = ( object$bootstraps[ , si_sl$info$X] 
	        + object$bootstraps[ , si_sl$info$XM]
	        * si_sl$original$slopes[[i]][["m_val_data"]] )
	# insert values in matrix
	bscoef[ ,i] = bootstrcoeff
	}
	# name cols of matrix
	colnames(bscoef) = mod_values

	# create object, name class and return object
	object = append( si_sl, list(bootstraps=bscoef, data=object$data, formula=object$formula, replics=object$replics) )
	class(object) = "simpleslopes.bootmi"
	return( object)
}