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
#' @param m2_var Name of optional 2nd moderating variable
#' @param ci Confidence interval, default 95 
#' @param mod_values_type Either sd (=standard deviation, default) or 
#' val (=values of data used)
#' @param mod_values Vector of values of the moderator, default c(-1,0,1)
#' @param centered Interaction coefficent mean centered? TRUE or FALSE
#' @param dat_org optional original data set used for mira objects, default NULL
#' @return object of class "simpleslopes"
#' @name simslop
#' @author Stephan Volpers \email{stephan.volpers@@plixed.de}
#' @references Preacher, Kristopher J.; Curran, Patrick J.; Bauer, Daniel J. 
#' (2006): Computational Tools for Probing Interactions in Multiple Linear 
#' Regression, Multilevel Modeling, and Latent Curve Analysis. In: Journal of 
#' Educational and Behavioral Statistics 31 (4), S. 437-448.
#' @export

simslop <- function( 
  object
  , x_var
  , m_var
  , m2_var = NULL
  , ci = 95
  , mod_values_type = c( "sd","val")
  , mod_values = c( -1,1)
  , centered = FALSE
  , dat_org = NULL
  ) { 
  UseMethod("simslop")
}

#' @rdname simslop
#' @export
simslop.default <- function( 
  object
  , x_var
  , m_var
  , m2_var = NULL
  , ci = 95
  , mod_values_type = c( "sd","val")
  , mod_values = c( -1,1)
  , centered = FALSE
  , dat_org = NULL
) {
  
  # get value of moderator
  mod_values_type <- match.arg( mod_values_type) 
  
  # set confidence interval value
  if( !is.null(ci) ) {
    ci <- as.integer(ci)
    if( !(ci > 0 && ci < 100) ) {
        stop("Error: Please enter a 
          confidence interval of ]0;100[. 
          Only natural numbers are allowed.")
    }
    user_ci <- ci/100
  } else {
    user_ci <- 0
  }

  # check if intercept in coefficents, 
  # intercept does not decrease degrees of freedom
  if ( names( object$coeff[1]) == "(Intercept)" ) {
    degfreedm <- nrow( object$dat) - length( object$coeff)
    intrcpt_coeff <- object$coeff[[1]]
  } else {
    degfreedm <- nrow( object$dat) - length( object$coeff) - 1
    intrcpt_coeff <- 0
  } 


  if(degfreedm < 1) {
      stop("Error: Wrong data or coefficients.")
  }
  # print( colnames( object$dat))
  # print( c(x_var,m_var,m2_var))

  # print( class( object$dat[,c(x_var,m_var,m2_var)]))
  # if moderators are mean-centered, 
  # x and m var also have to be mean-centered
  if( centered == TRUE ) {
    object$dat <- data.frame( 
      apply( 
        object$dat[,c(x_var,m_var,m2_var)]
        , 2
        , function(x){
          scale(x, scale = FALSE)
        }
      )
    )
  }

  # identify moderating variable(s)
  pattrn <- paste0( 
    "^(",x_var,"|",m_var,")(:|\\.XX\\.|\\.RX\\.)",
    "(",x_var,"|",m_var,")$")
  xm_var <- grep( pattrn, names( object$coeff), value= TRUE)

  if( !is.null(m2_var) ) {
    pattrn <- paste0( 
      "^(",x_var,"|",m2_var,")(:|\\.XX\\.|\\.RX\\.)",
      "(",x_var,"|",m2_var,")$"
      )
    xm2_var <- grep( pattrn, names( object$coeff), value= TRUE)

    pattrn <- paste0( 
       "^(",m_var,"|",m2_var,")(:|\\.XX\\.|\\.RX\\.)",
      "(",m_var,"|",m2_var,")$")
    mm2_var <- grep( pattrn, names( object$coeff), value = TRUE)

    pattrn <- paste0( 
      "(",x_var,"|",m_var,"|",m2_var,")(:|\\.XX\\.|\\.RX\\.)",
      "(",x_var,"|",m_var,"|",m2_var,")(:|\\.XX\\.|\\.RX\\.)",
      "(",x_var,"|",m_var,"|",m2_var,")")
    xmm2_var <- grep( pattrn, names( object$coeff), value = TRUE)
  }
   
  if(mod_values_type == "sd") {
    value_x_h <- eval(
      mean( object$dat[,x_var], na.rm = TRUE) 
      + sd( object$dat[,x_var], na.rm = TRUE)
      )
    value_x_l <- eval(
      mean( object$dat[,x_var], na.rm = TRUE) 
      - sd( object$dat[,x_var], na.rm = TRUE)
      )
  } else {
    value_x_l <- quantile( object$dat[,x_var], probs = c(.25))
    value_x_h <- quantile( object$dat[,x_var], probs = c(.75))
  }

  amnt_mod_vals <- length(mod_values)
  # create empty matrix
  if( !is.null(m2_var) ) {
    simslop <- data.frame( 
      matrix( 
        rep(0,6*amnt_mod_vals^(1+length(m2_var)))
        , ncol = 6
      ))
    colnames(simslop) <- c("value_m","value_m2","slope"
      ,"se","y_l","y_h")
  } else {
    simslop <- data.frame( 
      matrix( 
        rep(0,5*amnt_mod_vals^(1+length(m2_var)))
        , ncol = 5
      ))
    colnames(simslop) <- c("value_m","slope","se","y_l","y_h")
  }
  
  for(i in seq( amnt_mod_vals)) {
    
    # create values for moderator
    if( mod_values_type == "sd" ) { # && mod_values[i] != 0
      value_m <- eval(
        mean( object$dat[ ,m_var], na.rm = TRUE) 
        + (mod_values[i]) 
        * sd( object$dat[ ,m_var], na.rm = TRUE)
        )
    } else {
      value_m <- mod_values[i]
    }

    # calculate slope value w1
    # see Preacher, Curran, and Bauer (2006)
    #     y = b0+b1x+b2z+b3xz
    # <=> y = (b0+b2*z)+(b1+b3z)*x
    # <=> y = w0+w1*x
    slope <- eval( object$coeff[[x_var]] 
        + object$coeff[[xm_var]] * value_m )

    if( !is.null(user_ci) ) {
      # caculate variance
      w1 <- eval( object$cov_matrix[x_var,x_var]
        + ( 2 * object$cov_matrix[x_var,xm_var]
        + value_m*object$cov_matrix[xm_var,xm_var]
        ) * value_m )

      # Plot Values
      y_l <- eval( intrcpt_coeff
        + ( object$coeff[[x_var]] 
          + object$coeff[[xm_var]] 
          * value_m
          ) * value_x_l
        + object$coeff[[m_var]] * value_m
        )
      
      y_h <- eval( intrcpt_coeff
        + ( object$coeff[[x_var]]
          + object$coeff[[xm_var]]
          * value_m
        ) * value_x_h
        + object$coeff[[m_var]] * value_m
        )
    }

    # Extension for three-way-interaction
    if( !is.null(m2_var) ) {
      
      for(j in seq( amnt_mod_vals)) {

        if( mod_values_type == "sd" ) {     
          value_m2 <- eval( 
            mean( object$dat[ ,m2_var], na.rm = TRUE) 
            + (mod_values[j]) 
            * sd( object$dat[ ,m2_var], na.rm = TRUE)
          )
        } else {
          value_m2 <- mod_values[j]
        }
        simslop$value_m[((i-1)*amnt_mod_vals+j)] <- value_m
        simslop$value_m2[((i-1)*amnt_mod_vals+j)] <- value_m2

        # extend slope and variance calulation for three-way-interaction
        slope_ext <- eval( slope
          + ( object$coeff[[xm2_var]] 
            + object$coeff[[xmm2_var]] 
            * value_m 
          ) * value_m2
          )
        simslop$slope[((i-1)*amnt_mod_vals+j)] <- slope_ext
        
        if( !is.null(user_ci) ) {
          w1_ext <- eval( w1
            + ( value_m2*object$cov_matrix[xm2_var,xm2_var]
                +value_m*value_m*value_m2*object$cov_matrix[xmm2_var,xmm2_var]
                +2*(
                  object$cov_matrix[x_var,xm2_var]
                  +value_m*object$cov_matrix[x_var,xmm2_var]
                  +value_m*object$cov_matrix[xm_var,xm2_var]
                  +value_m*value_m*object$cov_matrix[xm_var,xmm2_var]
                  +value_m*value_m2*object$cov_matrix[xm2_var,xmm2_var]
                )
              ) * value_m2
            )
          simslop$se[((i-1)*amnt_mod_vals+j)] <- sqrt(w1_ext)

          y_l_ext <- eval(y_l
            + ( object$coeff[[m2_var]]
            + object$coeff[[xm2_var]]*value_x_l + object$coeff[[mm2_var]]*value_m
            + object$coeff[[xmm2_var]]*value_x_l*value_m ) * value_m2
          )
          y_h_ext <- eval(y_h
            + ( object$coeff[[m2_var]]
            + object$coeff[[xm2_var]]*value_x_h + object$coeff[[mm2_var]]*value_m
            + object$coeff[[xmm2_var]]*value_x_h*value_m ) * value_m2
          )
          simslop$y_l[((i-1)*amnt_mod_vals+j)] <- y_l_ext
          simslop$y_h[((i-1)*amnt_mod_vals+j)] <- y_h_ext
        }
    
      } # end for j
      
    } else { # if !is.null(m2_var) == FALSE
      simslop$value_m[i] <- value_m
      simslop$slope[i] <- slope
      if( !is.null(user_ci) ) {
        simslop$se[i] <- sqrt(w1)
        simslop$y_l[i] <- y_l
        simslop$y_h[i] <- y_h
      }
    } # end if is.null(m2_var)

  }

  if( !is.null(user_ci) ) {
    # SE and t-value
    # simslop$se = sqrt(simslop$var)
    simslop$t_value <- eval( simslop$slope / simslop$se)
    # p -value
    simslop$p_value <- eval( 2 
      * (1 - pt(abs(simslop$t_value), df = degfreedm))
      )
    # confidence intervals
    # two sided
    crit_t_val <- qt((1-(1-(user_ci))/2), df = degfreedm) 
    simslop$LBCI <- eval( simslop$slope-crit_t_val*simslop$se)
    simslop$UBCI <- eval( simslop$slope+crit_t_val*simslop$se)
  }

  # general information for simple slopes test
  info <- list( 
    "Y" = object$y_var
    ,"X" = x_var
    ,"x_low" = value_x_l
    ,"x_high" = value_x_h
    ,"M" = m_var
    ,"Confidence_Interval" = ci
    ,"Type_of_moderator_values" = mod_values_type
    ,"Values_of_Moderator" = mod_values
  )

  if( !is.null(m2_var) ) {
    info$M2 <- m2_var
  }

  slopes_object <- list( 
    simple_slopes = simslop
    , info = info
    )
  
  class( slopes_object) <- "simslop"
  return( slopes_object)
}

#' @section Helper functions exist: Uses object to extract coefficients, 
#' data, dependend variable and variance covariance matrix
#' @rdname simslop
#' @export
simslop.lm <- function( object, x_var, m_var, m2_var= NULL, ci=95, mod_values_type=c("sd","val"), mod_values=c(-1,0,1), centered=FALSE, dat_org= NULL) {
  # get dependend variable
	modelt <- terms( object)
	y_var <- as.character( modelt[[2L]])
	# get data used in regression
	data_set <- model.matrix( object)
	# get regression coefficients
	coeff <- coefficients( object)
	# get covariance matrix of parameter estimates (ACOV-matrix)
	cov_matrix <- vcov( object)
  # merge to obj
  obj <- list( coeff= coeff, y_var= y_var, cov_matrix= cov_matrix, dat= data_set)
	# calculate simple slopes
	slopes <- simslop.default(
    object = obj
    , x_var = x_var
    , m_var = m_var
    , m2_var= NULL
    , ci = ci
    , mod_values_type = mod_values_type
    , mod_values = mod_values
    , centered = centered
    , dat_org = NULL
  )
	return( slopes)
}

#' @rdname simslop
#' @export
simslop.mira <- function( object, x_var, m_var, m2_var= NULL, ci=95, mod_values_type=c("sd","val"), mod_values=c(-1,0,1), centered=FALSE, dat_org= NULL) {
  # get dependend variable
  modelt <- terms( object$analyses[[1]])
  y_var <- as.character( modelt[[2L]])
  # get regression coefficients
  coeff <- mice::getqbar( mice::pool(object))
  # get mean covariance matrix of parameter estimates (ACOV-matrix)
  vcovs <- lapply( object$analyses, vcov)
  cov_matrix <- Reduce("+", vcovs) / length(vcovs)
  # get mean imputed data used in regression
  if( is.null(dat_org)) {
    dats <- lapply( object$analyses, model.matrix)
    data_set <- Reduce("+", dats) / length(dats)  
  } else {
    data_set <- dat_org
  }
  # merge to obj
  obj <- list( 
    coeff= coeff
    , y_var= y_var
    , cov_matrix= cov_matrix
    , dat= data_set
    )
  # calculate simple slopes
  slopes <- simslop.default( obj, x_var, m_var, m2_var, ci, mod_values_type, mod_values, centered)
}

#' @rdname simslop
#' @export
simslop.lmerMod <- function( object, x_var, m_var, m2_var= NULL, ci=95, mod_values_type=c("sd","val"), mod_values=c(-1,0,1), centered=FALSE, dat_org= NULL) {
	# get dependend variable
	modelt <- terms( object)
	y_var <- as.character( modelt[[2L]])
	# get data used in regression
	data_set <- model.matrix( object)
	# get regression coefficients
	coeff <- coefficients( object)[[1]]
	coeff <- colMeans( coeff)
	# get covariance matrix of parameter estimates (ACOV-matrix)
	cov_matrix <- vcov( object)
	# merge to obj
  obj <- list( coeff= coeff, y_var= y_var, cov_matrix= cov_matrix, dat= data_set)
  # calculate simple slopes
	slopes <- simslop.default( obj, x_var, m_var, ci, mod_values_type, mod_values, centered)
	return( slopes)
}