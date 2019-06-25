#' @title Prepare calculation of bootstrapped confidence intervals.
#' @description
#' This is a helper function to make use of \code{\link[boot]{boot.ci}} method 
#' from boot package.
#' @param boot_object object of class "simpleslopes.bootmi" or 
#' "bootmi.lm"
#' @author Stephan Volpers \email{stephan.volpers@@plixed.de}
#' @export

calc_bootmi <- function ( 
  data
  , frmla
  , imputationMethod
  , glm_family
  , res_int = FALSE
  , center_mods = FALSE
  , simslopinfo = FALSE
  ) {
  

  ####
  # Start creating data
  ####

  # indices = sample( rownames(data), replace = TRUE)
  # bootstr_sample = data[indices, !grepl("\\.RX\\.", colnames( data))]

  # create residual interactions AFTER bootstrapping, because...
  # ...residuals depend on regression results of each sample
  if(res_int == TRUE) {
    res = ( frmla, data)
    frmla = res$formula
    data = res$data
  } else {
    int = add_interactions( frmla, data)
    frmla = int$formula
    data = int$data
  }


    # # correct data and formula with actual values after creating residual interactions 
    # # if intercept ommitted
    # if( TRUE %in% grepl( "-1", formula) ) {
    #   # add ommit
    #   f = sub( "~", "~ -1 +", res$formula)
    #   formula = as.formula( paste( f[[2]], f[[1]], f[[3]]))
    # } else {
    #   formula = res$formula
    # }
    # data = res$data


  # extract ids here, needed in case of centering
  ids = as.numeric( rownames( data))


  # IMPUTE data WHEN RESIDUALS EXIST
  # Check if imputation is needed 
  if( 
    (imputation != "none") 
    && (( sum( is.na( data))) > 0 )
  ) {
    id = as.numeric( rownames( data))
    # impute data
    mids_data = mice::mice( 
      data
      , method = imputation
      , m = 1
      , print = FALSE
    )
    # convert from mids object to data set
    data = mice::complete( 
      mids_data
      , action="long"
      , include=FALSE
    )
    data = data[ , -c(1,2)]
    rownames(data) = id
  }


  # Center Variables, if requested
  if( center_mods == TRUE ) { 
    # extract terms
    terms = attr( terms( as.formula( frmla)), "term.labels")
    # extract interaction terms
    centered_vars = sapply( terms, function(x) {
      if( grepl( '.RX.', x, fixed = TRUE) 
        || grepl( '.XX.', x, fixed = TRUE) 
        || grepl( ':', x, fixed = TRUE) ) {
        return(x)
      }
    })
    # center interaction terms
    data = centering( data, unlist(centered_vars))
  }
  rownames(data) = ids


  ####
  # Apply statistical method
  ####

  # supress warnings
  options(warn=-1)
  # calculate linear regression
  lmfit = glm(
    formula = frmla
    , data = data
    , family = glm_family
    )
  # end supression of warnings
  options(warn=0)


  # # caculate simple slopes
  # if( simslopinfo != FALSE ) {

  #   # need x and m be centered ?
  #   if( center_mods == TRUE | res_int == TRUE ) {
  #     center = TRUE
  #   } else {
  #     center = FALSE
  #   }

  #   # calculate simple slopes 
  #   # currently for one iv 
  #   # but n moderator variables
  #   simslops = vapply( 
  #     simslopinfo$m_var
  #     , function( m) {
  #       simslop(
  #         object = lmfit
  #         , x_var = simslopinfo$x_var
  #         , m_var = m
  #         , ci = FALSE
  #         , mod_values_type = simslopinfo$mod_values_type
  #         , mod_values = simslopinfo$mod_values
  #         , centered = center
  #       )
  #     }
  #     , lmfit = lmfit
  #     , simslopinfo = simslopinfo
  #     , centered = center
  #     )

  # } # end if simple slopes

}



