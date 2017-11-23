#' @title adds residual interactions, imputes and centers
#' @description
#' This is a helper function of \code{\link{bootmi}} that
#' \enumerate{
#'   \item calculates the (residual) interaction terms,
#'   \item imputes missing values using mice, and 
#'   \item centers interaction terms
#' }
#' @param frmla A regression formula
#' @param data A data.frame used for the regression
#' @param res_int TRUE or FALSE. Calculate residual interactions?
#' @param imputation Imputation method provided by mice-package.
#' @param center_mods TRUE or FALSE. Center interaction terms?
#' @param bootstraps TRUE or FALSE. Bootstrap sample or not?
#' @return If bootstraps = FALSE: Returns list of data (list$data) and 
#' updated formula (list$formula)
#' If bootstraps = TRUE: Returns data
#' @author Stephan Volpers \email{stephan.volpers@@plixed.de}
#' @export
#' @references Hippel, Paul T. von (2009): How to Impute Interactions,
#' Squares, and other Transformed Variables. In: 
#' Sociological Methodology 39 (1), S. 265–291. 
#' DOI: 10.1111/j.1467-9531.2009.01215.x.

resimpcen <- function( frmla, data, res_int, imputation, center_mods, bootstraps=FALSE) {

  # create residual interactions AFTER bootstrapping, because...
  # ...residuals depend on regression results of each sample
  if(res_int == TRUE) {
    res = add_residual_interactions( frmla, data)
    frmla = res$formula
    data = res$data
  } else {
    int = add_interactions( frmla, data)
    frmla = int$formula
    data = int$data
  }
  ids = as.numeric( rownames( data))

  # IMPUTE data WHEN RESIDUALS EXIST
  # Check if imputation is needed 
  if( (imputation != "none") && (( sum( is.na( data))) > 0 )) {
    id = as.numeric( rownames( data))
    # impute data
    mids_data = mice::mice( data, method=imputation, m=1, print=FALSE)
    # convert from mids object to data set
    data = mice::complete( mids_data, action="long", include=FALSE)
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
    print( unlist(centered_vars))
    # center interaction terms
    data = centering( data, unlist(centered_vars))
  }
  rownames(data) = ids

  if(bootstraps) {
    return(data)
  } else {
    return( list(formula=frmla, data=data))
  }
}
