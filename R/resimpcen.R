resimpcen <-
function(frmla, data, res_int, imputation, center_mods, bootstraps=FALSE) {

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
  # print( str( data))
  # print( summary( data))
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
  # print( length( rownames(data)))
  rownames(data) = ids

  if(bootstraps) {
    return(data)
  } else {
    return( list(formula=frmla, data=data))
  }
}
