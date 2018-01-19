#' @title add_residual_interactions
#' @description
#' Creates residual interaction terms and adds them to the data.frame
#' @param formula a regression formula including the interaction terms 
#' @param data a data.frame used for the regression
#' @return Object containing the updated regression formula \code{obj$formula}
#' and data set \code{obj$data} 
#' @author Stephan Volpers \email{stephan.volpers@@plixed.de}
#' @export

add_residual_interactions <- function( formula, data) {
  
  # create lmresid_merge_temp_var by rownames as helper for later merge
  data$lmresid_merge_temp_var = as.numeric( rownames( data))
  # regress regular model to obtain interaction terms
  model = lm( formula, data=data)
  # extract data used by regression
  dat = model.matrix(model)
  # remove intercept
  if ( colnames(dat)[1] == "(Intercept)" ) dat = dat[, -1] 
  # prepare identification of interaction terms
  dat_col_names = colnames(dat)
  
  # iterate through variable list and build residual interactions
  for (i in seq_along(dat_col_names)) {
    # check if variable is interaction term
    if( grepl(":", dat_col_names[i], fixed=TRUE ) ) {
      #create varname for interaction term
      newname = gsub(":", ".RX.", dat_col_names[i], fixed=TRUE)
      # create residual for interaction term
      res = build_residual( dat_col_names[i] , data=data, new_varname=newname)
      # add residuals of interaction term to orginal data set
      if ( is.null(res) )  stop("Error creating residual interactions: Unable to calculate!")
      # insert lmresid_merge_temp_var to prepare for merging
      res$lmresid_merge_temp_var = as.numeric( rownames( res))
      # merge data sets
      data = merge( data, res, by="lmresid_merge_temp_var", all.x=TRUE)
      # reset rownames after merging
      rownames(data) = data$lmresid_merge_temp_var
    }
  }

  #get model terms
  modelt = terms(model)
  ivs = gsub( ":", ".RX.", dat_col_names, fixed=TRUE)
  #save dependent var as character
  depvar = as.character( modelt[[2L]])
  
  #create formula for independent vars
  frmla = paste0( ivs, collapse = "+")
  #create whole formula
  frmla = as.formula( paste0( depvar, "~", frmla))
  
  #extract data needed
  final_data = data[c(ivs,depvar)]

  # return dataframe and new regression formula
  return( list( data=final_data, formula=frmla))
}
