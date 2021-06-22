#' @title add_residual_interactions
#' @description
#' Creates residual interaction terms and adds them to the data.frame
#' @param formula a regression formula including the interaction terms 
#' @param data a data.frame used for the regression
#' @return Object containing the updated regression formula \code{obj$formula}
#' and data set \code{obj$data} 
#' @author Stephan Volpers \email{stephan.volpers@@plixed.de}
#' @export

# create a function
make_dummies <- function(v, prefix = '') {
  s <- sort(unique(v))
  d <- outer(v, s, function(v, s) 1L * (v == s))
  colnames(d) <- paste0(prefix, s)
  d
}

add_residual_interactions <- function( formula, data) {

  # formula <- interaction_formula
  # data <- model.dat

  # bind the dummies to the original dataframe
  if(length(seq_along(data)[sapply( data, class) == "factor"]) > 0 ) {
    data <- cbind( data,
      lapply(
        seq_along(data)[sapply( data, class) == "factor"]
        , function( x, datafr) {
          if( class(datafr[[x]]) == "factor" ) {
            make_dummies( datafr[[x]], prefix = colnames( datafr[x]))
          } else {
            datafr[[x]]
          }
        }
        , datafr = data
      )
    )
  }
  # remove duplicates if factors have already been added manually
  data <- data[!duplicated( colnames(data))]

  # create lmresid_merge_temp_var by rownames as helper for later merge
  data$lmresid_merge_temp_var = as.numeric( rownames( data))
  # regress regular model to obtain interaction terms
  model = lm( formula, data=data)
  # extract data used by regression
  dat = model.matrix(model)
  # remove intercept
  if ( colnames(dat)[1] == "(Intercept)" ) dat = dat[, -1] 
  # prepare identification of interaction terms
  ivs = gsub( "(?:I\\()?(\\w*)\\s*(?:\\*|:)\\s*(\\w*)(?:\\))?"
    , "\\1.RX.\\2"
    , colnames(dat)
  )

  # iterate through variable list and build interaction terms
  for (i in seq_along(ivs)) {
    # check if variable is interaction term
    if( grepl(".RX.", ivs[i], fixed=TRUE )) {
      # create residual for interaction term
      res = build_residual( 
        variable= gsub( ".RX.", ":", ivs[i])
        , data= data
        , new_varname= ivs[i]
        )
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

  #save dependent var as character
  depvar = as.character( terms(model)[[2L]])
  #create formula for independent vars
  frmla = paste0( ivs, collapse = "+")
  #create whole formula
  frmla = as.formula( paste0( depvar, "~", frmla))


  #extract data needed
  final_data = data[c(ivs,depvar)]

  # return dataframe and new regression formula
  return( list( data=final_data, formula=frmla) )

}
