#' @title add_interactions
#' @description
#' Creates interaction terms and adds them to the data frame
#' @param formula a regression formula including the interaction terms 
#' @param data a data frame used for the regression
#' @return Object containing the updated regression formula \code{obj$formula}
#' and data set \code{obj$data} 
#' @author Stephan Volpers \email{stephan.volpers@@plixed.de}
#' @export

add_interactions <- function( formula, data) {
 
  # create id by rownames as helper for later merge
  data$id = as.numeric( rownames( data))
  
  # regress regular model to obtain interaction terms
  model = lm( formula, data=data)
  # extract data used by regression
  dat = model.matrix(model)
  # remove intercept
  if ( colnames(dat)[1] == "(Intercept)" ) dat = dat[ , -1] 
  # prepare identification of interaction terms
  ivs = gsub( ":", ".XX.", colnames(dat), fixed=TRUE)
  #get model terms
  modelt = terms(model)

  # iterate through variable list and build interaction terms
  for (i in seq_along(ivs)) {
    # check if variable is interaction term
    if( grepl(".XX.", ivs[i], fixed=TRUE )) {
      # get single variables
      x_m_var =  strsplit( ivs[i], '.XX.')[[1]]
      # create interaction term
      data[ivs[i]] = data[x_m_var[1]] * data[x_m_var[2]]
      # if terms > 2, loop through terms
      if( length(x_m_var) > 2 ) {
        for(j in 3:length(x_m_var)) {
          data[ivs[i]] = data[ivs[i]] * data[x_m_var[j]]
        }
      }
    }
  }

  #save dependent var as character
  depvar = as.character( modelt[[2L]])
  #create formula for independent vars
  frmla = paste0( ivs, collapse = "+")
  #create whole formula
  frmla = as.formula( paste0( depvar, "~", frmla))
  
  #extract data needed
  final_data = data[c(ivs,depvar)]

  # return dataframe and new regression formula
  return( list( data=final_data, formula=frmla) )
}
