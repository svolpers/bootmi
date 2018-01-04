#' @title creates residuals
#' @description
#' Helper function of \code{\link{add_residual_interactions}} to 
#' create a specific residual interaction term.
#' @param variable an interaction term
#' @param data a data.frame
#' @param new_varname name of the residual interaction term
#' @return A data.frame of one row consisting of the residual interaction term.
#' @author Stephan Volpers \email{stephan.volpers@@plixed.de}
#' @export

build_residual <- function( variable, data, new_varname) {
  # get number of terms depending on interation 
  nterms = length( strsplit( variable, ':')[[1]])
  # if no interaction return NULL
  if( nterms == 1) return(NULL)
  # build dependent varname = Interaction term
  y = paste0( "I(", gsub( ":", "*", variable, fixed=TRUE), ")")
  # build independent varnames
  xs = paste0( "(", gsub( ":", "+", variable, fixed=TRUE), ")")
  # for higer order interactions build relvant interaction tems
  if( nterms > 2) xs = paste0( xs, "^", nterms-1)
  # create formula
  reg_form = as.formula( paste0(y,"~",xs))
  # extract variable names from formula
  rcterms = attr( terms(reg_form), "term.labels")
  # replace interactions with residual term already used
  rcterms = gsub( ":", ".RX.", rcterms, fixed=TRUE)
  # create final formula
  finalFormula = paste0( y, "~", paste(rcterms, collapse = "+"))
  # calculate regression
  aReg = lm( finalFormula, data)
  # save residuals in data frame
  res = data.frame( as.numeric( resid( aReg)), stringsAsFactors = FALSE)
  # rename column
  colnames(res) = c(new_varname)
  # reset rownames
  rownames(res) = rownames(model.matrix(aReg))
  # return 
  return(res)
}
