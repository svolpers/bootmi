build_residual <-
function( variable, data, new_varname) {
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
  res = data.frame( resid( aReg), stringsAsFactors = FALSE)
  # rename column
  colnames(res) = c(new_varname)
  # return 
  return(res)
}
