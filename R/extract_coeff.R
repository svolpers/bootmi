extract_coeff <-
function( simple_slopes) {
  # extract moderator and slope values
  vals = sapply(simple_slopes$original, function(x) {
    return( c( x[["m_val_user"]], x[["slope"]] ))
  })
  # save slope values
  coeff = vals[2,]
  # name slope values with moderator values
  names(coeff) = (vals[1,])
  # return named num
  return(coeff)
}
