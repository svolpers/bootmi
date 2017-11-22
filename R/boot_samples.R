boot_samples <-
function( data, R=5000) {
  # create R bootstrap samples and save in list
  samples = lapply( (1:R), function(x) {
    data[sample( nrow(data), nrow(data), replace = TRUE), ]
  })
  # return
  return(samples)
}
