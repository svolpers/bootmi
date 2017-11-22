#' calculates ordinary bootstrap samples
#'
#' This is a helper function that calculates a given amount of
#' ordinary bootstrap samples from a data.frame
#'
#' @param data a data.frame used for the regression
#' @param R number of bootstrap samples. Default is 5000.
#' @return Object containing the updated regression formula \code{obj$formula}
#' and data set \code{obj$data} 
#' @author Stephan Volpers \email{stephan.volpers@@plixed.de}
#' @export

boot_samples <-
function( data, R=5000) {
  # create R bootstrap samples and save in list
  samples = lapply( (1:R), function(x) {
    data[sample( nrow(data), nrow(data), replace = TRUE), ]
  })
  # return
  return(samples)
}
