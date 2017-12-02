#' @title Calculates ordinary bootstrap samples
#' @description
#' This is a function that calculates a given amount of
#' ordinary bootstrap samples from a data.frame.
#' @param data A data.frame used for the regression
#' @param R Number of bootstrap samples. Default is 5000.
#' @return List containing R bootstrap samples 
#' @author Stephan Volpers \email{stephan.volpers@@plixed.de}
#' @export

boot_samples <- function( data, R=5000) {
	
	if( !is.matrix( data) && !is.data.frame( data)) {
		stop("Matrix or data.frame needed.")
	}
	# extract amount of rows
	rownumbers = nrow(data)
	# create R bootstrap samples and save in list
	samples = lapply( (1:R), function(x) {
		data[ sample( rownumbers, rownumbers, replace = TRUE), ]
	})
	# return
	return(samples)
}
