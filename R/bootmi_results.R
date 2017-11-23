#' @title creates output table for bootmi.lm
#' @description
#' This is a helper function of \code{\link{summary.bootmi.lm}} and 
#' \code{\link{summary.simpleslopes.bootmi}} that creates output 
#' of the analyses.
#' @param boot_object object of class "simpleslopes.bootmi" or 
#' "bootmi.lm"
#' @author Stephan Volpers \email{stephan.volpers@@plixed.de}
#' @export

bootmi_results <- function( bootmi.lm) {
  
  # calculate bootstrap info
  bias = colMeans( bootmi.lm$bootstraps) - bootmi.lm$original$coef 
  bias_corrected_estimate = bootmi.lm$original$coef - bias

  # create output matrix
  output = matrix( c( bootmi.lm$original$coef, bias, bias_corrected_estimate), nrow = length( bootmi.lm$original$coef) )

  # helper function to calculate bootstrap ci with boot package
  boot_ci = bootmi_ci( bootmi.lm) 
  # calculate Confidence Intervals
  boot::boot.ci( boot_ci, conf=bootmi.lm$ci, type=bootmi.lm$ci_type, index=1)

  # create empty vector for confidence intervals
  cis = c()
  # loop through independent variables of the model
  for(i in 1:length( bootmi.lm$original$coef) ) {
    # use boot function boot.ci to calculate bootstrap confidence intervals
    val = boot::boot.ci( boot_ci, conf=bootmi.lm$ci, type=bootmi.lm$ci_type, index=i)[[bootmi.lm$ci_type]]
    if(bootmi.lm$ci_type == "norm")
      cis = c( cis, val[2], val[3])
    else
      cis = c( cis, val[4], val[5])
  }
  # add CI to output matrix
  output = cbind( output, matrix( cis, ncol = 2, byrow = TRUE))
  # round output
  output = apply(output, c(1,2), function(x) round(x , digits = 4))
  # name cols and rows
  colnames(output) <- c("Estimate Original", "Bias", "Estimate Bias corrected", "BLLCI", "BULCI")
  rownames(output) <- names( bootmi.lm$original$coef)
  # return 
  return( output)
}
