#' @title creates output table for bootmi.lm
#' @description
#' This is a helper function of \code{\link{summary.bootmi.lm}} and 
#' \code{\link{summary.simslop.bootmi}} that creates output 
#' of the analyses.
#' @param bootmi_object object of class "simslop.bootmi" or 
#' "bootmi.lm"
#' @author Stephan Volpers \email{stephan.volpers@@plixed.de}
#' @export

bootmi_results <- function( bootmi_object) {
  
  # calculate bootstrap info
  bias = colMeans( bootmi_object$bootstraps) - bootmi_object$original$coef 
  bias_corrected_estimate = bootmi_object$original$coef - bias

  # create output matrix
  output = matrix( c( bootmi_object$original$coef, bias, bias_corrected_estimate), nrow = length( bootmi_object$original$coef) )

  # helper function to calculate bootstrap ci with boot package
  boot_ci = bootmi_ci( bootmi_object) 
  # calculate Confidence Intervals
  boot::boot.ci( boot_ci, conf=bootmi_object$ci, type=bootmi_object$ci_type, index=1)

  # create empty vector for confidence intervals
  ncoefs = length( bootmi_object$original$coef)
  cis = 2*vector("numeric", ncoefs)
  # loop through independent variables of the model
  for(i in seq(ncoefs) ) {
    # use boot function boot.ci to calculate bootstrap confidence intervals
    val = boot::boot.ci( boot_ci, conf=bootmi_object$ci, type=bootmi_object$ci_type, index=i)[[bootmi_object$ci_type]]
    if(bootmi_object$ci_type == "norm") {
      cis[i] = val[2]
      cis[(i+ncoefs)] = val[3]
    } else {
      cis[i] = val[4]
      cis[(i+ncoefs)] = val[5]
    }
  }
  # add CI to output matrix
  output = cbind( output, matrix( cis, ncol = 2, byrow = FALSE))
  # round output
  output = apply(output, c(1,2), function(x) round(x , digits = 4))
  # name cols and rows
  colnames(output) <- c("Estimate Original", "Bias", "Estimate Bias corrected", "BLLCI", "BULCI")
  rownames(output) <- names( bootmi_object$original$coef)
  # return 
  return( output)
}
