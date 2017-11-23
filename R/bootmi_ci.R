#' @title Prepare calculation of bootstrapped confidence intervals.
#' @description
#' This is a helper function to make use of \code{\link[boot]{boot.ci}} method 
#' from boot package.
#' @param boot_object object of class "simpleslopes.bootmi" or 
#' "bootmi.lm"
#' @author Stephan Volpers \email{stephan.volpers@@plixed.de}
#' @export

bootmi_ci <- function( boot_object) {

  if( (class(boot_object) != "simpleslopes.bootmi") && 
    (class(boot_object) != "bootmi.lm")
  ) { 
    stop("Object of class bootmi.lm or simpleslopes.bootmi required.")
  }
  sample_size = nrow( boot_object$data)
  set.seed( boot_object$seed)
  # create boot information
  boot_ci = list (
    t0 = boot_object$original$coef, 
    t = boot_object$bootstraps, 
    R = boot_object$replics, 
    data = boot_object$data, 
    seed = .Random.seed, 
    sim = "ordinary", 
    call = as.call( quote( boot(data = boot_object$data, statistic = bootmi, R = boot_object$replics, formula = boot_object$formula) )), 
    stype = "i", 
    strata = sapply( (1:sample_size), function(x) 1 ), 
    weights = sapply( (1:sample_size), function(x) 1/sample_size )
  )
  return( boot_ci)
}
