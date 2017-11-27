#' @title Summary of a \code{regosi} object
#' @description
#' Summary of a \code{regosi } object
#' @param object A \code{regosi } object
#' @param ... any default statement
#' @return \code{NULL}
#' @author Stephan Volpers \email{stephan.volpers@@plixed.de}
#' @export

summary.regosi <- function( object, ... ) {
  cat("\n")
  if( object$root_term < 0) {
    cat("There are no real values of ", object$m_var, " that represent regions of significance.", sep = "")
    if( object$a > 0) {
      cat("The slope of ", object$x_var, " is statistically significant for ALL values of the moderator ", object$m_var, ".", sep = "")
    } else {
      cat("The slope of ", object$x_var, " is NEVER statistically significant.", sep = "")
    }
  } else {
    # Boundaries of interval
    crit_mod_val_high = round( (( -object$b + sqrt( object$root_term) ) / (2*object$a)), 3)
    crit_mod_val_low = round( (( -object$b - sqrt( object$root_term) ) / (2*object$a)), 3)
    if(object$a > 0) {
      cat("Values of the moderator ", object$m_var, " OUTSIDE the interval\n", 
        "]", crit_mod_val_low, ":", crit_mod_val_high, "[\n",
        "cause the slope of (b1 + b2*", object$m_var,")*", object$x_var, " to be statistically significant.", 
        sep = "")
    } else {
      cat("Values of the moderator ", object$m_var, " INSIDE the interval\n", 
        "[", crit_mod_val_low, ":", crit_mod_val_high, "]\n",
        "cause the slope of (b1 + b2*", object$m_var,")*", object$x_var, " to be statistically significant.", 
        sep = "")
    }
  } 
  cat("\n\n")
}
