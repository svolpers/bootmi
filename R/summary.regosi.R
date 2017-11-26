#' @title Summary of a \code{regosi} object
#' @description
#' Summary of a \code{regosi } object
#' @param object A \code{regosi } object
#' @return \code{NULL}
#' @author Stephan Volpers \email{stephan.volpers@@plixed.de}
#' @export

summary.regosi <- function( regosi, ... ) {
  cat("\n")
  if( regosi$root_term < 0) {
    cat("There are no real values of ", regosi$m_var, " that represent regions of significance.", sep = "")
    if( regosi$a > 0) {
      cat("The slope of ", regosi$x_var, " is statistically significant for ALL values of the moderator ", regosi$m_var, ".", sep = "")
    } else {
      cat("The slope of ", regosi$x_var, " is NEVER statistically significant.", sep = "")
    }
  } else {
    # Boundaries of interval
    crit_mod_val_high = round( (( -regosi$b + sqrt( regosi$root_term) ) / (2*regosi$a)), 3)
    crit_mod_val_low = round( (( -regosi$b - sqrt( regosi$root_term) ) / (2*regosi$a)), 3)
    if(regosi$a > 0) {
      cat("Values of the moderator ", regosi$m_var, " OUTSIDE the interval\n", 
        "]", crit_mod_val_low, ":", crit_mod_val_high, "[\n",
        "cause the slope of (b1 + b2*", regosi$m_var,")*", regosi$x_var, " to be statistically significant.", 
        sep = "")
    } else {
      cat("Values of the moderator ", regosi$m_var, " INSIDE the interval\n", 
        "[", crit_mod_val_low, ":", crit_mod_val_high, "]\n",
        "cause the slope of (b1 + b2*", regosi$m_var,")*", regosi$x_var, " to be statistically significant.", 
        sep = "")
    }
  } 
  cat("\n\n")
}
