#' @title Summary of a \code{bootmi.lm} object
#' @description
#' Summary of a \code{bootmi} object
#' @param object A \code{bootmi} object
#' @param ... Other summary arguments
#' @return \code{NULL}
#' @rdname summary
#' @author Stephan Volpers \email{stephan.volpers@@plixed.de}
#' @export


summary.bootmi <- function( object, ... ) {
  print( object$output)
  cat(
    (object$cilvl*100), "% "
    , object$citype
    , " bootstrapped confidence intervals calculated with "
    , object$bootfit$R
    ," bootstrap samples.\n"
    , sep = ""
    )
  cat("LLCI = Lower Level Confidence Interval; ULCI = Upper Level Confidence Interval\n")
}

#' @title Summary of a \code{simslop.bootmi} object
#' @description
#' Summary of a \code{simslop.bootmi} object
#' @rdname summary
#' @export
summary.simslop.bootmi <- function( object, ci_type = c("basic", "norm", "stud", "perc", "bca", "all"), ci = 95, ... ) {
  
  # set confidence interval value
  ci = as.integer(ci)
  if(!(ci > 0 && ci < 100)) {
      stop("Please enter a confidence interval of ]0;100[. Only natural numbers are allowed.")
  }
  object$ci = ci/100
  # check for confidence interval type
  object$ci_type = match.arg(ci_type)

  # create results
  output = bootmi_results( object)
  # add colnames and rownames
  colnames(output) <- c("Slope Original", "Bootstrap Bias", "Slope Bias Corrected", "BLLCI", "BULCI")
  
  cat("\nAnalysis of the simple slopes\n")
  cat("Dependent variable:", object$info$Y, "\n")
  cat("Independent variable:", object$info$X, "\n")
  cat("Moderator:", object$info$M, "\n\n")
  cat("Coefficients at values of the moderator:\n")
  print(output)
  cat( "\n", (object$ci*100), "% ", object$ci_type, " bootstrapped confidence intervals calculated with ", object$replics," bootstrap samples.\n", 
    "BLLCI = Bootstrap Lower Level Confidence Interval / BULCI = Bootstrap Upper Level Confidence Interval\n",
    sep = "")
}
