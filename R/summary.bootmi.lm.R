#' @title Summary of a \code{bootmi.lm} object
#' @description
#' Summary of a \code{bootmi.lm} object
#' @param object A \code{bootmi.lm} object
#' @param ci_type Type of confidence interval, namely 
#' "basic", "norm", "stud", "perc", "bca", or "all" 
#' @param ci Confidence interval, default 95
#' @return \code{NULL}
#' @rdname summary
#' @author Stephan Volpers \email{stephan.volpers@@plixed.de}
#' @export

summary.bootmi.lm <- function( object, ci_type = c("basic", "norm", "stud", "perc", "bca", "all"), ci = 95) {
  # set confidence interval value
  ci = as.integer(ci)
  if(!(ci > 0 && ci < 100)) {
      stop("Please enter a confidence interval of ]0;100[. Only natural numbers are allowed.")
  }
  object$ci = ci/100
  # check for confidence interval type
  object$ci_type = match.arg(ci_type)
  # get summary of original regression model
  summary_model = summary.lm( object$original)
  # replace coefficients with bootstraped result
  summary_model$coefficients = bootmi_results( bootmi.lm)
  # insert original formula
  frml = as.character( object$formula)
  frml = paste0( frml[[2]], frml[[1]], frml[[3]])
  summary_model$call = paste("lm(formula=", frml, ", data= bootmi$data")
  # print summary with additional information
  print( summary_model)
  cat((object$ci*100), "% ", object$ci_type, " bootstrapped confidence intervals calculated with ", object$replics," bootstrap samples.\n", sep = "")
  cat("BLLCI = Bootstrap Lower Level Confidence Interval / BULCI = Bootstrap Upper Level Confidence Interval\n")
}

#' @title Summary of a \code{simpleslopes.bootmi} object
#' @description
#' Summary of a \code{simpleslopes.bootmi} object
#' @rdname summary
#' @param object A \code{simpleslopes.bootmi} object
#' @export
summary.simpleslopes.bootmi <- function( object, ci_type = c("basic", "norm", "stud", "perc", "bca", "all"), ci = 95) {
  
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
  colnames(output) <- c("Slope Original", "Bias", "Slope Bias Corrected", "BLLCI", "BULCI")
  
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
