summary.simpleslopes.bootmi <-
function( bootmi.slopes, ci_type = c("basic", "norm", "stud", "perc", "bca", "all"), ci = 95) {
  
  # set confidence interval value
  ci = as.integer(ci)
  if(!(ci > 0 && ci < 100)) {
      stop("Please enter a confidence interval of ]0;100[. Only natural numbers are allowed.")
  }
  bootmi.slopes$ci = ci/100
  # check for confidence interval type
  bootmi.slopes$ci_type = match.arg(ci_type)

  # create results
  output = bootmi_results( bootmi.slopes)
  # add colnames and rownames
  colnames(output) <- c("Slope Original", "Bias", "Slope Bias Corrected", "BLLCI", "BULCI")
  
  cat("\nAnalysis of the simple slopes\n")
  cat("Dependent variable:", bootmi.slopes$info$Y, "\n")
  cat("Independent variable:", bootmi.slopes$info$X, "\n")
  cat("Moderator:", bootmi.slopes$info$M, "\n\n")
  cat("Coefficients at values of the moderator:\n")
  print(output)
  cat( "\n", (bootmi.slopes$ci*100), "% ", bootmi.slopes$ci_type, " bootstrapped confidence intervals calculated with ", bootmi.slopes$replics," bootstrap samples.\n", 
    "BLLCI = Bootstrap Lower Level Confidence Interval / BULCI = Bootstrap Upper Level Confidence Interval\n",
    sep = "")
}
