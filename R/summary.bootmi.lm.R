summary.bootmi.lm <-
function( bootmi.lm, ci_type = c("basic", "norm", "stud", "perc", "bca", "all"), ci = 95) {
  # set confidence interval value
  ci = as.integer(ci)
  if(!(ci > 0 && ci < 100)) {
      stop("Please enter a confidence interval of ]0;100[. Only natural numbers are allowed.")
  }
  bootmi.lm$ci = ci/100
  # check for confidence interval type
  bootmi.lm$ci_type = match.arg(ci_type)
  # get summary of original regression model
  summary_model = summary.lm( bootmi.lm$original)
  # replace coefficients with bootstraped result
  summary_model$coefficients = bootmi_results( bootmi.lm)
  # insert original formula
  frml = as.character( bootmi.lm$formula)
  frml = paste0( frml[[2]], frml[[1]], frml[[3]])
  summary_model$call = paste("lm(formula=", frml, ", data= bootmi$data")
  # print summary with additional information
  print( summary_model)
  cat((bootmi.lm$ci*100), "% ", bootmi.lm$ci_type, " bootstrapped confidence intervals calculated with ", bootmi.lm$replics," bootstrap samples.\n", sep = "")
  cat("BLLCI = Bootstrap Lower Level Confidence Interval / BULCI = Bootstrap Upper Level Confidence Interval\n")
}
