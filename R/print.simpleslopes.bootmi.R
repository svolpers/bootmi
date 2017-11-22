print.simpleslopes.bootmi <-
function( bootmi.slopes) {
  cat("$original\n"); 
  print(bootmi.slopes$original)
  cat("$info\n"); 
  print(bootmi.slopes$info)
  cat("$plot\n"); 
  print(bootmi.slopes$plot)
  cat("$formula\n"); 
  print(bootmi.slopes$formula)
  cat("\n$data (head of original data)\n"); 
  print( head(bootmi.slopes$data))
  cat("\n$replics\n", bootmi.slopes$replics, "\n")
  cat("\n$bootstraps (head of bootstrap coefficients)\n"); 
  print( head(bootmi.slopes$bootstraps))
}
