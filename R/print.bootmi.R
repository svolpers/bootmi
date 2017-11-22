print.bootmi <-
function(bootmi) {
  cat("$formula\n"); 
  print(bootmi$formula)
  cat("\nOverview of data values\n"); 
  print( summary(bootmi$data))
  cat("\n$data (head of original data)\n"); 
  print( head(bootmi$data))
  cat("\n$bootstraps[[1]] (head of first bootstrap sample)\n"); 
  print( head(bootmi$bootstraps[[1]]))
  cat("\n$bootstraps[[2]] (head of second bootstrap sample)\n"); 
  print( head(bootmi$bootstraps[[2]]))
  cat("\n$bootstraps[[3]] (head of third bootstrap sample)\n"); 
  print( head(bootmi$bootstraps[[3]]))
  cat("\n$replics\n", bootmi$replics, "\n")
  cat("\n$imputation\n", bootmi$imputation, "\n")
  cat("\n$seed\n", bootmi$seed, "\n")
  cat("\n$parallel\n", bootmi$parallel, "\n")
}
