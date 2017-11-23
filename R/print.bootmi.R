#' @title Prints a \code{bootmi} object
#' @description
#' Prints a \code{bootmi} object
#' @param object A \code{bootmi} object
#' @return \code{NULL}
#' @rdname print
#' @author Stephan Volpers \email{stephan.volpers@@plixed.de}
#' @export

print.bootmi <- function( bootmi) {
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


#' @title Prints a \code{simpleslopes.bootmi} object
#' @description
#' Prints a \code{simpleslopes.bootmi} object
#' @rdname print
#' @export

print.simpleslopes.bootmi <- function( bootmi.slopes) {
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


#' @title Prints a \code{bootmi.lm} object
#' @description
#' Prints a \code{bootmi.lm} object
#' @rdname print
#' @export

print.bootmi.lm <- function( bootmi.lm) {
  summary.bootmi.lm( bootmi.lm)
}


#' @title Prints a \code{regosi} object
#' @description
#' Prints a \code{regosi} object
#' @rdname print
#' @export

print.regosi <- function( regosi) {
  summary.regosi( regosi)
}


#' @title Prints a \code{simpleslopes} object
#' @description
#' Prints a \code{simpleslopes} object
#' @rdname print
#' @export
#' 
print.simpleslopes <- function( slopes) {
  cat("$original\n"); 
  print(slopes$original)
  cat("$info\n"); 
  print(slopes$info)
  cat("$plot\n"); 
  print(slopes$plot)
}
