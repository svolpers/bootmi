#' @title Prints a \code{bootmi} object
#' @description
#' Prints a \code{bootmi} object
#' @param x A \code{bootmi} object
#' @param ... Other print arguments
#' @return \code{NULL}
#' @rdname print
#' @author Stephan Volpers \email{stephan.volpers@@plixed.de}
#' @export

print.bootmi <- function( x, ... ) {
  cat("$formula\n"); 
  print(x$formula)
  cat("\nOverview of data values\n"); 
  print( summary(x$data))
  cat("\n$data (head of original data)\n"); 
  print( utils::head(x$data))
  cat("\n$bootstraps[[1]] (head of first bootstrap sample)\n"); 
  print( utils::head(x$bootstraps[[1]]))
  cat("\n$bootstraps[[2]] (head of second bootstrap sample)\n"); 
  print( utils::head(x$bootstraps[[2]]))
  cat("\n$bootstraps[[3]] (head of third bootstrap sample)\n"); 
  print( utils::head(x$bootstraps[[3]]))
  cat("\n$replics\n", x$replics, "\n")
  cat("\n$imputation\n", x$imputation, "\n")
  cat("\n$seed\n", x$seed, "\n")
  cat("\n$parallel\n", x$parallel, "\n")
}


#' @title Prints a \code{simpleslopes.bootmi} object
#' @description
#' Prints a \code{simpleslopes.bootmi} object
#' @rdname print
#' @export

print.simpleslopes.bootmi <- function( x, ... ) {
  cat("$original\n"); 
  print(x$original)
  cat("$info\n"); 
  print(x$info)
  cat("$plot\n"); 
  print(x$plot)
  cat("$formula\n"); 
  print(x$formula)
  cat("\n$data (head of original data)\n"); 
  print( utils::head(x$data))
  cat("\n$replics\n", x$replics, "\n")
  cat("\n$bootstraps (head of bootstrap coefficients)\n"); 
  print( utils::head(x$bootstraps))
}


#' @title Prints a \code{bootmi.lm} object
#' @description
#' Prints a \code{bootmi.lm} object
#' @rdname print
#' @export

print.bootmi.lm <- function( x, ... ) {
  summary.bootmi.lm( x)
}


#' @title Prints a \code{regosi} object
#' @description
#' Prints a \code{regosi} object
#' @rdname print
#' @export

print.regosi <- function( x, ... ) {
  summary.regosi( x)
}


#' @title Prints a \code{simpleslopes} object
#' @description
#' Prints a \code{simpleslopes} object
#' @rdname print
#' @export
#' 
print.simpleslopes <- function( x, ... ) {
  cat("$original\n"); 
  print(x$original)
  cat("$info\n"); 
  print(x$info)
  cat("$plot\n"); 
  print(x$plot)
}
