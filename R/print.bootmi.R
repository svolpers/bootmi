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
  cat("$output\n"); 
  print(x$output)
  cat("\n$bootfit\n")
  print( str( x$bootfit))
  cat("\n$imputationMethod\n")
  print(x$imputationMethod)
  cat("\n$cilvl\n")
  print(x$cilvl)
  cat("\n$citype\n")
  print(x$citype)
}


#' @title Prints a \code{simslop.bootmi} object
#' @description
#' Prints a \code{simslop.bootmi} object
#' @rdname print
#' @export

print.simslop.bootmi <- function( x, ... ) {
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


#' @title Prints a \code{simslop} object
#' @description
#' Prints a \code{simslop} object
#' @rdname print
#' @export
#' 
print.simslop <- function( x, ... ) {
  cat("$simple_slopes\n"); 
  print(x$simple_slopes)
  cat("$info\n"); 
  print(x$info)
}
