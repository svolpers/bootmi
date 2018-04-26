#' @title Summary of a \code{simslop} object
#' @description
#' Summary of a \code{simslop} object
#' @param object A \code{simslop} object
#' @param ... any default statement
#' @return \code{NULL}
#' @author Stephan Volpers \email{stephan.volpers@@plixed.de}
#' @export

summary.simslop <- function( object, ... ) {
  cat("\nAnalysis of the simple slopes\n")
  cat("Dependent variable:", object$info$Y, "\n")
  cat("Independent variable:", object$info$X, "\n")
  cat("Moderator:", object$info$M, "\n\n")
  cat("coefficients at values of the moderator:\n")
  matr = t( sapply( object$original, function(x) { return( round(x, 4)) }))
  rownames(matr) = as.numeric( matr[,1])
  print( matr[, c(-1)])
  cat( "\n", ( object$info$Confidence_Interval ), "% Confidence Intervals\nLLCI = Lower Level Confidence Interval / ULCI = Upper Level Confidence Interval\n",
    sep = "")
}
