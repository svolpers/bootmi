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
  cat("Moderator:", object$info$M, "\n")
  if( length( object$info$M2 ) > 0 ) {
    cat("Moderator 2:", object$info$M2, "\n")
  }
  cat("\ncoefficients at values of the moderator:\n")
  t = apply( 
    object$simple_slopes[grep( "y_l|y_h|LBCI|UBCI", names(object$simple_slopes), invert = TRUE)]
    , 2
    , function(x) {
      numformat( x)
    })
  t2 = apply( 
    object$simple_slopes[grep( "LBCI|UBCI", names(object$simple_slopes))]
    , 2
    , function(x) {
      numformat( x, 3)
    })
  print( data.frame( cbind( t,t2)))
  cat( "\n", ( object$info$Confidence_Interval ), "% Confidence intervals\nLBCI = Lower bound of confidence interval / UBCI = Upper bound of confidence interval\n",
    sep = "")
}
