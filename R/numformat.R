#' @title Round to decimal points
#' @description
#' Function to round to x decimal points including trailing zero
#' @param val numeric value
#' @param decpoints Round to how many decimal points
#' @return \code{NULL}
#' @author Stephan Volpers \email{stephan.volpers@@plixed.de}
#' @export


numformat <- function(val, decpoints= 2) {
  decpoints = paste0("%.",decpoints,"f")
  sub("^(-?)0.", "\\1.", sprintf( decpoints, val))
}
