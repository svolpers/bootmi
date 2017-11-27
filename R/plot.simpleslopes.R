#' @title plots simple slopes
#' @description
#' Opens a new graphical window and plots simple slopes
#' @param x An object of type \code{simpleslopes} or 
#' \code{simpleslopes.bootmi}
#' @param ... any default arguments
#' @return \code{NULL}
#' @name plot
#' @author Stephan Volpers \email{stephan.volpers@@plixed.de}
#' @export

plot.simpleslopes <- function( x, ... ) {
  # extract information from slopes object
  mod_vals = round( x$info$Values_of_Moderator, 2)
  xv = x$plot$x # values on x-axis
  yv = x$plot$y # values on y-axis
  
  # create title and filename
  title = paste0( x$info$Y, " ~ ", x$info$X, " * ", x$info$M, "(+ ivs)")
  
  # open new graphical window
  grDevices::dev.new(); graphics::par( mar= c(5,4,4,9))
  # plot start and end points of slopes
  graphics::plot( xv, yv, main= title, xlab= x$info$X, ylab= x$info$Y, 
    ylim= c((min(yv)),(max(yv))), xlim= c((min(xv)),(max(xv))), 
    bty= "L"
  )
  # extract position of starting points 
  i = c( 1:( length( xv)))[c(T,F)]
  # connect corresponding dots by lines
  graphics::segments( xv[i], yv[i], xv[i+1], yv[i+1], lty=1:length(i))
  # add legend
  graphics::legend( x= "topright", legend= mod_vals, inset= c(-.40,0), 
    lty= 1:length(i), cex= .8, ncol= 1, title= "Moderator Values", xpd= TRUE)
}

#' @rdname plot
#' @export
plot.simpleslopes.bootmi <- function( x, ... ) {
  plot.simpleslopes( x, ...)
}

