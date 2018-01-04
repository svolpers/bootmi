#' @title plots simple slopes
#' @description
#' Opens a new graphical window and plots simple slopes. 
#' CAUTION: 
#' No more than 30 different values of the moderator allowed. 
#' I recommend a maximum of six to plot.
#' @param x An object of type \code{simslop} or 
#' \code{simslop.bootmi}
#' @param ... any default arguments
#' @return \code{NULL}
#' @name plot
#' @author Stephan Volpers \email{stephan.volpers@@plixed.de}
#' @export

plot.simslop <- function( x, ... ) {
  
  # extract information from slopes object
  mod_vals = round( x$info$Values_of_Moderator, 2)
  xv = x$plot$x # values on x-axis
  yv = x$plot$y # values on y-axis

  if( length(mod_vals) > 30 ) {
    stop("No more than 30 different moderator values allowed for plotting.")
  }

  # create title and filename
  title = paste0( x$info$Y, " ~ ", x$info$X, " * ", x$info$M, "(+ ivs)")
  
  # extract position of starting points 
  i = c( 1:( length( xv)))[c(T,F)]
  # if more than six moderator values, use colors 
  linecolors = rep( c("grey0","grey50","red","blue","cyan"), 
      times= 1, length.out= NA, each= 6)

  # open new graphical window
  graphics::par( mar= c(5,4,4,9))
  # plot start and end points of slopes
  graphics::plot( xv, yv, main= title, xlab= x$info$X, ylab= x$info$Y, 
    ylim= c((min(yv)),(max(yv))), xlim= c((min(xv)),(max(xv))), 
    bty= "L"
  )
  # connect corresponding dots by lines
  graphics::segments( xv[i], yv[i], xv[i+1], yv[i+1], 
    lty= 1:length(i), col= linecolors)
  # add legend
  graphics::legend( x= "topright", legend= mod_vals, inset= c(-.40,0), 
    lty= 1:length(i), col= linecolors, 
    cex= .8, ncol= 1, title= "Moderator Values", xpd= TRUE)
}

#' @rdname plot
#' @export
plot.simslop.bootmi <- function( x, ... ) {
  plot.simslop( x, ...)
}
