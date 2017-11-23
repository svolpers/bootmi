#' @title plots simple slopes
#' @description
#' OPens a new graphical window and plots simple slopes
#' @param slopes An object of type \code{simpleslopes} or 
#' \code{simpleslopes.bootmi}
#' @return \code{NULL}
#' @name plot
#' @author Stephan Volpers \email{stephan.volpers@@plixed.de}
#' @export

plot.simpleslopes <- function( slopes) {
  # extract information from slopes object
  mod_vals = round( slopes$info$Values_of_Moderator, 2)
  x = slopes$plot$x # values on x-axis
  y = slopes$plot$y # values on y-axis
  
  # create title and filename
  title = paste0( slopes$info$Y, " ~ ", slopes$info$X, " * ", slopes$info$M, "(+ ivs)")
  
  # open new graphical window
  x11(); par(mar = c(5,4,4,9))
  # plot start and end points of slopes
  plot( x, y, main=title, xlab=slopes$info$X, ylab=slopes$info$Y, ylim=c((min(y)),(max(y))), xlim=c((min(x)),(max(x))), bty="L"
  )
  # extract position of starting points 
  i = c(1:length(x))[c(T,F)]
  # connect corresponding dots by lines
  segments(x[i],y[i],x[i+1],y[i+1],lty=1:length(i))
  # add legend
  legend( x="topright", legend = mod_vals, inset=c(-.40,0), lty = 1:length(i), cex = .8, ncol = 1, title = "Moderator Values", xpd=TRUE)
}

#' @rdname plot
#' @export
plot.simpleslopes.bootmi <- function( obj) {
  plot.simpleslopes( obj)
}

