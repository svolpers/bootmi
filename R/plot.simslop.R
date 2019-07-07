#' @title plots simple slopes
#' @description
#' Opens a new graphical window and plots simple slopes. 
#' CAUTION: 
#' No more than 10 different values of the moderator allowed. 
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
  
  yv = c( rbind( x$simple_slopes$y_l, x$simple_slopes$y_h ))
  xv = rep( c( x$info$x_low, x$info$x_high), eval(length(yv)/2)) # values on x-axis

  if( length(mod_vals) > 10 ) {
    stop("No more than 10 different moderator values allowed for plotting.")
  }

  # create title and filename
  title = paste0( x$info$Y, " ~ ", x$info$X, " * ", x$info$M)
  if( length( x$info$M2 ) > 0 ) {
    title = paste0( title, " * ", x$info$M2)
  }
  # extract position of starting points 
  i = c( 1:( length( xv)))[c(T,F)]
  # if more than six moderator values, use colors 
  linecolors = rep( 
    c("grey0","grey40","grey80","red","blue","cyan")
    , times= 1
    , length.out= NA
    , each= length(mod_vals)
  )

  # open new graphical window
  graphics::par( mar= c(5,4,4,9))
  # plot start and end points of slopes
  graphics::plot( xv, yv
    , main= title
    , xlab= x$info$X
    , ylab= x$info$Y
    , ylim= c((min(yv)),(max(yv)))
    , xlim= c((min(xv)),(max(xv)))
    , bty= "L"
    , type = "n"
  )
  # connect corresponding dots by lines
  graphics::segments( 
    xv[i]
    , yv[i]
    , xv[i+1]
    , yv[i+1]
    , lty= 1:length(mod_vals)
    , lwd= 5
    , col= linecolors
    )

  # add legend
  if( length( x$info$M2 ) > 0 ) {
    mods = apply( 
      expand.grid( mod_vals, mod_vals)
      , 1 
      , paste 
      , collapse = " " 
    )
    legend_title = paste( "Moderator Values of \n", x$info$M2, x$info$M)
  } else {
    mods = mod_vals
    legend_title = paste( "Moderator Values of \n", x$info$M)
  }

  graphics::legend( 
    x= "topright"
    # , legend= mod_vals
    , legend= mods
    , inset= c(-.3,0)
    , lty= 1:length(mod_vals)
    , lwd= 3
    , col= linecolors
    , cex= .8
    , ncol= 1
    , title= legend_title
    , xpd= TRUE
    , seg.len= 5
  )

}

#' @rdname plot
#' @export
plot.simslop.bootmi <- function( x, ... ) {
  plot.simslop( x, ...)
}

