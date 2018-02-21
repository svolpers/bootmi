#' @title linear model for bootstrapped and imputed data
#' @description
#' The method applies \code{lm()} on original and bootstrapped data.
#' @details
#' For faster computation, \pkg{parallel} is implemented.
#' @param bootmi An object of class \code{\link{bootmi}}
#' @param ... any default lm statement
#' @return object of class "bootmi.lm" including
#' \itemize{
#'   \item Regression formula
#'   \item Original data
#'   \item Bootstrapped coefficients
#'   \item Bootstrap samples 
#'   \item Amount of bootstrap replications
#'   \item Moderation centered, TRUE or FALSE
#'   \item Seed value
#' } 
#' @author Stephan Volpers \email{stephan.volpers@@plixed.de}
#' @export
#' 

lm.bootmi <- function( bootmi, ...) {

  call <- match.call()
  
  if( class( bootmi) != "bootmi") {
    stop("Object of class \"bootmi\" required.")
  }
  
  # convert formula to string for regression
  dep_var = as.character( bootmi$formula)[[2]]
  # extract values of the dependent variable and save in matrix
  val_dep_var = matrix( bootmi$data[[dep_var]], ncol = 1)
  rownames(val_dep_var) = rownames(bootmi$data)
  colnames(val_dep_var) = dep_var

  # regress model on original data
  lm_org = lm( bootmi$formula, bootmi$data, ...)
  # create final data used in the model
  bootmi$data = merge( 
    as.data.frame( model.matrix( lm_org))
    , as.data.frame( val_dep_var)
    , by=0
    , all.x = TRUE
    )
  rownames( bootmi$data) = as.numeric( bootmi$data[["Row.names"]])
  # remove Row.names and (Intercept)
  bootmi$data = bootmi$data[grep( "Row.names|\\(Intercept\\)", colnames(bootmi$data), value= TRUE, invert= TRUE)]

  # regress model on bootstrap samples
  no_cores <- parallel::detectCores() - 1
  if( (bootmi$parallel == TRUE) && (no_cores > 1) ) {
    # Initiate cluster
    cl <- parallel::makeCluster(no_cores)
    # Export objects to clusters
    parallel::clusterExport(cl=cl, varlist=c("bootmi$formula"), envir=environment())
    # regression
    boot_lm <- parallel::parLapply(cl, bootmi$bootstraps, function(x) lm( bootmi$formula, x, ...)$coef )
    # stop parallel
    parallel::stopCluster(cl)
  } else {
    boot_lm <- lapply( bootmi$bootstraps, function(x) lm( bootmi$formula, x, ...)$coef )
  }
  # from list to table
  bootstrap_coeff = do.call( rbind, boot_lm)

  # save results into list
  object = list( 
    formula=bootmi$formula
    , original=lm_org
    , bootstraps=bootstrap_coeff
    , data=bootmi$data
    , replics=bootmi$replics
    , center_mods=bootmi$center_mods
    , seed=bootmi$seed
    )
  # class(object) = "bootmi.lm"
  oldClass(object) <- c("bootmi.lm", "lm")
  return(object)
}


#' @title Generalized linear model for bootstrapped and imputed data
#' @description
#' The method applies \code{glm()} on original and bootstrapped data.
#' @details
#' For faster computation, \pkg{parallel} is implemented.
#' @param bootmi An object of class \code{\link{bootmi}}
#' @param family see \code{\link[stats]{glm}}
#' @param ... any default glm statement
#' @return object of class "bootmi.lm" including
#' \itemize{
#'   \item Regression formula
#'   \item Original data
#'   \item Bootstrapped coefficients
#'   \item Bootstrap samples 
#'   \item Amount of bootstrap replications
#'   \item Moderation centered, TRUE or FALSE
#'   \item Seed value
#' } 
#' @author Stephan Volpers \email{stephan.volpers@@plixed.de}
#' @export

glm.bootmi <- function( bootmi, family= gaussian, ...) {
  
  call <- match.call()
  
  if( class( bootmi) != "bootmi") {
    stop("Object of class \"bootmi\" required.")
  }
  
  # convert formula to string for regression
  dep_var = as.character( bootmi$formula)[[2]]
  # extract values of the dependent variable and save in matrix
  val_dep_var = matrix( bootmi$data[[dep_var]], ncol = 1)
  rownames(val_dep_var) = rownames(bootmi$data)
  colnames(val_dep_var) = dep_var

  # regress model on original data
  lm_org = glm( bootmi$formula, family, bootmi$data, ...)
  # create final data used in the model
  bootmi$data = merge( as.data.frame( model.matrix( lm_org)), as.data.frame( val_dep_var), by=0, all.x = TRUE)
  rownames( bootmi$data) = as.numeric( bootmi$data[["Row.names"]])
  # remove Row.names and (Intercept)
  bootmi$data = bootmi$data[grep( "Row.names|\\(Intercept\\)", colnames(bootmi$data), value= TRUE, invert= TRUE)]

  # regress model on bootstrap samples
  no_cores <- parallel::detectCores() - 1
  if( (bootmi$parallel == TRUE) && (no_cores > 1) ) {
    # Initiate cluster
    cl <- parallel::makeCluster(no_cores)
    # Export objects to clusters
    parallel::clusterExport(cl=cl, varlist=c("bootmi$formula"), envir=environment())
    # regression
    boot_glm <- parallel::parLapply(cl, bootmi$bootstraps, function(x) glm( bootmi$formula, family, x, ... )$coef )
    # stop parallel
    parallel::stopCluster(cl)
  } else {
    boot_glm <- lapply( bootmi$bootstraps, function(x) glm( bootmi$formula, family, x, ... )$coef )
  }
  # from list to table
  bootstrap_coeff = do.call( rbind, boot_glm)

  # save results into list
  object = list( 
    formula=bootmi$formula
    , original=lm_org
    , bootstraps=bootstrap_coeff
    , data=bootmi$data
    , replics=bootmi$replics
    , center_mods=bootmi$center_mods
    , seed=bootmi$seed
    )
  # class(object) = "bootmi.lm"
  oldClass(object) <- c("bootmi.lm", "glm", "lm")
  return(object)
}