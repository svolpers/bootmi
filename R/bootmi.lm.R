bootmi.lm <-
function( bootmi) {

  if(class(bootmi) != "bootmi") {
    stop("Object of class \"bootmi\" needed.")
  }
  
  # convert formula to string for regression
  frml = as.character( bootmi$formula)
  # extract values of the dependent variable and save in matrix
  val_dep_var = matrix( bootmi$data[[frml[[2]]]], ncol = 1)
  rownames(val_dep_var) = rownames(bootmi$data)
  colnames(val_dep_var) = frml[[2]]
  # create formula
  frml = paste0( frml[[2]], frml[[1]], frml[[3]])

  # regress model on original data
  lm_org_data = lm( frml, bootmi$data )
  # create final data used in the model
  bootmi$data = merge( as.data.frame( model.matrix( lm_org_data)), as.data.frame( val_dep_var), by=0, all.x = TRUE)
  rownames( bootmi$data) = as.numeric( bootmi$data[["Row.names"]])

  # regress model on bootstrap samples
  no_cores <- parallel::detectCores() - 1
  if( (bootmi$parallel == TRUE) && (no_cores > 1) ) {
    # Initiate cluster
    cl <- parallel::makeCluster(no_cores)
    # Export objects to clusters
    parallel::clusterExport(cl=cl, varlist=c("frml"), envir=environment())
    # regression
    boot_lm <- parallel::parLapply(cl, bootmi$bootstraps, function(x) lm( frml, x )$coef )
    # stop parallel
    parallel::stopCluster(cl)
  } else {
    boot_lm <- lapply( bootmi$bootstraps, function(x) lm( frml, x)$coef )
  }
  # from list to table
  bootstrap_coeff = do.call( rbind, boot_lm)

  # save results into list
  object = list( formula=bootmi$formula, original=lm_org_data, bootstraps=bootstrap_coeff, data=bootmi$data[,c(-1,-2)], replics=bootmi$replics, center_mods=bootmi$center_mods, seed=bootmi$seed)
  class(object) = "bootmi.lm"
  return(object)
}
