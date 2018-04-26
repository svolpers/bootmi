#' @title bootmi: Bootstrap Multiple Imputed (survey) data
#' @description
#' The bootmi method generates bootraps samples and imputes 
#' each as proposed by Shao and Sitter (1996). 
#' @param formula A regression formula
#' @param data A data.set
#' @param R Number of bootstraps.
#' @param impute Deterministic imputation method of type 
#' "none", "norm.predict", "pmm", "mean" ;
#' More Details? See \code{\link[mice]{mice}}
#' @param center_mods TRUE or FALSE
#' @param seed Value for set.seed, default = FALSE
#' @param parallel TRUE or FALSE
#' @param resint TRUE or FALSE
#' @return object of class "bootmi" including
#' \itemize{
#'   \item $formula, formula used
#'   \item $data, original data
#'   \item $bootstraps, bootstap data
#'   \item $replics, bootstrap replics
#'   \item $imputation, imputation method
#'   \item $seed, seed value
#'   \item $parallel, use parallel TRUE or FALSE
#'   \item $center_mods, center moderators? TRUE or FALSE
#' } 
#' @name bootmi
#' @author Stephan Volpers \email{stephan.volpers@@plixed.de}
#' @import stats
#' @export
bootmi <- function( formula, data, R= 5000, impute=c("none","norm.predict","pmm","mean"), resint=FALSE, center_mods=FALSE, seed=FALSE, parallel=FALSE ) {
    UseMethod("bootmi")
}

#' @rdname bootmi
#' @export
bootmi.default <- function( formula, data, R= 5000, impute=c("none","norm.predict","pmm","mean"), resint=FALSE, center_mods=FALSE, seed=FALSE, parallel=FALSE ) {

	# integrity checks
	impute = match.arg( impute)
	# check for data type
	if(!is.data.frame(data)) {
		stop("Data must be a data.frame")
	}
	# make replics an integer
	R = as.integer(R)
	# set seed to enable replication
	if( seed != FALSE ) {
		seed = as.integer(seed)
		set.seed(seed) 
	} 
	# Create bootstrap samples
	bootstraps = boot_samples(data, R)

	# do imputation of interactions 	
	if( 
		# check if interactions exist
		( length( grep( ":|\\*", attr( terms( as.formula(formula)), "term.labels"))) > 0 )
		# or user specifications match 
		| impute != "none" | resint != FALSE | center_mods != FALSE
	) {
		# residual interactions and imputation on original data
		res = resimpcen(frmla=formula, data=data, res_int=resint, imputation=impute, center_mods=center_mods, bootstraps=FALSE)
		
		# get number of cores for parallelization
		no_cores = parallel::detectCores() - 1
		# residual interactions and imputation on bootstraps
		if( (parallel == TRUE) && (no_cores > 1) ) {
			# Initiate cluster
			cl = parallel::makeCluster(no_cores)
			# Make function availiable in clusters
			invisible( parallel::clusterEvalQ(cl, {
				library(mice) 
				library(bootmi)
			}))
			# Export objects to clusters
			parallel::clusterExport(cl=cl, varlist=c("formula", "impute", "resint", "center_mods"), envir=environment())
			# set seed in each cluster for reproducibility
			parallel::clusterSetRNGStream(cl, seed)
			# create residuals or imputation
			bootstraps = parallel::parLapply(cl, bootstraps, function(x) {
				resimpcen( frmla=formula, data=x, res_int=resint, imputation=impute, center_mods=center_mods, bootstraps=TRUE)
			})
			# stop parallelization
			parallel::stopCluster(cl)
		} else {
			# set seed for reproducibility
			set.seed(seed)
			# create residuals or imputation
			bootstraps = lapply( bootstraps, function(x) {
				resimpcen( frmla=formula, data=x, res_int=resint, imputation=impute, center_mods=center_mods, bootstraps=TRUE)
			})
		}
		# correct data and formula with actual values after creating residual interactions 
		# if intercept ommitted
		if( TRUE %in% grepl( "-1", formula) ) {
			# add ommit
			f = sub( "~", "~ -1 +", res$formula)
			formula = as.formula( paste( f[[2]], f[[1]], f[[3]]))
		} else {
			formula = res$formula
		}
		data = res$data
	}
	# if residual interaction terms, they are more or less mean centered
	# information needed for potential simple slopes analysis
	if(resint == TRUE) center_mods = TRUE

	object <- list(formula=formula, data=data, bootstraps=bootstraps, replics=R, imputation=impute, seed=seed, parallel=parallel, center_mods=center_mods)
	# create class
	class(object) = "bootmi"
	# return class
	return(object)
}