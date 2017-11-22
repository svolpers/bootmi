#' ordinary bootstrap for deterministic imputation methods
#'
#' The method is an implementation of bootstrapping imputed data  
#' as proposed by Shao and Sitter (1996). In addition, the method 
#' makes optional use of residual centering as proposed by 
#' Little, Bovaird and Widaman (2006), MICE as provided by 
#' van Buuren and Groothuis-Oudshoorn (2011), and takes into account
#' the transform, then impute procedure as proposed by von Hippel (2009).
#' For faster computation, the parallel-package is implemented.
#'
#' @title bootmi: Bootstrap multiple imputed survey data
#' @param formula A regression formula
#' @param data A data.set
#' @param R Number of bootstraps.
#' @param impute Deterministic imputation method of type 
#' "none", "norm.predict", "pmm", "mean" 
#' @param center_mods TRUE or FALSE
#' @param seed Value for set.seed, default = FALSE
#' @param parallel TRUE or FALSE
#' @param resint TRUE or FALSE
#' @rdname bootmi
#' @author Stephan Volpers <stephan.volpers@plixed.de>
#' @references Buuren, Stef van; Groothuis-Oudshoorn, Karin (2011): mice. 
#' Multivariate Imputation by Chained Equations in R. In: 
#' Journal of Statistical Software 45 (3). DOI: 10.18637/jss.v045.i03.
#' @references Hippel, Paul T. von (2009): How to Impute Interactions, 
#' Squares, and other Transformed Variables. In: Sociological Methodology 
#' 39 (1), S. 265–291. DOI: 10.1111/j.1467-9531.2009.01215.x.
#' @references Little, Todd D.; Bovaird, James A.; Widaman, Keith F. (2006): 
#' On the merits of orthogonalizing powered and product terms: 
#' Implications for modeling interactions among latent variables. 
#' In: Structural Equation Modeling 13 (4), S. 497–519.
#' @references R Core Team (2017). R: A language and environment for 
#' statistical computing. R Foundation for Statistical Computing, 
#' Vienna, Austria. URL https://www.R-project.org/.
#' @references Shao, Jun; Sitter, Randy R. (1996): 
#' Bootstrap for Imputed Survey Data. In: Journal of the 
#' American Statistical Association 91 (435), S. 1278–1288. 
#' DOI: 10.2307/2291746.
#' @export
bootmi <- function( formula, data, R, impute=c("none","norm.predict","pmm","mean"), center_mods=FALSE, seed=FALSE, parallel=FALSE, resint=FALSE) {
    UseMethod("bootmi")
}

#' @export
bootmi.default <- function( formula, data, R, impute=c("none","norm.predict","pmm","mean"), center_mods=FALSE, seed=FALSE, parallel=FALSE, resint=FALSE ) {

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

	# Create residual interactions and impute data
	if( impute != "none" || center_mods != FALSE ) {
		# residual interactions and imputation on original data
		res = resimpcen(frmla=formula, dat=data, res_int=resint, imputation=impute, center_mods=center_mods, bootstraps=FALSE)
		# residual interactions and imputation on bootstraps
		no_cores = parallel::detectCores() - 1
		if( (parallel == TRUE) && (no_cores > 1) ) {
			# Initiate cluster
			cl = parallel::makeCluster(no_cores)
			# Make function availiable in clusters
			invisible( parallel::clusterEvalQ(cl, {
				library(mice) 
				source("lm_res_mi_v4.R")
				source("boot_v004.R") 
			}))
			# Export objects to clusters
			parallel::clusterExport(cl=cl, varlist=c("formula", "impute", "resint", "center_mods"), envir=environment())
			# set seed in each cluster for reproducibility
			parallel::clusterSetRNGStream(cl, seed)
			# create residuals or imputation
			bootstraps = parallel::parLapply(cl, bootstraps, function(x) {
				resimpcen( frmla=formula, dat=x, res_int=resint, imputation=impute, center_mods=center_mods, bootstraps=TRUE)
			})
			# stop parallelization
			parallel::stopCluster(cl)
		} else {
			# set seed for reproducibility
			set.seed(seed)
			# create residuals or imputation
			bootstraps = lapply( bootstraps, function(x) {
				resimpcen( frmla=formula, dat=x, res_int=resint, imputation=impute, center_mods=center_mods, bootstraps=TRUE)
			})
		}
		# correct data and formula with actual values after creating residual interactions 
		formula = res$formula
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