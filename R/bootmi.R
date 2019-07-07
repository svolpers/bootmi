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
bootmi <- function( 
	formula
	, data
	, R = 5000
	, imputationMethod = c( "none",  "norm.predict", "pmm", "mean")
	, cilvl = 0.95
	, citype = c( "norm", "basic", "perc", "bca")
	, residualinteractions = FALSE
	, centerinteractions = FALSE
	, simslopinfo = NULL
	, seed = FALSE
	, parallel = FALSE  
	) { UseMethod("bootmi") }

#' @rdname bootmi
#' @export
bootmi.default <- function( 
	formula
	, data
	, R = 5000
	, imputationMethod = c( "none", "norm.predict", "pmm", "mean")
	, cilvl = 0.95
	, citype = c( "norm", "basic", "perc", "bca")
	, residualinteractions = FALSE
	, centerinteractions = FALSE
	, simslopinfo = NULL
	, seed = FALSE
	, parallel = FALSE 
	) {

	# check for data type
	if(!is.data.frame(data)) {
		stop("Data must be a data.frame")
	}


	#
	# EXTRACT DATA NEEDED FUNCTION
	#

	# find varnames including any alphanumeric 
	# - and . character
	vars = regmatches(
		formula
		, gregexpr("[\\w\\-\\.\\w]*"
			, formula
			, perl = TRUE
			, ignore.case = TRUE
			)
		)

	# extract cols with matching varnames
	# excluding duplicates and empty strings
	data = data[ unique(vars[[1]][vars[[1]] != ""])]


	# check for availiable imputationMethod 
	imputationMethod <- match.arg( imputationMethod)
	# check for availiable ci type 
	citype <- match.arg( citype) 

	# make replics an integer
	R <- as.integer(R)

	# initiate parallel computation
	if( parallel != FALSE ) {
		no_cores <- parallel::detectCores() - 1
		if( Sys.info()["sysname"] == "Windows" ) {
			paralleltype <- "snow"
			# Initiate cluster
			clusters <- parallel::makeCluster(no_cores)
			# Make function availiable in clusters
			invisible( parallel::clusterEvalQ(clusters, {
				library(mice) 
				# library(bootmi)
				devtools::load_all()
			}))
			# Export objects to clusters
			parallel::clusterExport(
				cl = clusters
				, varlist = c("formula"
					, "imputationMethod"
					, "residualinteractions"
					, "centerinteractions"
					, "simslopinfo"
					, "simslopinfo"
					, "data"
					)
				, envir = environment()
				)
			# set seed in each cluster for reproducibility
			parallel::clusterSetRNGStream(clusters, seed)
		} else {
			paralleltype <- "multicore"
			clusters <- NULL
		}
	} else {
		paralleltype <- "no"
		no_cores <- 1
		clusters <- NULL
	}


	# set seed to enable replication
	if( seed != FALSE ) {
		seed <- as.integer(seed)
		set.seed(seed) 
	}

	# do bootstrap function
	boot_fit <- boot::boot( 
    data = data 
    , statistic = calc_bootmi
    , R = R
    , frmla = formula
    , imputationMethod = imputationMethod
    # , glm_family = glm_family
    , res_int = residualinteractions
		, center_mods = centerinteractions
		, simslopinfo = simslopinfo
    , parallel = paralleltype
    , ncpus = no_cores
    , cl = clusters
  )	

	if( parallel != FALSE & Sys.info()["sysname"] == "Windows" ) {
		parallel::stopCluster( clusters)
	}

	# calculate confidence intervals
	cis <- vapply(
		1:length(boot_fit$t0)
		, function( x, boot_out, ci, citype) {
			# switch between types of confidence intervals
			switch( citype,
         bca = boot::boot.ci( 
									boot_out
									, index = x
									, conf = ci
									, type = "bca"
								)$bca[4:5]
         , basic = boot::boot.ci( 
									boot_out
									, index = x
									, conf = ci
									, type = "basic"
								)$basic[4:5]
         , norm = boot::boot.ci( 
									boot_out
									, index = x
									, conf = ci
									, type = "norm"
								)$normal[2:3]
         , perc = boot::boot.ci( 
									boot_out
									, index = x
									, conf = ci
									, type = "perc"
								)$perc[4:5]
       )
		}
		, vector("double", length = 2)
		, boot_out = boot_fit
		, ci = cilvl
		, citype = citype
		)   
	rownames(cis) <- c("LLCI","ULCI")

	# add stars if zero not in CIs
	stars <- apply( t(cis), 1, function(x) {
		if( (x["LLCI"] * x["ULCI"]) > 0 ) 
			"*"
		else
			" "
		})
	# create output table
	output <- cbind(
		numformat( boot_fit$t0, 2)
		, numformat( 
			eval( colMeans(boot_fit$t) - boot_fit$t0)
			, 2
			)
		, numformat( 
			apply( boot_fit$t, 2, sd)
			, 2
			)
		, numformat( t(cis)[,1], 3)
		, numformat( t(cis)[,2], 3)
		, stars
		)
	rownames(output) <- names( boot_fit$t0)
	colnames(output) <- c( "original", "bias", "std. error"
		, "LLCI", "ULCI", "")
	
	# create return object
	object <- list(
		output = output
		, bootfit = boot_fit
		, imputationMethod = imputationMethod
		, cilvl = cilvl
		, citype = citype
		)

	# create class
	class(object) <- "bootmi"
	# return class
	return(object)
}