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
	, imputationMethod = c("none","norm.predict","pmm","mean")
	, resint = FALSE
	, center_mods = FALSE
	, seed = FALSE
	, parallel = FALSE 
	) { UseMethod("bootmi") }

#' @rdname bootmi
#' @export
bootmi.default <- function( 
	formula
	, data
	, R = 5000
	, imputationMethod = c("none","norm.predict","pmm","mean")
	, residualinteractions = FALSE
	, centerinteractions = FALSE
	, seed = FALSE
	, parallel = FALSE 
	, glm_family = "gaussian" 
	, simslopinfo = FALSE
	) {

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
	# initiate parallel computation
	if( parallel != FALSE ) {
		no_cores = parallel::detectCores() - 1
		if( Sys.info()["sysname"] == "Windows" ) {
			paralleltype = "snow"
		} else {
			paralleltype = "multicore"
		}
	} else {
		paralleltype = "no"
	}
	# check for availiable imputationMethod 
	imputationMethod = match.arg( imputationMethod) 

	# do bootstrap function
	boot_fit = boot::boot( 
    data = data 
    , statistic = calc_bootmi
    , R = R
    # other named arguments for statistic 
    , frmla = formula
    , imputationMethod = imputationMethod
    , glm_family = glm_family
    , res_int = residualinteractions
		, center_mods = centerinteractions
		, simslopinfo = simslopinfo
    # parallel features
    , parallel = paralleltype
    , ncpus = no_cores

    # , imputationMethodMed= imputationMethodMed
    # , predictorMatrixMed= predictorMatrixMed
    # , frmlMed= frmlMed
    # , imputationMethodOut= impM
    # , predictorMatrixOut= predM2
    # , frmlOut= out.full.frml
    # , interaction_formula= interaction_formula
    # , medmodnames= medmodnames
    # , modvals= modvals
  )

  # str(boot_pmm)

  # if( citype == "bca" ) {
  #   cival = boot::boot.ci( boot_pmm, index = 1, conf = 0.95, type= "bca")
  #   cival = cival$bca[c(4:5)]
  # } else {
  #   # cival = boot::boot.ci( boot_pmm, index = 1, conf = 0.95, type= "basic")$basic[c(4:5)]
  #   cival = boot::boot.ci( boot_pmm, index = 1, conf = 0.95, type= "basic")
  #   cival = cival$basic[c(4:5)]
  # }

  # return( list(
  #   ind_effect= boot_pmm$t0
  #   , ci = cival
  #   ))



	object <- list(
		bootfit = boot_fit
		, imputationMethod = imputationMethod
		)
	# create class
	class(object) = "bootmi"
	# return class
	return(object)
}