#' @title \pkg{bootmi}: BOOTstrap Multiple Imputed (survey) data
#' @description
#' The \pkg{bootmi} package first and foremost implements bootstrapping 
#' imputed (survey) data as proposed by Shao and Sitter (1996). 
#' Furthermore, residual centering as proposed by Little, Bovaird and Widaman 
#' (2006), MICE as provided by van Buuren and Groothuis-Oudshoorn (2011), and 
#' the "transform, then impute procedure" as proposed by von Hippel (2009) are 
#' implemented. 
#' @details	
#' In addition to analysing bootstrapped imputed (survey) data, analysis of 
#' the simple slopes as proposed by Aiken and West (1991), regions of 
#' significance as proposed by Bauer and Curran (2005) and (moderated) 
#' mediation analysis as proposed by Preacher, Rucker, and Hayes (2007) are 
#' also implemented for linear models (lm), multilevel models (lmer) and mice 
#' models (mice). 
#' 
#' For faster computation, \pkg{parallel} is implemented.
#' 
#' @section Functions:
#' 
#' The main functions are:
#' \tabular{ll}{
#'   \code{bootmi()} \tab Bootstrap and impute the missing data *R* times\cr
#'   \code{lm()} \tab Analyze bootmi data\cr
#'   \code{simslop()} \tab Conducts analysis of the simple slopes\cr
#'   \code{regosi()} \tab Calculates regions of significance\cr
#'   \code{mediate()} \tab Tests (moderated) mediation.\cr} (under construction)
#' 
#' @name bootmi
#' @references Bauer, Daniel J.; Curran, Patrick J. (2005): Probing 
#' Interactions in Fixed and Multilevel Regression: Inferential and Graphical 
#' Techniques. In: Multivariate Behavioral Research 40 (3), S. 373-400. 
#' @references Buuren, Stef van; Groothuis-Oudshoorn, Karin (2011): mice. 
#' Multivariate Imputation by Chained Equations in R. In: 
#' Journal of Statistical Software 45 (3). 
#' @references Hippel, Paul T. von (2009): How to Impute Interactions, 
#' Squares, and other Transformed Variables. In: Sociological Methodology 
#' 39 (1), S. 265-291. 
#' @references Little, Todd D.; Bovaird, James A.; Widaman, Keith F. (2006): 
#' On the merits of orthogonalizing powered and product terms: 
#' Implications for modeling interactions among latent variables. 
#' In: Structural Equation Modeling 13 (4), S. 497-519.
#' @references Preacher, Kristopher J.; Rucker, Derek D.; Hayes, Andrew F. 
#' (2007): Addressing Moderated Mediation Hypotheses: Theory, Methods, and 
#' Prescriptions. In: Multivariate Behavioral Research 42 (1), S. 185-227.
#' @references Shao, Jun; Sitter, Randy R. (1996): 
#' Bootstrap for Imputed Survey Data. In: Journal of the 
#' American Statistical Association 91 (435), S. 1278-1288. 
#' 
NULL