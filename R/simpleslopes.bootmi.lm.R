simpleslopes.bootmi.lm <-
function( bootmi_lm_obj, x_var, m_var, mod_values_type = "sd", mod_values = c(-1,0,1)) {

  if( class(bootmi_lm_obj) != "bootmi.lm" ) {
      stop("Object of class \"bootmi.lm\" needed.")
  }

  # calculate simple solpes for original data set
  si_sl = simpleslopes( lm_object=bootmi_lm_obj$original, x_var=x_var, m_var=m_var, mod_values_type=mod_values_type, mod_values=mod_values, centered=bootmi_lm_obj$center_mods)

  # extract coefficients for later use of boot.ci helper functions
  coeff = extract_coeff( si_sl)
  si_sl$original = list( slopes=si_sl$original, coef=coeff )

  # create empty matrix for bootstrapped coefficients
  bscoef = matrix(0, nrow = bootmi_lm_obj$replics, ncol = length( mod_values))

  # loop through values of the moderator
  for(i in 1:length( mod_values)) {
    # calculate bootstrap coefficients for the moderator
    bootstrcoeff = ( bootmi_lm_obj$bootstraps[ , si_sl$info$X] 
            + bootmi_lm_obj$bootstraps[ , si_sl$info$XM]
            * si_sl$original$slopes[[i]][["m_val_data"]] )
    # insert values in matrix
    bscoef[ ,i] = bootstrcoeff
  }
  # name cols of matrix
  colnames(bscoef) = mod_values

  # create object, name class and return object
  object = append( si_sl, list(bootstraps=bscoef, data=bootmi_lm_obj$data, formula=bootmi_lm_obj$formula, replics=bootmi_lm_obj$replics) )
  class(object) = "simpleslopes.bootmi"
  return( object)
}
