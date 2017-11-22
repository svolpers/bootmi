simpleslopes <-
function( lm_object, x_var, m_var, ci=95, mod_values_type=c("sd","val"), mod_values=c(-1,0,1), centered=FALSE) {
    UseMethod("simpleslopes")
}
