bootmi <-
function( formula, data, R, impute=c("none","norm.predict","pmm","mean"), center_mods=FALSE, seed=FALSE, parallel=FALSE, resint=FALSE) {
    UseMethod("bootmi")
}
