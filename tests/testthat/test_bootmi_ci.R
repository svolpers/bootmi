context("Bootmi CI Helper")

dat = as.data.frame( matrix( c(2.17,1.67,2.5,5.5,2.67,5.5,6,6.67,6,4,1,6.2,6.2,4,6.2,5,5.5,3.75,6.25,5.5), ncol = 4, byrow = FALSE))
colnames(dat) = c("X","M1","M2","Y")
coeff = coef( lm( "Y ~ X + M1 + M2", data = dat))
bootstr = as.table(coeff)
bootstr = rbind( bootstr, coeff, coeff, coeff, coeff )

# create boot object
original = list(coef= coeff)
boot_object = list(
	data= dat,
	seed= 12345,
	original= original,
	bootstraps= bootstr,
	replics= 5,
	formula= 10
	)
class( boot_object) = "bootmi.lm"

test_that(" uses and returns correct tyes", {
	class( boot_object) = "bootmi"
	expect_error( bootmi_ci( boot_object), "Object of class bootmi.lm or simpleslopes.bootmi required.")
	class( boot_object) = "bootmi.lm"
	expect_equal( typeof( bootmi_ci( boot_object)), typeof( original))
	class( boot_object) = "simpleslopes.bootmi"
	expect_equal( typeof( bootmi_ci( boot_object)), typeof( original))
	expect_equal( typeof( bootmi_ci( boot_object)$call), typeof( call("round", 10.5)))
	expect_equal( typeof( bootmi_ci( boot_object)$t0), typeof( coeff))
	expect_equal( typeof( bootmi_ci( boot_object)$t), typeof( bootstr))
})

test_that(" values are correctly assigned", {
	class( boot_object) = "bootmi.lm"
	expect_equal( bootmi_ci( boot_object)$R, 5)
	expect_match( bootmi_ci( boot_object)$sim, "ordinary")
	expect_match( bootmi_ci( boot_object)$stype, "i")
	expect_equal( length( bootmi_ci( boot_object)$strata), 5)
	expect_equal( bootmi_ci( boot_object)$strata[[1]], 1)
	expect_equal( length( bootmi_ci( boot_object)$weights), 5)
	expect_equal( bootmi_ci( boot_object)$weights[[1]], 0.2)
	expect_equal( bootmi_ci( boot_object)$data[3,2], 6.67)
	expect_equal( bootmi_ci( boot_object)$t0[[2]], 0.35399079)
	expect_equal( bootmi_ci( boot_object)$t[1,2], 0.3539908)
	expect_equal( bootmi_ci( boot_object)$t[1,4], -0.03054181)

})
