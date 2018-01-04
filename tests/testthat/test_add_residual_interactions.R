context("Add Residual Interactions")

# create data
set.seed(1)
dat = as.data.frame( matrix( rnorm(80, mean = 4, sd = 2), ncol = 4, byrow = FALSE))
colnames(dat) = c("X","M1","M2","Y")
dat[11,2] = NA; dat[3,3] = NA

test_that("amount of interaction terms is correct.", {
	expect_equal( ncol( add_residual_interactions( "Y ~ X + M1 + X:M1 + M2", dat)$data), eval(4+1))
	expect_equal( ncol( add_residual_interactions( "Y ~ M1*X + X:M2 + M2", dat)$data), eval(4+2))
	expect_equal( ncol( add_residual_interactions( "Y ~ X*M1*M2", dat)$data), eval(4+4))
})

test_that("value of interaction terms are correct.", {
	expect_equal( add_residual_interactions( "Y ~ X + M1 + X:M1 + M2", dat)$data[13,4], -1.68266805)
	expect_equal( add_residual_interactions( "Y ~ X + M1 + X:M1 + M2", dat)$data[3,2], 4.14912997)
	expect_equal( add_residual_interactions( "Y ~ X + M1 + X:M1 + M2", dat)$data[5,5], 2.5134536)
	expect_equal( add_residual_interactions( "Y ~ X*M1*M2", dat)$data[13,4], -1.68266805)
	expect_equal( add_residual_interactions( "Y ~ X*M1*M2", dat)$data[3,5], as.double(NA))
	expect_equal( add_residual_interactions( "Y ~ X*M1*M2", dat)$data[13,5], -3.1162061)
	expect_equal( add_residual_interactions( "Y ~ X*M1*M2", dat)$data[18,6], 0.148428521)
	expect_equal( add_residual_interactions( "Y ~ X*M1*M2", dat)$data[4,7], 0.88848298)
	expect_equal( add_residual_interactions( "Y ~ X*M1*M2", dat)$data[5,8], 2.5134536)

})

test_that("formula is correct.", {
	expect_equal( add_residual_interactions( "Y ~ X + M1 + X:M1 + M2", dat)$formula, as.formula( "Y ~ X + M1 + M2 + X.RX.M1"))
	expect_equal( add_residual_interactions( "Y ~ M1*X + X:M2 + M2", dat)$formula, as.formula( "Y ~ M1 + X + M2 + M1.RX.X + X.RX.M2"))
	expect_equal( add_residual_interactions( "Y ~ X*M1*M2", dat)$formula, as.formula( "Y ~ X + M1 + M2 + X.RX.M1 + X.RX.M2 + M1.RX.M2 + X.RX.M1.RX.M2"))
})
