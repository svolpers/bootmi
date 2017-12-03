context("Add Residual Interactions")

# create formula 
# create data
data = as.data.frame( matrix( c(2.17,1.67,2.5,5.5,2.67,5.5,6,6.67,6,NA,1,6.2,6.2,4,6.2,5,5.5,3.75,6.25,5.5), ncol = 4, byrow = FALSE))
colnames(data) = c("X","M1","M2","Y")

test_that("amount of interaction terms is correct.", {
	expect_equal( ncol( add_residual_interactions( "Y~X+M1+X:M1+M2", data)$data), eval(4+1))
	expect_equal( ncol( add_residual_interactions( "Y ~ M1*X + X:M2 + M2", data)$data), eval(4+2))
	expect_equal( ncol( add_residual_interactions( "Y ~ X*M1*M2", data)$data), eval(4+4))
})

test_that("value of interaction terms are correct.", {
	expect_equal( add_residual_interactions( "Y ~ X + M1 + X:M1 + M2", data)$data[1,4], 0.0439245)
	expect_equal( add_residual_interactions( "Y ~ X + M1 + X:M1 + M2", data)$data[3,4], 0.03277948)
	expect_equal( add_residual_interactions( "Y ~ X + M1 + X:M1 + M2", data)$data[5,5], 5.5)
	expect_equal( add_residual_interactions( "Y ~ X*M1*M2", data)$data[1,4], 0.0439245)
	expect_equal( add_residual_interactions( "Y ~ X*M1*M2", data)$data[5,7], as.double(NA))
	expect_equal( add_residual_interactions( "Y ~ X*M1*M2", data)$data[3,7], 0)
	expect_equal( add_residual_interactions( "Y ~ X*M1*M2", data)$data[2,6], 0.2133144)
	expect_equal( add_residual_interactions( "Y ~ X*M1*M2", data)$data[1,8], 5)
})

test_that("formula is correct.", {
	expect_equal( add_residual_interactions( "Y ~ X + M1 + X:M1 + M2", data)$formula, as.formula( "Y ~ X + M1 + M2 + X.RX.M1"))
	expect_equal( add_residual_interactions( "Y ~ M1*X + X:M2 + M2", data)$formula, as.formula( "Y ~ M1 + X + M2 + M1.RX.X + X.RX.M2"))
	expect_equal( add_residual_interactions( "Y ~ X*M1*M2", data)$formula, as.formula( "Y ~ X + M1 + M2 + X.RX.M1 + X.RX.M2 + M1.RX.M2 + X.RX.M1.RX.M2"))
})