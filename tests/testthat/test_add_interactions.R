context("Add Interactions")

# create formula 
# create data
data = as.data.frame( matrix( c(2.17,1.67,2.5,5.5,2.67,5.5,6,6.67,6,NA,1,6.2,6.2,4,6.2,5,5.5,3.75,6.25,5.5), ncol = 4, byrow = FALSE))
colnames(data) = c("X","M1","M2","Y")

test_that("amount of interaction terms is correct.", {
	expect_equal( ncol( add_interactions( "Y~X+M1+X:M1+M2", data)$data), eval(4+1))
	expect_equal( ncol( add_interactions( "Y ~ M1*X + X:M2 + M2", data)$data), eval(4+2))
	expect_equal( ncol( add_interactions( "Y ~ X*M1*M2", data)$data), eval(4+4))
})

test_that("value of interaction terms are correct.", {
	expect_equal( add_interactions( "Y ~ X + M1 + X:M1 + M2", data)$data[1,4], 11.935)
	expect_equal( add_interactions( "Y ~ X + M1 + X:M1 + M2", data)$data[3,4], 16.675)
	expect_equal( add_interactions( "Y ~ X + M1 + X:M1 + M2", data)$data[5,5], 5.5)
	expect_equal( add_interactions( "Y ~ X*M1*M2", data)$data[1,4], 11.935)
	expect_equal( add_interactions( "Y ~ X*M1*M2", data)$data[5,7], as.double(NA))
	expect_equal( add_interactions( "Y ~ X*M1*M2", data)$data[3,7], 103.385)
	expect_equal( add_interactions( "Y ~ X*M1*M2", data)$data[2,6], 37.200)
	expect_equal( add_interactions( "Y ~ X*M1*M2", data)$data[1,8], 5)
})

test_that("formula is correct.", {
	expect_equal( add_interactions( "Y ~ X + M1 + X:M1 + M2", data)$formula, as.formula( "Y ~ X + M1 + M2 + X.XX.M1"))
	expect_equal( add_interactions( "Y ~ M1*X + X:M2 + M2", data)$formula, as.formula( "Y ~ M1 + X + M2 + M1.XX.X + X.XX.M2"))
	expect_equal( add_interactions( "Y ~ X*M1*M2", data)$formula, as.formula( "Y ~ X + M1 + M2 + X.XX.M1 + X.XX.M2 + M1.XX.M2 + X.XX.M1.XX.M2"))
})