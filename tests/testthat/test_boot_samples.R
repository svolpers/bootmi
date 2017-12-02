
context("Bootstrap samples")

test_that("data is matrix or data.frame", {
	expect_error( boot_samples( "a"), "Matrix or data.frame needed.")
})

test_that("number of samples equals R", {
	# create data
	data = as.data.frame( matrix( 1:12, nrow = 4))
	# tests
	expect_equal( length( boot_samples( data, -2)), 4)
	expect_equal( length( boot_samples( data, 0)), 2)
	expect_equal( length( boot_samples( data, 1)), 1)
	expect_equal( length( boot_samples( data, 4)), 4)
})

test_that("sample size equals original data", {
	# create data
	data = as.data.frame( matrix( 40:79, nrow = 10))
	# tests
	expect_equal( nrow( boot_samples( data, 1)[[1]]), 10)
	expect_equal( nrow( boot_samples( data[2:5,], 1)[[1]]), 4)
})
