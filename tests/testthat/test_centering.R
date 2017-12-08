context("Centering")

# create data
dat = as.data.frame( matrix( c(2.17,1.67,2.5,5.5,2.67,5.5,6,6.67,6,NA,1,6.2,6.2,4,6.2,5,5.5,3.75,6.25,5.5), ncol = 4, byrow = FALSE))
colnames(dat) = c("X","M1","M2","Y")


test_that("colnames remain same.", {
	# some vars centered
	c1 = colnames( centering( dat, c("X","M1")))
	c2 = c("X","M1","M2","Y")
	expect_equal( c1[order(c1)], c2[order(c2)])
	# all vars centered
	c1 = colnames( centering( dat, c("X","M1","M2","Y")))
	c2 = c("X","M1","M2","Y")
	c1 = colnames( centering( dat, "all"))
	c2 = c("X","M1","M2","Y")
	expect_equal( c1[order(c1)], c2[order(c2)])
	# no vars centered
	c1 = colnames( centering( dat, c()))
	c2 = c("X","M1","M2","Y")
	expect_equal( c1[order(c1)], c2[order(c2)])
})

test_that("size of data frame is correct.", {
	expect_equal( ncol( centering( dat, c("X","M1"))), 4)
	expect_equal( ncol( centering( dat, c())), 4)
	expect_equal( ncol( centering( dat, c("X","M1","M2","Y"))), 4)
	expect_equal( ncol( centering( dat, "all")), 4)
})

test_that("values are correct.", {
	expect_equal( centering( dat, c("X","M1"))[1,4], -0.5425)
	expect_equal( centering( dat, c("X","M1"))[3,3], -0.402)
	expect_equal( centering( dat, c("X","M1"))[4,1], 4)
	expect_equal( centering( dat, c())[4,2], 6)
	expect_equal( centering( dat, c("X","M1","M2","Y"))[5,2], as.double(NA))
	expect_equal( centering( dat, c("X","M1","M2","Y"))[2,4], 0.3)
	expect_equal( centering( dat, "all")[2,4], 0.3)
})