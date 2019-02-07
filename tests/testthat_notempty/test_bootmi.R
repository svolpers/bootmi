context("bootmi")

# create data
set.seed(1)
dat = as.data.frame( matrix( 
	rnorm( 80, mean = 4, sd = 2)
	, ncol = 4
	, byrow = FALSE
	))
colnames(dat) = c("X","M1","M2","Y")
dat[11,2] = NA; dat[3,3] = NA
frml= "Y ~ I(X*X) + X*M2"

test_that("amount bootstraps are correct.", {
	expect_equal( 
		length( bootmi( "Y ~ X", dat)$bootstraps)
		, bootmi( "Y ~ X", dat)$replics # =default
		)
	expect_equal( 
		length( bootmi( "Y ~ X", dat, R= 10)$bootstraps)
		, bootmi( "Y ~ X", dat, R= 10)$replics 
		)
	expect_equal( 
		length( bootmi( "Y ~ X", dat, R= 15)$bootstraps)
		, 15 
		)
})

test_that("there are no missing values when imputed.", {
	
	expect_equal( 
		sum( 
			is.na( 
				bootmi( frml, dat, 1, "mean", seed= 1)$data
				)
			)
		, 0
		)
	expect_equal( 
		sum( 
			is.na( 
				bootmi( frml, dat, 1, "pmm", seed= 1)$data
				)
			)
		, 0
		)
	expect_equal( 
		sum( 
			is.na( 
				bootmi( frml, dat, 1, "norm.predict", seed= 1)$data
				)
			)
		, 0
		)
	expect_equal( 
		sum( 
			is.na( 
				bootmi( frml, dat, 1, seed= 1)$data
				)
			)
		, 2
		)
})


test_that("imputations are correct.", {
	expect_equal( 
		signif( bootmi( frml, dat, 1, "mean", seed= 1)$data[3,3], 6)
		, 4.21884
		)
	expect_equal( 
		signif( bootmi( frml, dat, 1, "pmm", seed= 1)$data[3,4], 6)
		, 12.9114
		)
	expect_equal( 
		signif( bootmi( frml, dat, 1, "pmm", seed= 1, resint=TRUE)$data[3,4], 6)
		, 1.0771
		)
	expect_equal( 
		signif( bootmi( frml, dat, 1, "pmm", seed= 2, resint=TRUE)$bootstraps[[1]][2,4], 6)
		, -0.261989
		)
	expect_equal( 
		signif( bootmi( frml, dat, 1, "pmm", seed= 2, resint=FALSE, center_mods=TRUE)$bootstraps[[1]][7,5], 6)
		, -10.6574
		)
	expect_equal( 
		signif( bootmi( frml, dat, 1, "norm.predict", seed= 2, resint=FALSE, center_mods=TRUE)$bootstraps[[1]][7,5], 6)
		, -11.0009
		)
})

test_that("formula is correct.", {
	expect_equal( 
		bootmi( 
			frml
			, dat
			, R= 1
			, resint= TRUE
			, center_mods= TRUE
			)$formula
		, as.formula( "Y ~ X.RX.X + X + M2 + X.RX.M2")
	)
	expect_equal( 
		bootmi( 
			frml
			, dat
			, R= 1
			, resint= FALSE
			, center_mods= TRUE
		)$formula
		, as.formula( "Y ~ X.XX.X + X + M2 + X.XX.M2")
	)
	expect_equal( 
		bootmi( 
			frml
			, dat
			, R= 1
			, resint= FALSE
			, center_mods= FALSE
		)$formula
		, as.formula( "Y ~ X.XX.X + X + M2 + X.XX.M2")
	)
})
