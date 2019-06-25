context("resimpcen")

# create data
set.seed(1)
dat = as.data.frame( matrix( rnorm( 80, mean = 4, sd = 2), ncol = 4, byrow = FALSE))
colnames(dat) = c("X","M1","M2","Y")
dat[11,2] = NA; dat[3,3] = NA
frmla= "Y ~ X*M1 + M2"

test_that("values are correct.", {
	set.seed(15432)
	expect_equal( 
		resimpcen( 
			frmla
			, dat
			, res_int= TRUE
			, imputation= "pmm"
			, center_mods= FALSE
			, bootstraps= TRUE
		)[3,3]
		, 7.9607998
		)
	set.seed(15432)
	expect_equal( 
		resimpcen( 
			frmla
			, dat
			, res_int= TRUE
			, imputation= "pmm"
			, center_mods= FALSE
			, bootstraps= TRUE
		)[11,4]
		, 1.56439896
		)
	set.seed(15432)
	expect_equal( 
		resimpcen( 
			frmla
			, dat
			, res_int= FALSE
			, imputation= "pmm"
			, center_mods= TRUE
			, bootstraps= TRUE
		)[11,5]
		, 8.740324801
		)
})

test_that("formula is correct.", {
	expect_equal( 
		resimpcen( 
			frmla
			, dat
			, res_int= TRUE
			, imputation= "pmm"
			, center_mods= FALSE
			, bootstraps= FALSE
		)$formula
		, as.formula( "Y ~ X + M1 + M2 + X.RX.M1")
	)
	expect_equal( 
		resimpcen( 
			frmla
			, dat
			, res_int= FALSE
			, imputation= "pmm"
			, center_mods= TRUE
			, bootstraps= FALSE
		)$formula
		, as.formula( "Y ~ X + M1 + M2 + X.XX.M1")
	)
})
