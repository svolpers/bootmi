context("bootmi_results")

# create data
set.seed(1)
x <- rcauchy(150,0,2)
m1 <- runif(150,5,95)
m2 <- rbinom(150,1,.3)

b0 <- 17
b1 <- 0.1
b1a <- 0.5
b2 <- 0.037
b3 <- -0.2
b4 <- -5.2
b5 <- 4.8

eps <- rnorm(m2,0,20)
y <- b0 + b1*x + b1a*x^2 + b2*m1 + b3*m2 + b4*x*m1 + b5*x*m2 + eps

dat = as.data.frame( matrix( 
	c( y, x, m1, m2)
	, ncol = 4
	, byrow = FALSE
	))
colnames(dat) = c("Y","X","M1","M2")
dat[11,2] = NA; dat[3,3] = NA

frml= "Y ~ -1 + I(X*X) + X*M2 + X*M1"
bootmi_resint= bootmi( frml, dat, 500, "mean", resint= TRUE, seed= 1)
b= lm.bootmi(bootmi_resint)
sisl = simslop( b
	, "X"
	, "M1"
	)

# ci_type = c("basic", "norm", "stud", "perc", "bca", "all")
b$ci_type = "norm"
b$ci = 95/100
sisl$ci_type = "all"
sisl$ci = 95/100
bootmi_object = b
b_res = bootmi_results( b)
sisl_res = bootmi_results( b)

test_that("bootmi.lm results are correct.", {
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

test_that("simslop.bootmi results are correct.", {
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
