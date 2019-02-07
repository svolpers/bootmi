context("simslop")

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
bootmi_resint= bootmi( frml, dat, 10, "mean", resint= TRUE, seed= 1)
bootmi_not_centered = bootmi( frml, dat, 10, "mean", seed= 1)
b= lm.bootmi(bootmi_resint)
c= lm.bootmi(bootmi_not_centered)

sisl = simslop( b
	, "X"
	, "M1"
	, ci=95
	, mod_values_type = "sd"
	, mod_values = c(-1,0,1)
	)

sisl2 = simslop( c
	, "X"
	, "M2"
	, ci=90
	, mod_values_type = "val"
	, mod_values = c(
		mean( c$data$M1)-sd( c$data$M1)
		,mean( c$data$M1)
		,mean( c$data$M1)+sd( c$data$M1)
		)
	, centered= FALSE
	)

sisl3 = simslop( b$original
	, "X"
	, "M1"
	, ci=99
	, centered= TRUE
	)


test_that("resint \'sd\' slopes values are correct.", {
	expect_equal( 
		sisl$original$slopes[[1]][["m_val_user"]], -1
		)
	expect_equal( 
		signif( sisl$original$slopes[[3]][["m_val_data"]], 6)
		, signif( sd( b$data$M1), 6)
		)
	expect_equal( 
		signif( sisl$original$slopes[[2]][["slope"]], 6)
		, signif( coef(b$original)[["X"]], 6)
		)
	expect_equal( 
		signif( sisl$original$slopes[[1]][["SE"]], 6)
		, 1.19068
		)
	expect_equal( 
		signif( sisl$original$slopes[[1]][["t_value"]], 6)
		, 16.93
		)
	expect_equal( 
		signif( sisl$original$slopes[[2]][["p_value"]], 6)
		, 0
		)	
	expect_equal( 
		signif( sisl$original$slopes[[2]][["LLCI"]], 6)
		, -111.41
		)	
	expect_equal( 
		signif( sisl$original$slopes[[3]][["ULCI"]], 6)
		, -237.903
		)
})


test_that("not centered \'vals\' slopes values are correct.", {
	expect_equal( 
		signif( sisl2$original$slopes[[1]][["m_val_user"]], 6)
		, signif( sisl2$original$slopes[[1]][["m_val_data"]], 6)
		)
	expect_equal( 
		signif( sisl2$original$slopes[[3]][["m_val_data"]], 6)
		, signif( ( mean( c$data$M1) + sd( c$data$M1)), 6)
		)
	expect_equal( 
		signif( sisl2$original$slopes[[2]][["p_value"]], 6)
		, 0.0236522
		)
	expect_equal( 
		names( sisl2$original$coef)
		, as.character( 
			signif( 
				c( 
					( mean( c$data$M1)-sd( c$data$M1))
					,( mean( c$data$M1))
					,( mean( c$data$M1)+sd( c$data$M1))
				)
				, 4
			)
		  )
		)
	expect_equal( 
		signif( sisl2$info$Values_of_Moderator, 4)	
		, signif( 
				c( 
					( mean( c$data$M1)-sd( c$data$M1))
					,( mean( c$data$M1))
					,( mean( c$data$M1)+sd( c$data$M1))
				)
				, 4
			)
		)
	expect_equal( 
		signif( sisl2$plot$x[[1]], 6)
		, signif( sisl2$plot$x[[3]], 6)
		)
	expect_equal( 
		signif( sisl2$plot$x[[2]], 6)
		, signif( sisl2$plot$x[[4]], 6)
		)	
	expect_equal( 
		signif( sisl2$plot$y[[1]], 8)
		, -732.3795
		)	
	expect_equal( 
		signif( sisl2$bootstraps[[1,1]], 4)
		, 144.4
		)	
	expect_equal( 
		signif( sisl2$bootstraps[[5,2]], 4)
		, 248.9
		)
})


test_that("values for lm are correct.", {
	expect_equal( 
		sisl3$original[[1]][["m_val_user"]], -1
		)
	expect_equal( 
		signif( sisl3$original[[3]][["m_val_data"]], 6)
		, signif( sd( b$data$M1), 6)
		)
	expect_equal( 
		signif( sisl3$original[[2]][["slope"]], 6)
		, signif( coef(b$original)[["X"]], 6)
		)
	expect_equal( 
		signif( sisl3$original[[1]][["SE"]], 6)
		, 1.19068
		)
	expect_equal( 
		signif( sisl3$original[[1]][["t_value"]], 6)
		, 16.93
		)
	expect_equal( sisl3$info$Y, "Y" )
	expect_equal( sisl3$info$X, "X" )
	expect_equal( sisl3$info$M, "M1" )
	expect_equal( sisl3$info$XM, "X.RX.M1" )
	expect_equal( sisl3$info$Confidence_Interval, 99 )
})