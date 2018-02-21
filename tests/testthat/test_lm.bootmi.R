context("lm.bootmi")

# create data
set.seed(1)
x <- rcauchy(50,0,2)
m1 <- runif(50,5,95)
m2 <- rbinom(50,1,.5)

b0 <- 17
b1 <- 0.1
b1a <- 0.5
b2 <- 0.037
b3 <- -0.2
b4 <- -5.2
b5 <- 4.8

eps <- rnorm(m2,0,15)
y <- b0 + b1*x + b1a*x^2 + b2*m1 + b3*m2 + b4*x*m1 + b5*x*m2 + eps

dat = as.data.frame( matrix( 
	c( y, x, m1, m2)
	, ncol = 4
	, byrow = FALSE
	))
colnames(dat) = c("Y","X","M1","M2")
dat[11,2] = NA; dat[3,3] = NA

frml= "Y ~ -1 + I(X*X) + X*M2 + X*M1"
bootmi= bootmi( frml, dat, 200, "mean", resint= TRUE, seed= 1)
b= lm.bootmi(bootmi)

test_that("bootstrap dimensions are correct.", {
	expect_equal( 
		dim( b$bootstraps)
		, c( b$replics
			, length( attr( b$original$terms, "term.labels")) + attr( b$original$terms, "intercept")
			)
		)
})

test_that("bootstrap values are correct.", {
	expect_equal( 
		signif( colMeans( b$bootstraps), 6)
		, c(
			X.RX.X= 0.49887          
			, X= -105.12400
			, M2= -57.97170
			, M1= 3.39313
			, X.RX.M2 = 5.73417
			, X.RX.M1 = -5.02711
		   )
		)
})

test_that("lm model is correct.", {
	expect_equal( 
		length( b$original)
		, length( lm( bootmi$formula, bootmi$data))
		)
	expect_equal( 
		coef( b$original)
		, coef( lm( bootmi$formula, bootmi$data))
		)
})