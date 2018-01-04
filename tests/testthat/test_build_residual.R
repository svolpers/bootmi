context("Build Residual")

# create data
set.seed(1)
dat = as.data.frame( matrix( rnorm(80, mean = 4, sd = 2), ncol = 4, byrow = FALSE))
colnames(dat) = c("X","M1","M2","Y")
dat[11,2] = NA; dat[3,3] = NA

# Manually create uncorrelated residual
X.RX.M1 <- residuals( lm( I(X*M1) ~ X + M1, data= dat ))
res = cbind( as.numeric( names( X.RX.M1)), as.numeric(X.RX.M1))
colnames(res) = c("id", "X.RX.M1")
dat = cbind( as.numeric( rownames(dat)), dat)
colnames(dat)[1] = "id"
dat = merge(dat, res, by= "id", all.x= TRUE)
rownames(dat) = dat$id
dat = dat[-1]

X.RX.M2 <- residuals( lm( I(X*M2) ~ X + M2, data= dat ))
res = cbind( as.numeric( names( X.RX.M2)), as.numeric(X.RX.M2))
colnames(res) = c("id", "X.RX.M2")
dat = cbind( as.numeric( rownames(dat)), dat)
colnames(dat)[1] = "id"
dat = merge(dat, res, by= "id", all.x= TRUE)
rownames(dat) = dat$id
dat = dat[-1]

M1.RX.M2 <- residuals( lm( I(M1*M2) ~ M1 + M2, data= dat ))
res = cbind( as.numeric( names( M1.RX.M2)), as.numeric(M1.RX.M2))
colnames(res) = c("id", "M1.RX.M2")
dat = cbind( as.numeric( rownames(dat)), dat)
colnames(dat)[1] = "id"
dat = merge(dat, res, by= "id", all.x= TRUE)
rownames(dat) = dat$id
dat = dat[-1]

X.RX.Y <- residuals( lm( I(X*Y) ~ X + Y, data= dat ))
res = cbind( as.numeric( names( X.RX.Y)), as.numeric(X.RX.Y))
colnames(res) = c("id", "X.RX.Y")
dat = cbind( as.numeric( rownames(dat)), dat)
colnames(dat)[1] = "id"
dat = merge(dat, res, by= "id", all.x= TRUE)
rownames(dat) = dat$id
dat = dat[-1]

M2.RX.Y <- residuals( lm( I(M2*Y) ~ M2 + Y, data= dat ))
res = cbind( as.numeric( names( M2.RX.Y)), as.numeric(M2.RX.Y))
colnames(res) = c("id", "M2.RX.Y")
dat = cbind( as.numeric( rownames(dat)), dat)
colnames(dat)[1] = "id"
dat = merge(dat, res, by= "id", all.x= TRUE)
rownames(dat) = dat$id
dat = dat[-1]

M1.RX.Y <- residuals( lm( I(M1*Y) ~ M1 + Y, data= dat ))
res = cbind( as.numeric( names( M1.RX.Y)), as.numeric(M1.RX.Y))
colnames(res) = c("id", "M1.RX.Y")
dat = cbind( as.numeric( rownames(dat)), dat)
colnames(dat)[1] = "id"
dat = merge(dat, res, by= "id", all.x= TRUE)
rownames(dat) = dat$id
dat = dat[-1]

X.RX.M1.RX.M2 <- residuals( 
	lm( I(X*M1*M2) ~ X + M1 + M2 + X.RX.M1 + X.RX.M2 + M1.RX.M2, data= dat ))
res = cbind( as.numeric( names( X.RX.M1.RX.M2)), as.numeric(X.RX.M1.RX.M2))
colnames(res) = c("id", "X.RX.M1.RX.M2")
dat = cbind( as.numeric( rownames(dat)), dat)
colnames(dat)[1] = "id"
dat = merge(dat, res, by= "id", all.x= TRUE)
rownames(dat) = dat$id
dat = dat[-1]

X.RX.M1.RX.Y <- residuals( 
	lm( I(X*Y*M1) ~ X + Y + M1 + X.RX.Y + X.RX.M1 + M1.RX.Y, data= dat ))
res = cbind( as.numeric( names( X.RX.M1.RX.Y)), as.numeric(X.RX.M1.RX.Y))
colnames(res) = c("id", "X.RX.M1.RX.Y")
dat = cbind( as.numeric( rownames(dat)), dat)
colnames(dat)[1] = "id"
dat = merge(dat, res, by= "id", all.x= TRUE)
rownames(dat) = dat$id
dat = dat[-1]

X.RX.M2.RX.Y <- residuals( 
	lm( I(X*Y*M2) ~ X + Y + M2 + X.RX.Y + X.RX.M2 + M2.RX.Y, data= dat ))
res = cbind( as.numeric( names( X.RX.M2.RX.Y)), as.numeric(X.RX.M2.RX.Y))
colnames(res) = c("id", "X.RX.M2.RX.Y")
dat = cbind( as.numeric( rownames(dat)), dat)
colnames(dat)[1] = "id"
dat = merge(dat, res, by= "id", all.x= TRUE)
rownames(dat) = dat$id
dat = dat[-1]

M1.RX.M2.RX.Y <- residuals( 
	lm( I(M1*M2*Y) ~ Y + M1 + M2 + M1.RX.Y + M2.RX.Y + M1.RX.M2, data= dat ))
res = cbind( as.numeric( names( M1.RX.M2.RX.Y)), as.numeric(M1.RX.M2.RX.Y))
colnames(res) = c("id", "M1.RX.M2.RX.Y")
dat = cbind( as.numeric( rownames(dat)), dat)
colnames(dat)[1] = "id"
dat = merge(dat, res, by= "id", all.x= TRUE)
rownames(dat) = dat$id
dat = dat[-1]

# Not needed for testing, 
# just for calculation of the correct value

# X.RX.M1.RX.M2.RX.Y <- residuals( 
# 	lm( I(X*M1*M2*Y)~X+M1+M2+Y+X.RX.M1+X.RX.M2+X.RX.Y+M1.RX.M2+M1.RX.Y+M2.RX.Y+X.RX.M1.RX.M2+X.RX.M1.RX.Y+X.RX.M2.RX.Y+M1.RX.M2.RX.Y
# 		, data= dat ))
# res = cbind( as.numeric( names( X.RX.M1.RX.M2.RX.Y)), as.numeric(X.RX.M1.RX.M2.RX.Y))
# colnames(res) = c("id", "X.RX.M1.RX.M2.RX.Y")
# dat = cbind( as.numeric( rownames(dat)), dat)
# colnames(dat)[1] = "id"
# dat = merge(dat, res, by= "id", all.x= TRUE)
# rownames(dat) = dat$id
# dat = dat[-1]

test_that("amount of values is correct.", {
	expect_equal( nrow( build_residual( "X:M1", dat, "X.RX.M1")), eval(19))
	expect_equal( nrow( build_residual( "X:M2:Y", dat, "X.RX.M2.RX.Y")), eval(19))
	expect_equal( nrow( build_residual( "X:Y", dat, "X.RX.Y")), eval(20))
	expect_equal( nrow( build_residual( "X:M1:M2:Y", dat, "X.RX.M1.RX.M2.RX.Y")), eval(18))
})

test_that("value of residual interaction terms are correct.", {
	expect_equal( build_residual( "X:M1", dat, "X.RX.M1")["13",], -1.68266805)
	expect_equal( build_residual( "X:M2", dat, "X.RX.M2")["3",], as.double(NA))
	expect_equal( build_residual( "M2:Y", dat, "M2.RX.Y")["11",], -0.30508056)
	expect_equal( build_residual( "X:M1:M2", dat, "X.RX.M1.RX.M2")["4",], 0.88848298)
	expect_equal( build_residual( "M1:M2:Y", dat, "M1.RX.M2.RX.Y")["15",], 4.9461284)
	expect_equal( build_residual( "X:M1:M2:Y", dat, "X.RX.M1.RX.M2.RX.Y")["2",], -2.02642761)
	expect_equal( build_residual( "X:M1:M2:Y", dat, "X.RX.M1.RX.M2.RX.Y")["12",], 2.11278153)
})
