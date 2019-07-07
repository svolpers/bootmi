# devtools::document("C:/Users/Volpers/Documents/GitHub/bootmi")

# devtools::use_testthat()



# library("bootmi")



# devtools::install("C:/Users/Volpers/Documents/GitHub/bootmi", build = TRUE)
# install.packages("devtools")
# devtools::install_github(repo = "svolpers/bootmi")


# devtools::load_all()


dat<-read.csv2("../data/cerq-sor.csv")
dels<-list(150,225,227,45,235,215,188,22,219,130)
dat2<-subset(dat, position < 5 & (duration < 0 | duration > 300))
dat2<-subset(dat2, !(id %in% dels))

#recode invers items using car
dat2$ce.zeit.2<-car::recode(dat2$ce.zeit.2, "1=7; 2=6; 3=5; 5=3; 6=2; 7=1")
dat2$ce.zeit.4<-car::recode(dat2$ce.zeit.4, "1=7; 2=6; 3=5; 5=3; 6=2; 7=1")

# create scales
dat2$s_perf <- (rowMeans(subset(dat2, select = c(p3,p5,p6,p1)), na.rm = TRUE))
dat2$s_rel <- (rowMeans(subset(dat2, select = c(r1,r3,r4,r2,r6,r5)), na.rm = TRUE))
dat2$s_ce_ms <- (rowMeans(subset(dat2, select = c(ce.topm.1,ce.topm.2,ce.topm.3,ce.topm.4,ce.topm.5,ce.topm.6)), na.rm = TRUE))
dat2$s_ce_wd <- (rowMeans(subset(dat2, select = c(ce.freiraum.1,ce.freiraum.2,ce.freiraum.4,ce.freiraum.6,ce.freiraum.7)), na.rm = TRUE))
dat2$s_ce_ta <- (rowMeans(subset(dat2, select = c(ce.zeit.1,ce.zeit.3,ce.zeit.4)), na.rm = TRUE))
dat2$s_ce_rr <- (rowMeans(subset(dat2, select = c(ce.belohn.2,ce.belohn.3,ce.belohn.4)), na.rm = TRUE))
dat2$s_ce_ob <- (rowMeans(subset(dat2, select = c(ce.orgaziele.1,ce.orgaziele.3)), na.rm = TRUE))


#subset data set to variables needed
CERQ = subset(dat2, select=c(id,erf.ber,var.gehalt,s_perf,s_rel,s_ce_ms,s_ce_wd,s_ce_ta,s_ce_rr,s_ce_ob))

data_set = CERQ[ order(CERQ$id), ]
rownames(data_set) = data_set$id
data_set = data_set[c(-1)]
# str(data_set); summary(data_set)


# mean center of variables
# data_set = data.frame( apply( data_set, 2, function(x) scale(x, scale = FALSE)))
erf_ber <- scale( data_set$erf.ber, scale = FALSE)

# data_set$erf.ber <- erf_ber


formula1 = "s_perf~s_rel*s_ce_ms+s_rel*s_ce_wd+s_rel*s_ce_ta+s_rel*s_ce_rr+s_rel*s_ce_ob+erf.ber+var.gehalt"

# str(data_set)
simslop <- list(
	x_var = c("s_ce_ms","s_ce_wd","s_ce_ta","s_ce_rr","s_ce_ob")
	, m_var = "s_rel"
	, mod_values_type = "sd"
	, mod_values = c(-1,0,1)
	)

devtools::load_all()
obj1_215 <- bootmi( 
	formula = formula1
	, data = data_set
	, R = 1000
	, imputationMethod = "mean"
	, residualinteractions = TRUE
	, centerinteractions = TRUE
	, seed = 20171231
	, parallel = TRUE 
	# , glm_family = "gaussian" 
	, simslopinfo = simslop
	)

obj1_215_R1000_S20171231 <- obj1_215

save( obj1_215_R1000_S20171231, file ="../data/obj1_215_R1000_S20171231.RData")
str(obj1_215)


cis <- vapply(
	1:length(obj1_215$bootfit$t0)
	, function( x, boot_out, ci, citype) {
		boot::boot.ci( 
			boot_out
			, index = x
			, conf = ci
			, type = citype
		)$bca[c(4:5)]
	}
	, vector("double", length = 2)
	, boot_out = obj1_215$bootfit
	, ci = 0.95
	, citype = "bca"
	)
rownames(cis) <- c("LLCI","ULCI")
# t(cis)

stars <- apply( t(cis), 1, function(x) {
	if( (x["LLCI"] * x["ULCI"]) > 0 ) 
		"*"
	else
		" "
	})

output <- cbind(
	round( summary(obj1_215$bootfit), 2)
	, round( t(cis), 3)
	, stars
	)
rownames(output) <- names(obj1_215$bootfit$t0)
output
