Exercises 7
# At the end of the trading day in London, we know:
# the closing values on the same day for ALL_ORD, CAC40, DAX, FT-SE100, HANGSENG, NIKKEI
# the closing values on the previous day for AMEX_MAJ, BOVESPA, DJIA, NASDAQ, SP500


pred.footsie.prepare <- function(max.lag = 5, split = c(50, 25), mask = rep(1, 10)) {

	# this function prepares the data for the prediction exercise and splits them into a train, validation and test sets
	# max.lag - the maximum FT-SE100 lag to include in the prediction
	# split - how much of the data (in percentage terms) to include in the training and validation sets, respectively
	# mask - which other indices to include (1 for yes, 0 for no)

	ind <- read.bossa.data(c("FT-SE100", "ALL_ORD", "AMEX_MAJ", "BOVESPA", "CAC40", "DAX", "DJIA", "HANGSENG", "NASDAQ", "NIKKEI", "SP500"))

	d <- dim(ind$r)

	start.index <- max(3, max.lag + 1)

	y <- matrix(0, d[2] - start.index + 1, 1)

	x <- matrix(0, d[2] - start.index + 1, d[1] - 1 + max.lag)

	y[,1] <- ind$r[1,start.index:d[2]]

	for (i in 1:max.lag) {

		x[,i] <- ind$r[1,(start.index-i):(d[2]-i)]

	}

	shift.indices <- c(0, 1, 1, 0, 0, 1, 0, 1, 0, 1)

	for (i in 2:(d[1])) {
		x[,i+max.lag-1] <- ind$r[i,(start.index-1-shift.indices[i-1]):(d[2]-1-shift.indices[i-1])]

	}

	end.training <- round(split[1] / 100 * d[2])

	end.validation <- round(sum(split[1:2]) / 100 * d[2])

	x <- x[,as.logical(c(rep(1, max.lag), mask))]

	y.train <- as.matrix(y[1:end.training], end.training, 1)
	x.train <- x[1:end.training,]

	y.valid <- as.matrix(y[(end.training+1):(end.validation)], end.validation-end.training, 1)
	x.valid <- x[(end.training+1):(end.validation),]

	y.test <- as.matrix(y[(end.validation+1):(d[2] - start.index + 1)], d[2]-start.index-end.validation+1, 1)
	x.test <- x[(end.validation+1):(d[2] - start.index + 1),]

	list(x=x, y=y, x.train=x.train, y.train=y.train, x.valid=x.valid, y.valid=y.valid, x.test=x.test, y.test=y.test)

}



vol.exp.sm <- function(x, lambda) {

	# Exponential smoothing of x^2 with parameter lambda

	sigma2 <- x^2
	n <- length(x)

	for (i in 2:n)
		sigma2[i] <- sigma2[i-1] * lambda + x[i]^2 * (1-lambda)

	sigma <- sqrt(sigma2)
	
	resid <- x/sigma
	resid[is.na(resid)] <- 0
	sq.resid <- resid^2

	list(sigma2=sigma2, sigma=sigma, resid = resid, sq.resid = sq.resid)

}

first.acf.squares.train <- function(x, lambda) {

# x is an object returned by "pred.footsie.prepare"
# this function computes the volatility for each covariate and the response in the training part
# it then computes the acfs of the squared residuals after removing the volatility, and adds up
# the first acfs for each covariate and response
# the point is to choose lambda so that as much as possible of the acf has been removed

	d <- dim(x$x.train)

	ss <- 0

	x.train.dev <- x$x.train
	y.train.dev <- x$y.train

	x.valid.dev <- x$x.valid
	y.valid.dev <- x$y.valid

	x.test.dev <- x$x.test
	y.test.dev <- x$y.test


	for (i in 1:(d[2])) {
		v <- vol.exp.sm(x$x.train[,i], lambda)
		ss <- ss + acf(v$sq.resid, plot=F)$acf[2]
		x.train.dev[,i] <- v$resid

		v <- vol.exp.sm(x$x.valid[,i], lambda)
		x.valid.dev[,i] <- v$resid

		v <- vol.exp.sm(x$x.test[,i], lambda)
		x.test.dev[,i] <- v$resid

	}

	v <- vol.exp.sm(x$y.train, lambda)
	ss <- ss + acf(v$sq.resid, plot=F)$acf[2]
	y.train.dev <- v$resid

	v <- vol.exp.sm(x$y.valid, lambda)
	y.valid.dev <- v$resid

	v <- vol.exp.sm(x$y.test, lambda)
	y.test.dev <- v$resid
		
	list(ss=ss, y.train.dev=y.train.dev, x.train.dev=x.train.dev, y.valid.dev=y.valid.dev, x.valid.dev=x.valid.dev, y.test.dev=y.test.dev, x.test.dev=x.test.dev)

}

thresh.reg <- function(x, y, th, x.pred = NULL) {

	# estimation of beta in y = a + x beta + epsilon (linear regression)
	# but only using those covariates in x whose marginal correlation
	# with y exceeds th
	# use th = 0 for full regression
	# note the intercept is added
	# x.pred is a new x for which we wish to make prediction

	d <- dim(x)

	ind <- (abs(cor(x, y)) > th)
	n <- sum(ind)

	new.x <- matrix(c(rep(1, d[1]), x[,ind]), d[1], n+1)

	beta <- solve(t(new.x) %*% new.x) %*% t(new.x) %*% matrix(y, d[1], 1)

	ind.ex <- c(1, as.numeric(ind))
	
	ind.ex[ind.ex == 1] <- beta

	pr <- 0

	if (!is.null(x.pred)) pr <- sum(ind.ex * c(1, x.pred))

	list(beta = ind.ex, pr=pr)

}


sharpe.curves <- function(x, lambda, th, warmup, win = seq(from = 10, to = warmup, by = 10), reg.function = thresh.reg) {

	# computes Sharpe ratios for a sequence of rolling windows (D in the lecture notes)
	# for the training, validation and test sets

	w <- length(win)

	train.curve <- valid.curve <- test.curve <- rep(0, w)

	for (i in 1:w) {

		train.curve[i] <- rolling.thresh.reg(x, lambda, th, win[i], warmup, reg.function)$err
		valid.curve[i] <- rolling.thresh.reg.valid(x, lambda, th, win[i], warmup, reg.function)$err
		test.curve[i] <- rolling.thresh.reg.test(x, lambda, th, win[i], warmup, reg.function)$err
	}

	list(train.curve = train.curve, valid.curve = valid.curve, test.curve = test.curve)

}


rolling.thresh.reg <- function(x, lambda, th, win, warmup, reg.function = thresh.reg) {

	# performs prediction over a rolling window of size win
	# over the training set
	# x - returned by pred.footsie.prepare
	# lambda - parameter for exponential smoothing
	# th - threshold for thresh.reg
	# warmup - T_0 from the lecture notes


	xx <- first.acf.squares.train(x, lambda)

	n <- length(xx$y.train.dev)

	err <- 0

	predi <- truth <- rep(0, n-warmup+1)

	for (i in warmup:n) {

		y <- xx$y.train.dev[(i-win):(i-1)]
		xxx <- xx$x.train.dev[(i-win):(i-1),]

		zz <- reg.function(xxx, y, th, xx$x.train.dev[i,])

		predi[i-warmup+1] <- zz$pr
		truth[i-warmup+1] <- xx$y.train.dev[i]

	}
	
	ret <- predi * truth

	err <- sqrt(250) * mean(ret) / sqrt(var(ret))

	list(err=err, predi=predi, truth=truth)

}


rolling.thresh.reg.valid <- function(x, lambda, th, win, warmup, reg.function = thresh.reg) {

	# The same as the previous function but for the validation set

	xx <- first.acf.squares.train(x, lambda)

	n <- length(xx$y.valid.dev)

	err <- 0

	predi <- truth <- rep(0, n-warmup+1)

	for (i in warmup:n) {

		y <- xx$y.valid.dev[(i-win):(i-1)]
		xxx <- xx$x.valid.dev[(i-win):(i-1),]

		zz <- reg.function(xxx, y, th, xx$x.valid.dev[i,])

		predi[i-warmup+1] <- zz$pr
		truth[i-warmup+1] <- xx$y.valid.dev[i]

	}

	
	ret <- predi * truth

	err <- sqrt(250) * mean(ret) / sqrt(var(ret))

	list(err=err, predi=predi, truth=truth)

}


rolling.thresh.reg.test <- function(x, lambda, th, win, warmup, reg.function = thresh.reg) {

	# The same as the previous function but for the test set

	xx <- first.acf.squares.train(x, lambda)

	n <- length(xx$y.test.dev)

	err <- 0

	predi <- truth <- rep(0, n-warmup+1)

	for (i in warmup:n) {
	
		y <- xx$y.test.dev[(i-win):(i-1)]
		xxx <- xx$x.test.dev[(i-win):(i-1),]
		zz <- reg.function(xxx, y, th, xx$x.test.dev[i,])

		predi[i-warmup+1] <- zz$pr
		truth[i-warmup+1] <- xx$y.test.dev[i]

	}

	
	ret <- predi * truth

	err <- sqrt(250) * mean(ret) / sqrt(var(ret))

	list(err=err, predi=predi, truth=truth)

}


sim.grid <- function(x, lambda = 0.944, th.grid = seq(from = 0, to = 1, by = .01), win, warmup = 250) {

	# Which threshold th best over training set?

	tt <- length(th.grid)

	res <- rep(0, tt)

	for (i in 1:tt) res[i] <- rolling.thresh.reg(x, lambda, th.grid[i], win, warmup)$err

	res

}

sim.grid.valid <- function(x, lambda = 0.944, th.grid = seq(from = 0, to = 1, by = .01), win, warmup = 250) {

	# The same over the validation set.

	tt <- length(th.grid)

	res <- rep(0, tt)

	for (i in 1:tt) res[i] <- rolling.thresh.reg.valid(x, lambda, th.grid[i], win, warmup)$err

	res

}



R notebook with a detailed discussion of this exercise sheet
# A good way to check the quality of the data is to run the 
# cov.est.2 function, known to you from the previous seminars,
# on each pair of indices. For example:

ddd <- cov.est.2(c("SP500", "NIKKEI"), .95)

# Everything seems fine there: if we do

ts.plot(ddd$m[1,1,])
ts.plot(ddd$m[2,2,])

# then we get plausibly-looking plots of the estimated volatilities
# of both indices (indicentally, please note how NIKKEI seems to have
# 'ignored' the recent events; the other such index appears to be 
# HANGSENG (the HK index)).

# However, not all datasets pass this test. For example, trying

ddd <- cov.est.2(c("DAX", "CAC40"), .95)

# gives:

# Read 6380 records
# Read 6414 records
# Error in rnorm(sum(r[i, ] == 0)) : invalid arguments

# We need to look at both files to understand what is happening there.

# In the CAC40 file, this seems to be the offending record:

# CAC40,20160927,4436.0000,4438.5400,0.0000,0.0000,0

# replacing it with 

# CAC40,20160927,4436.0000,4438.5400,4402.1300,4407.8500,0

# leads to no execution errors for 

ddd <- cov.est.2(c("DAX", "CAC40"), .95)

# now. Also, the estimated volatility for CAC40 looks plausible:

ts.plot(ddd$m[2,2,])

# However, we still need to look at DAX. Here, the offending line
# seems to be

# DAX,20171109,13378.9600,13402.0500,0.0600,0.0600,0

# Replacing it with

# DAX,20171109,13378.9600,13402.0500,13345.1100,13382.4200,0

# now leads to plausible-looking volatility estimator for DAX.

# With these edits, 

ddd <- pred.footsie.prepare()

# executes correctly.

# Let us try executing the main function for this case study:

sharpe.curves(ddd, 0.95, 0, 250)

# This should gives us the Sharpe ratio for our linear prediction exercise,
# where we use the parameter 0.95 for exponential smoothing (see the lecture
# notes!), we do not remove any covariates for which the marginal correlation
# with the response is below a certain threshold (as we set the threshold to zero),
# and the warmup period is 250.

# However, we are most likely getting:

# Error in solve.default(t(new.x) %*% new.x) : 
#  system is computationally singular: reciprocal condition number = 1.82977e-18

# This is because we start, by default, with too small a value of D - please see
# the "win" parameter in sharpe.curves, which by default is

# win = seq(from = 10, to = warmup, by = 10)

# We need to change this so we are guaranteed to have regression problems with
# more observations than parameters, so that the X'X matrices are stably invertible. Try

sharpe.curves(ddd, 0.95, 0, 250, win = seq(from = 40, by = 10, to=250))

# This should produce meaningful results.

# One thing to observe is that the pred.footsie.prepare() function is actually
# random! This is because we have chosen to replace zero returns by random normally
# distributed ones in the function. So another execution should give us slightly
# different Sharpe ratios. Try executing this pair of commands again 
# to see for yourself:

ddd <- pred.footsie.prepare()
sharpe.curves(ddd, 0.95, 0, 250, win = seq(from = 40, by = 10, to=250))

# The results on the validation set are not very good for me. There are several ways
# in which we could try to improve them. We could e.g. remove all external covariates:

ddd <- pred.footsie.prepare(mask = rep(0, 10))
sharpe.curves(ddd, 0.95, 0, 250, win = seq(from = 40, by = 10, to=250))

# This seems to work much better, including on the test set (but remember the randomness issue).

# Another interesting parameter to play with is the threshold parameter. However,
# increasing the threshold does not appear to make a positive difference to me.

# Overall, my own conclusion is that a drastic reduction in the number of external
# covariates seems to work well. It is however possible that bringing back the right
# ones in the right way could improve matters: I will leave it to you to investigate
# further.








# Original code below #

# At the end of the trading day in London, we know:
# the closing values on the same day for ALL_ORD, CAC40, DAX, FT-SE100, HANGSENG, NIKKEI
# the closing values on the previous day for AMEX_MAJ, BOVESPA, DJIA, NASDAQ, SP500


pred.footsie.prepare <- function(max.lag = 5, split = c(50, 25), mask = rep(1, 10)) {

	# this function prepares the data for the prediction exercise and splits them into a train, validation and test sets
	# max.lag - the maximum FT-SE100 lag to include in the prediction
	# split - how much of the data (in percentage terms) to include in the training and validation sets, respectively
	# mask - which other indices to include (1 for yes, 0 for no)

	ind <- read.bossa.data(c("FT-SE100", "ALL_ORD", "AMEX_MAJ", "BOVESPA", "CAC40", "DAX", "DJIA", "HANGSENG", "NASDAQ", "NIKKEI", "SP500"))

	d <- dim(ind$r)

	start.index <- max(3, max.lag + 1)

	y <- matrix(0, d[2] - start.index + 1, 1)

	x <- matrix(0, d[2] - start.index + 1, d[1] - 1 + max.lag)

	y[,1] <- ind$r[1,start.index:d[2]]

	for (i in 1:max.lag) {

		x[,i] <- ind$r[1,(start.index-i):(d[2]-i)]

	}

	shift.indices <- c(0, 1, 1, 0, 0, 1, 0, 1, 0, 1)

	for (i in 2:(d[1])) {
		x[,i+max.lag-1] <- ind$r[i,(start.index-1-shift.indices[i-1]):(d[2]-1-shift.indices[i-1])]

	}

	end.training <- round(split[1] / 100 * d[2])

	end.validation <- round(sum(split[1:2]) / 100 * d[2])

	x <- x[,as.logical(c(rep(1, max.lag), mask))]

	y.train <- as.matrix(y[1:end.training], end.training, 1)
	x.train <- x[1:end.training,]

	y.valid <- as.matrix(y[(end.training+1):(end.validation)], end.validation-end.training, 1)
	x.valid <- x[(end.training+1):(end.validation),]

	y.test <- as.matrix(y[(end.validation+1):(d[2] - start.index + 1)], d[2]-start.index-end.validation+1, 1)
	x.test <- x[(end.validation+1):(d[2] - start.index + 1),]

	list(x=x, y=y, x.train=x.train, y.train=y.train, x.valid=x.valid, y.valid=y.valid, x.test=x.test, y.test=y.test)

}



vol.exp.sm <- function(x, lambda) {

	# Exponential smoothing of x^2 with parameter lambda

	sigma2 <- x^2
	n <- length(x)

	for (i in 2:n)
		sigma2[i] <- sigma2[i-1] * lambda + x[i]^2 * (1-lambda)

	sigma <- sqrt(sigma2)
	
	resid <- x/sigma
	resid[is.na(resid)] <- 0
	sq.resid <- resid^2

	list(sigma2=sigma2, sigma=sigma, resid = resid, sq.resid = sq.resid)

}

first.acf.squares.train <- function(x, lambda) {

# x is an object returned by "pred.footsie.prepare"
# this function computes the volatility for each covariate and the response in the training part
# it then computes the acfs of the squared residuals after removing the volatility, and adds up
# the first acfs for each covariate and response
# the point is to choose lambda so that as much as possible of the acf has been removed

	d <- dim(x$x.train)

	ss <- 0

	x.train.dev <- x$x.train
	y.train.dev <- x$y.train

	x.valid.dev <- x$x.valid
	y.valid.dev <- x$y.valid

	x.test.dev <- x$x.test
	y.test.dev <- x$y.test


	for (i in 1:(d[2])) {
		v <- vol.exp.sm(x$x.train[,i], lambda)
		ss <- ss + acf(v$sq.resid, plot=F)$acf[2]
		x.train.dev[,i] <- v$resid

		v <- vol.exp.sm(x$x.valid[,i], lambda)
		x.valid.dev[,i] <- v$resid

		v <- vol.exp.sm(x$x.test[,i], lambda)
		x.test.dev[,i] <- v$resid

	}

	v <- vol.exp.sm(x$y.train, lambda)
	ss <- ss + acf(v$sq.resid, plot=F)$acf[2]
	y.train.dev <- v$resid

	v <- vol.exp.sm(x$y.valid, lambda)
	y.valid.dev <- v$resid

	v <- vol.exp.sm(x$y.test, lambda)
	y.test.dev <- v$resid
		
	list(ss=ss, y.train.dev=y.train.dev, x.train.dev=x.train.dev, y.valid.dev=y.valid.dev, x.valid.dev=x.valid.dev, y.test.dev=y.test.dev, x.test.dev=x.test.dev)

}

thresh.reg <- function(x, y, th, x.pred = NULL) {

	# estimation of beta in y = a + x beta + epsilon (linear regression)
	# but only using those covariates in x whose marginal correlation
	# with y exceeds th
	# use th = 0 for full regression
	# note the intercept is added
	# x.pred is a new x for which we wish to make prediction

	d <- dim(x)

	ind <- (abs(cor(x, y)) > th)
	n <- sum(ind)

	new.x <- matrix(c(rep(1, d[1]), x[,ind]), d[1], n+1)

	beta <- solve(t(new.x) %*% new.x) %*% t(new.x) %*% matrix(y, d[1], 1)

	ind.ex <- c(1, as.numeric(ind))
	
	ind.ex[ind.ex == 1] <- beta

	pr <- 0

	if (!is.null(x.pred)) pr <- sum(ind.ex * c(1, x.pred))

	list(beta = ind.ex, pr=pr)

}


sharpe.curves <- function(x, lambda, th, warmup, win = seq(from = 10, to = warmup, by = 10), reg.function = thresh.reg) {

	# computes Sharpe ratios for a sequence of rolling windows (D in the lecture notes)
	# for the training, validation and test sets

	w <- length(win)

	train.curve <- valid.curve <- test.curve <- rep(0, w)

	for (i in 1:w) {

		train.curve[i] <- rolling.thresh.reg(x, lambda, th, win[i], warmup, reg.function)$err
		valid.curve[i] <- rolling.thresh.reg.valid(x, lambda, th, win[i], warmup, reg.function)$err
		test.curve[i] <- rolling.thresh.reg.test(x, lambda, th, win[i], warmup, reg.function)$err
	}

	list(train.curve = train.curve, valid.curve = valid.curve, test.curve = test.curve)

}


rolling.thresh.reg <- function(x, lambda, th, win, warmup, reg.function = thresh.reg) {

	# performs prediction over a rolling window of size win
	# over the training set
	# x - returned by pred.footsie.prepare
	# lambda - parameter for exponential smoothing
	# th - threshold for thresh.reg
	# warmup - T_0 from the lecture notes


	xx <- first.acf.squares.train(x, lambda)

	n <- length(xx$y.train.dev)

	err <- 0

	predi <- truth <- rep(0, n-warmup+1)

	for (i in warmup:n) {

		y <- xx$y.train.dev[(i-win):(i-1)]
		xxx <- xx$x.train.dev[(i-win):(i-1),]

		zz <- reg.function(xxx, y, th, xx$x.train.dev[i,])

		predi[i-warmup+1] <- zz$pr
		truth[i-warmup+1] <- xx$y.train.dev[i]

	}
	
	ret <- predi * truth

	err <- sqrt(250) * mean(ret) / sqrt(var(ret))

	list(err=err, predi=predi, truth=truth)

}


rolling.thresh.reg.valid <- function(x, lambda, th, win, warmup, reg.function = thresh.reg) {

	# The same as the previous function but for the validation set

	xx <- first.acf.squares.train(x, lambda)

	n <- length(xx$y.valid.dev)

	err <- 0

	predi <- truth <- rep(0, n-warmup+1)

	for (i in warmup:n) {

		y <- xx$y.valid.dev[(i-win):(i-1)]
		xxx <- xx$x.valid.dev[(i-win):(i-1),]

		zz <- reg.function(xxx, y, th, xx$x.valid.dev[i,])

		predi[i-warmup+1] <- zz$pr
		truth[i-warmup+1] <- xx$y.valid.dev[i]

	}

	
	ret <- predi * truth

	err <- sqrt(250) * mean(ret) / sqrt(var(ret))

	list(err=err, predi=predi, truth=truth)

}


rolling.thresh.reg.test <- function(x, lambda, th, win, warmup, reg.function = thresh.reg) {

	# The same as the previous function but for the test set

	xx <- first.acf.squares.train(x, lambda)

	n <- length(xx$y.test.dev)

	err <- 0

	predi <- truth <- rep(0, n-warmup+1)

	for (i in warmup:n) {
	
		y <- xx$y.test.dev[(i-win):(i-1)]
		xxx <- xx$x.test.dev[(i-win):(i-1),]
		zz <- reg.function(xxx, y, th, xx$x.test.dev[i,])

		predi[i-warmup+1] <- zz$pr
		truth[i-warmup+1] <- xx$y.test.dev[i]

	}

	
	ret <- predi * truth

	err <- sqrt(250) * mean(ret) / sqrt(var(ret))

	list(err=err, predi=predi, truth=truth)

}


sim.grid <- function(x, lambda = 0.944, th.grid = seq(from = 0, to = 1, by = .01), win, warmup = 250) {

	# Which threshold th best over training set?

	tt <- length(th.grid)

	res <- rep(0, tt)

	for (i in 1:tt) res[i] <- rolling.thresh.reg(x, lambda, th.grid[i], win, warmup)$err

	res

}

sim.grid.valid <- function(x, lambda = 0.944, th.grid = seq(from = 0, to = 1, by = .01), win, warmup = 250) {

	# The same over the validation set.

	tt <- length(th.grid)

	res <- rep(0, tt)

	for (i in 1:tt) res[i] <- rolling.thresh.reg.valid(x, lambda, th.grid[i], win, warmup)$err

	res

}




read.bossa.data <- function(vec.names) {
	p <- length(vec.names)
	n1 <- 20000
	dates <- matrix(99999999, p, n1)
	closes <- matrix(0, p, n1)
	max.n2 <- 0
	for (i in 1:p) {
		filename <- paste("Downloads/mstzgr/",vec.names[i], ".mst", sep="")
		tmp <- scan(filename, list(NULL, date=numeric(), NULL, NULL, NULL, close=numeric(), NULL), skip=1, sep=",")
		n2 <- length(tmp$date)
		max.n2 <- max(n2, max.n2)
		dates[i,1:n2] <- tmp$date
		closes[i,1:n2] <- tmp$close
	}

	dates <- dates[,1:max.n2]
	closes <- closes[,1:max.n2]

	days <- rep(0, n1)
	arranged.closes <- matrix(0, p, n1)

	date.indices <- starting.indices <- rep(1, p)

	already.started <- rep(0, p)

	day <- 1

	while(max(date.indices) <= max.n2) {
		current.dates <- current.closes <- rep(0, p)
		for (i in 1:p) {
			current.dates[i] <- dates[i,date.indices[i]]
			current.closes[i] <- closes[i,date.indices[i]]
		}

		min.indices <- which(current.dates == min(current.dates))
		days[day] <- current.dates[min.indices[1]]
		arranged.closes[min.indices,day] <- log(current.closes[min.indices])
		arranged.closes[-min.indices,day] <- arranged.closes[-min.indices, max(day-1, 1)]

		already.started[min.indices] <- 1
		starting.indices[-which(already.started == 1)] <- starting.indices[-which(already.started == 1)] + 1

		day <- day + 1
		date.indices[min.indices] <- date.indices[min.indices] + 1
	}

	days <- days[1:(day-1)]
	arranged.closes <- arranged.closes[,1:(day-1)]

	max.st.ind <- max(starting.indices)

	max.length <- 2
	while ((2 * max.length) <= (day - max.st.ind - 1)) max.length <- max.length * 2

	r <- matrix(0, p, max.length)
	for (i in 1:p) {
		r[i,] <- diff(arranged.closes[i,(day-max.length-1):(day-1)])
		r[i,] <- r[i,] / sqrt(var(r[i,]))
		r[i,r[i,]==0] <- rnorm(sum(r[i,]==0))
	}

	return(list(dates=dates, closes=closes, days=days, arranged.closes=arranged.closes, starting.indices=starting.indices, r=r))
}

