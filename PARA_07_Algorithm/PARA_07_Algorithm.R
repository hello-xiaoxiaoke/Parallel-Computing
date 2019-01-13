
x <- c(9,3,1,4,2,7,8,6,5)

sort1 <- function(x) {
	n <- length(x)
	if (n <= 1) return(x)
	for (i in 1:(n-1)) {
		for (j in 1:(n-i)) {
			if (x[j] > x[j+1]) {
				x[j:(j+1)] <- x[(j+1):j]
				if (n == 2) return(x)
			}
		}	
	}
	return(x)
}

sort2 <- function(x) {
	if(length(x) <= 1) return(x)
	x2 <- x[1]
	x1 <- x[x < x2]
	x3 <- x[x > x2]
	
	y1 <- sort2(x1)
	y2 <- x2
	y3 <- sort2(x3)
	y <- c(y1, y2, y3)
	return(y)
}

mcbsort <- function(x, ncores = 2, nsamp = 1000) {
	require(parallel)
	samp <- sort(x[sample(1:length(x), nsamp, replace = TRUE)])
	dowork <- function(me) {
		k <- floor(nsamp / ncores)
		if (me > 1) mylo <- samp[(me -1) * k + 1]
		if (me < ncores) myhi <- samp[me * k]
		if (me == 1) {
			myx <- x[x <= myhi]
		} else {
			myx <- x[x > mylo & x <= myhi]
		}
		sort(myx)
	}
	res <- mclapply(1:ncores, dowork, mc.cores = ncores)
	c(unlist(res))
}

test <- function(n, ncores) {
	x <- runif(n)
	mcbsort(x, ncores = ncores, nsamp = 1000)
}

test(100, 2)







