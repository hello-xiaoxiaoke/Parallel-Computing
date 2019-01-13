

################
##  性能度量  ##
################

library(profvis)

la1 <- function(X, FUN, ...) {
	FUN <- match.fun(FUN)
	if (!is.list(X)) X <- as.list(X)
	rval <- vector("list", length(X))
	for(i in seq(along = X)) rval[i] <- list(FUN(X[[i]], ...))
	names(rval) <- names(X)
	return(rval)
}

y <- 1:1000
system.time(for (i in 1:1000) la1(y, is.null))

outfile <- tempfile(fileext = ".txt")
Rprof(filename = outfile, append = FALSE, interval = 0.02,
memory.profiling=FALSE)
for (i in 1:1000) la1(y, is.null)
Rprof(NULL)
summaryRprof(filename = outfile)$by.self


profvis({
	for (i in 1:1000) la1(y, is.null)
})


################
##  基础优化  ##
################

library(compiler)
la1c <- cmpfun(la1)
y <- 1:1000
system.time(for (i in 1:1000) la1(y, is.null))
system.time(for (i in 1:1000) la1c(y, is.null))



vec1 <- function(n) {
	v <- 1:n
	for (i in 1:n) v[i] <- v[i]^2
	v
}

vec2 <- function(n) {
	(1:n)^2
}
system.time(vec1(10000))
system.time(vec1(1000000))
system.time(vec2(10000))
system.time(vec2(1000000))



#################
##    blas     ##
#################

A <- matrix(runif(3000^2),3000,3000)

system.time(A%*%A)



#################
##  bigmemory  ##
#################

library(bigmemory)
library(biganalytics)
library(bigalgebra)

sim1 <- matrix(rnorm(20000000), 200000, 100)
object.size(sim1) / 1024^2
sim2 <- as.big.matrix(sim1)
object.size(sim2) / 1024^2

summary(sim2)

set.seed(1)
A <- matrix(rnorm(4), nrow=2)
a <- as.big.matrix(A) 
b <- a %*% a 
c <- a + a 
d <- 2 * a 
d <- a * 2
f <- a %*% A
g <- a %*% a - b 




