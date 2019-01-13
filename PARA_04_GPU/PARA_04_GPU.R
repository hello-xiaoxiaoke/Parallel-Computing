
library(gpuR)
platformInfo(1L)
platformInfo(2L)

gpu.matmult <- function(n) {
	gpuA <- gpuMatrix(runif(n*n), nrow=n, ncol=n, type="float")
	gpuB <- gpuMatrix(runif(n*n), nrow=n, ncol=n, type="float")
	A <- as.matrix(gpuA)
	B <- as.matrix(gpuB)
    tic <- Sys.time()
    C <- A %*% B
    toc <- Sys.time()
    comp.time <- toc - tic
    cat("CPU: ", comp.time, "\n")
    tic <- Sys.time()
    C <- gpuA %*% gpuA
    toc <- Sys.time()
    comp.time <- toc - tic
    cat("GPU: ", comp.time, "\n")
}

gpu.matmult(10)
gpu.matmult(1000)
gpu.matmult(2000)




library(Rth)

n <- 200000000
tmp <- matrix(runif(2*n), ncol = 2)
x <- tmp[, 1]
y <- x + tmp[, 2]

system.time(c1 <- cor.test(x, y))
c1$estimate

system.time(c2 <- rthpearson(x, y, 2))
c1$estimate


