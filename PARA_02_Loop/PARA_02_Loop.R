

####################
##  常用并行框架  ##
####################

# 串行代码
links <- matrix(0, 5, 5)
links[1, c(2,5)] <- 1
links[2, c(1,4,5)] <- 1
links[3, c(2,4)] <- 1
links[4, c(1,2,3)] <- 1
links[5, c(1,2,3,5)] <- 1

mutoutser <- function(links) {
   nr <- nrow(links)
   nc <- ncol(links)
   tot <- 0
   for (i in 1:(nr-1)) {
      for (j in (i+1):nr) {
         for (k in 1:nc) {
			tot <- tot + links[i,k] * links[j,k]
		 }   
      }
   }
   tot / (nr * (nr - 1) / 2)
}

sim <- function(nr, nc) {
   lnk <- matrix(sample(0:1,(nr*nc),replace=TRUE),nrow=nr)
   print(system.time(mutoutser(lnk)))
}
sim(500, 500)


# 矩阵方式
mutoutser1 <- function(links) {
  nr <- nrow(links)
  nc <- ncol(links)
  tot <- 0
  for (i in 1:(nr-1)) {
    tmp <- links[(i+1):nr, ] %*% links[i, ]
    tot <- tot + sum(tmp)
  }
  tot / nr
}

sim1 <- function(nr,nc) {
   lnk <- matrix(sample(0:1,(nr*nc),replace=TRUE),nrow=nr)
   print(system.time(mutoutser1(lnk)))
}
sim1(500, 500)
sim1(2000, 2000)




#################
##   apply     ##
#################

# 行列指标
apply(mtcars, 1, mean)
apply(mtcars, 2, range)

# 自定义函数
myfun <- function(x, n) return(sum(x*n))
apply(mtcars, 2, myfun, n = 2)
apply(mtcars, 2, myfun, n = rnorm(32))

# lapply, sapply
sapply(mtcars, data.class) 
lapply(mtcars, data.class)
lapply(mtcars, quantile)
lapply(1:3, rnorm)




# 外链问题解法1
library(parallel)

doichunk <- function(ichunk) {
   tot <- 0
   nr <- nrow(lnks)
   for (i in ichunk) {
     
      tmp <- lnks[(i+1):nr,] %*% lnks[i,]
      tot <- tot + sum(tmp)
   }
   tot
}

mutoutpar <- function(cls, lnks) {
   nr <- nrow(lnks)
   clusterExport(cls,"lnks")
   ichunks <- 1:(nr-1)
   tots <- clusterApply(cls,ichunks,doichunk)  # 这一步就是并行计算
   Reduce(sum,tots) / nr
}

snowsim <- function(nr,nc,cls) {
   lnks <<- matrix(sample(0:1,(nr*nc),replace=TRUE),nrow=nr)
   print(system.time(mutoutpar(cls, lnks)))
}

cl2 <- makeCluster(2)
cl4 <- makeCluster(4)  

snowsim(2000, 2000, cl2)

# 外链问题使用foreach---没有框架也能并行
mutoutfe <- function(links) {
	require(foreach)
	nr <- nrow(links)
	nc <- ncol(links)
	tot = 0
	foreach (i = 1:(nr-1)) %dopar% {
		for (j in (i+1):nr) {
			for (k in 1:nc) 
				tot <- tot + links[i,k] * links[j,k]
		}
	}
	tot / (nr * (nr - 1) / 2)
}

simfe <- function(nr, nc, ncores) {
	require(doParallel)
	cls <- makePSOCKcluster(ncores)
	registerDoParallel(cls)  # 注册一个集群,自动并行,代码都不用改
	lnk <<- matrix(sample(0:1,(nr*nc),replace=TRUE),nrow=nr)
	print(system.time(mutoutfe(lnk)))
	stopCluster(cls)
}
sim(500, 500)




####################
##  循环调度实例  ##
####################

# 基于 Snow

snowapr <- function(cls, x, y, k, reverse = FALSE,
  dyn = FALSE, chunk = 1) {
  p <- ncol(x)
  allcombs <- genallcombs(p,k)
  ncombs <- length(allcombs)
  clusterExport(cls,"do1pset")
  tasks <- if (!reverse) seq(1,ncombs,chunk) else  # reverse不反向调度
    seq(ncombs,1,-chunk) 
  if (!dyn) {
    out <- clusterApply(cls, tasks, dochunk, x, y,
	  allcombs, chunk)
  } else {
    out <- clusterApplyLB(cls, tasks, dochunk, x, y, 
	  allcombs, chunk)
  }
  Reduce(rbind,out)
}

genallcombs <- function(p,k) {
  allcombs <- list()
  for (i in 1:k) {
    tmp <- combn(1:p,i)
    allcombs <- c(allcombs,matrixtolist(tmp,rc=2))
  }
  allcombs
}

matrixtolist <- function(rc,m) {
  if (rc == 1) {
    Map(function(rownum) m[rownum,],1:nrow(m))
  } else Map(function(colnum) m[,colnum],1:ncol(m))
}

dochunk <- function(psetstart,x,y,allcombs,chunk) {
  ncombs <- length(allcombs)
  lasttask <- min(psetstart+chunk-1,ncombs)
  t(sapply(allcombs[psetstart:lasttask],do1pset,x,y))  #用sapply,一次处理5个比for循环快
}

do1pset <- function(onepset,x,y) {
  slm <- summary(lm(y ~ x[,onepset]))
  n0s <- ncol(x) - length(onepset)
  c(slm$adj.r.squared,onepset,rep(0,n0s))
}

gendata <- function(n,p) {
  x <<- matrix(rnorm(n*p),ncol=p)
  y <<- x%*%c(rep(0.5,p)) + rnorm(n)
}

test <- function(cls,n,p,k,chunk=1,dyn=F,rvrs=F) {
  gendata(n,p)
  system.time(snowapr(cls,x,y,k,rvrs,dyn,chunk))
}

library(parallel)
cl2 <- makeCluster(2) 
cl4 <- makeCluster(4) 

genallcombs(4,2)

test(cl2, 10000, 20, 3, dyn = FALSE)
test(cl2, 10000, 20, 3, dyn = TRUE)
test(cl2, 10000, 20, 3, dyn = TRUE, chunk = 25)
test(cl2, 10000, 20, 3, dyn = TRUE, chunk = 50)
test(cl4, 10000, 20, 3, dyn = TRUE, chunk = 25)
test(cl4, 10000, 20, 3, dyn = TRUE, chunk = 50)



# 基于 multicore

library(parallel)
mcapr <- function(x,y,k,
      reverse=F,dyn=F,chunk=1, ncores=detectCores())  {
   x <- cbind(1,x)
   xpx <- crossprod(x,x)
   xpy <- crossprod(x,y)
   p <- ncol(x) - 1
   allcombs <- genallcombs(p,k)
   ncombs <- length(allcombs)
   tasks <- if (!reverse) seq(1,ncombs,chunk) else 
      seq(ncombs,1,-chunk) 
   out <- mclapply(tasks,dochunk1,
      x,y,xpx,xpy,allcombs,chunk,mc.cores=ncores,mc.preschedule=!dyn)
   Reduce(rbind,out)
}

genallcombs <- function(p,k) {
   allcombs <- list()
   for (i in 1:k) {
      tmp <- combn(1:p,i)
      allcombs <- c(allcombs,matrixtolist(tmp,rc=2))
   }
   allcombs
}

matrixtolist <- function(rc,m) {
   if (rc == 1) {
      Map(function(rownum) m[rownum,],1:nrow(m))
   } else Map(function(colnum) m[,colnum],1:ncol(m))
}

dochunk1 <- function(psetstart,x,y,xpx,xpy,allcombs,chunk) {
   ncombs <- length(allcombs)
   lasttask <- min(psetstart+chunk-1,ncombs)
   t(sapply(allcombs[psetstart:lasttask],do1pset1,x,y,xpx,xpy))
}

do1pset1 <- function(onepset,x,y,xpx,xpy) {
   ps <- c(1,onepset+1)  # account for constant term
   x1 <- x[,ps]
   xpx1 <- xpx[ps,ps]
   xpy1 <- xpy[ps]
   ar2 <- linregadjr2(x1,y,xpx1,xpy1)
   n0s <- ncol(x) - length(ps)
   c(ar2,onepset,rep(0,n0s))
}

linregadjr2 <- function(x,y,xpx,xpy) {
   bhat <- solve(xpx,xpy)
   resids <- y - x %*% bhat
   r2 <- 1 - sum(resids^2)/sum((y-mean(y))^2)
   n <-nrow(x); p <- ncol(x) - 1
   1 - (1-r2) * (n-1) / (n-p-1)
}

gendata <- function(n,p) {
   x <<- matrix(rnorm(n*p),ncol=p)
   y <<- x%*%c(rep(0.5,p)) + rnorm(n)
}

test <- function(n,p,k,ncores,chunk=1,dyn=F,rvrs=F) {
  gendata(n,p)
  system.time(mcapr(x,y,k,rvrs,dyn,chunk,ncores=ncores))
}

test(10000, 20, 3, ncores = 2, dyn = FALSE)
test(10000, 20, 3, ncores = 2, dyn = TRUE)  # 动态调度省了4s,把中间的间隙优化,但时间没有本质提升
test(10000, 20, 3, ncores = 2, dyn = TRUE, chunk = 25)
test(10000, 20, 3, ncores = 2, dyn = TRUE, chunk = 50) # 改变块的大小,时间会有明显的变化
test(10000, 20, 3, ncores = 4, dyn = TRUE, chunk = 25)
test(10000, 20, 3, ncores = 4, dyn = TRUE, chunk = 50)


# combn(1:5, 2) 组合数


