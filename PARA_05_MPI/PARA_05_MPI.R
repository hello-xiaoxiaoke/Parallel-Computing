
library(Rmpi)

# 简单的例子
mpi.spawn.Rslaves() 

mpi.remote.exec(paste("I am", mpi.comm.rank(), "of", mpi.comm.size()))

lmdata <- data.frame(y = rnorm(10000), x = rnorm(10000))
sampleid <- sample(1:4, 10000, replace = TRUE)

slavefun <- function() {
	tmp.data <- lmdata[which(sampleid == slaveID), ]
	tmp.model <- lm(y~x+0, data = tmp.data)
	tmp.sum <- summary(tmp.model)
	return(c(Est=tmp.sum$coefficients["x","Estimate"],
	P = tmp.sum$coefficients["x", "Pr(>|t|)"]))
}

mpi.bcast.Robj2slave(lmdata)
mpi.bcast.Robj2slave(sampleid)
mpi.bcast.Robj2slave(slavefun)
mpi.bcast.cmd(slaveID <- mpi.comm.rank())

res <- mpi.remote.exec(slavefun(), simplify = FALSE)

mpi.close.Rslaves()
mpi.exit()


# 串行计算素数

serprime <- function(n) {
   nums <- 1:n
   prime <- rep(1,n)
   maxdiv <- ceiling(sqrt(n))
   for (d in 2:maxdiv) {
      if (prime[d])
         prime[prime !=0 & nums > d & nums %% d == 0] <- 0
   }
   nums[prime != 0 & nums >= 2]
}

serprime(100)

system.time(serprime(1000000))


# 判断 x 是否能被 divs 整除
dosieve <- function(x, divs) {
   for (d in divs) {
      x <- x[x %% d != 0 | x == d]
   }
   x
}

dosieve(1:100, c(2, 3, 5, 7))

# 主机代码
primepipe <- function(n,divisors,msgsize) {
   mpi.bcast.Robj2slave(dowork)
   mpi.bcast.Robj2slave(dosieve)
   # start workers; note nonblocking call
   mpi.bcast.cmd(dowork,n,divisors,msgsize)
   # remove the evens right away
   odds <- seq(from=3,to=n,by=2)
   nodd <- length(odds)
   # send odds to node 1, in chunks 
   startmsg <- seq(from=1,to=nodd,by=msgsize)
   for (s in startmsg) {
      rng <- s:min(s+msgsize-1,nodd)
      mpi.send.Robj(odds[rng],tag=0,dest=1)
   }
   # send end-data sentinel
   mpi.send.Robj(NA,tag=0,dest=1)
   # receive results from last node
   lastnode <- mpi.comm.size()-1
   # return te result; don't forget the 2
   c(2,mpi.recv.Robj(tag=0,source=lastnode))
}

# 从机代码
dowork <- function(n,divisors,msgsize) {
   me <- mpi.comm.rank()
   lastnode <- mpi.comm.size()-1
   ld <- length(divisors)
   tmp <- floor(ld / lastnode)
   mystart <- (me-1) * tmp + 1
   myend <- mystart + tmp - 1
   if (me == lastnode) myend <- ld
   mydivs <- divisors[mystart:myend]
   if (me == lastnode) out <- NULL
   repeat {
      msg <- mpi.recv.Robj(tag=0,source=me-1)
      if (me < lastnode) {
         if (!is.na(msg[1])) {
            sieveout <- dosieve(msg,mydivs)
            mpi.send.Robj(sieveout,tag=0,dest=me+1)
         } else {  
            mpi.send.Robj(NA,tag=0,dest=me+1)
            return()
         }
      } else {
         if (!is.na(msg[1])) {
            sieveout <- dosieve(msg,mydivs)
            out <- c(out,sieveout)
         } else {  
            mpi.send.Robj(out,tag=0,dest=0)
            return()
         }
      }
   }
}


dvs <- serprime(ceiling(sqrt(1000)))
primepipe(1000, dvs, 100)

system.time(primepipe(1000000, dvs, 100))
system.time(primepipe(1000000, dvs, 10000))



# 单步运行从机函数
library(Rmpi)
n <- 100
divisors <-c(2,3,5,7)
me <- 2
lastnode <- mpi.comm.size()-1
ld <- length(divisors)
tmp <- floor(ld / lastnode)
mystart <- (me-1) * tmp + 1
myend <- mystart + tmp - 1
mydivs <- divisors[mystart:myend]
sieveout <- dosieve(msg,mydivs)
sieveout

