
library(parallel)
library(Rdsm)
library(pdist)

####################
##  共享内存简介  ##
####################

# 共享内存并行矩阵乘法
mmulthread <- function(u,v,w) {
   require(parallel)
   myidxs <- splitIndices(nrow(u),myinfo$nwrkrs)[[myinfo$id]]
   w[myidxs,] <- u[myidxs,] %*% v[,]
   0
}

test <- function(cls) {
   mgrinit(cls)
   mgrmakevar(cls,"a",6,2)
   mgrmakevar(cls,"b",2,6)
   mgrmakevar(cls,"c",6,6)
   a[,] <- 1:12
   b[,] <- rep(1,12)
   clusterExport(cls,"mmulthread")
   clusterEvalQ(cls,mmulthread(a,b,c))
   print(c[,])
}

c2 <- makeCluster(2)
test(c2)


# Snow并行矩阵乘法
snowmmul <- function(cls,u,v) {
   require(parallel)
   idxs <- splitIndices(nrow(u),length(cls))
   mmulchunk <- function(idxchunk) u[idxchunk,] %*% v
   res <- clusterApply(cls,idxs,mmulchunk)
   Reduce(rbind,res)
}

testcmp <- function(cls,n) {  
   require(parallel)
   mgrinit(cls)  
   mgrmakevar(cls,"a",n,n)
   mgrmakevar(cls,"c",n,n)
   amat <- matrix(runif(n^2),ncol=n)  
   a[,] <- amat
   clusterExport(cls,"mmulthread")  
   print(system.time(clusterEvalQ(cls,mmulthread(a,a,c))))
   print(system.time(cmat <- snowmmul(cls,amat,amat)))  
}

testcmp(c2, 2000)

# 锁
s <- function(n) {
   for (i in 1:n) {
      tot[1, 1] <- tot[1, 1] + 1
   }
}

clusterExport(c2, "s")
mgrinit(c2)
mgrmakevar(c2, "tot", 1, 1)
tot[1, 1] <- 0
clusterEvalQ(c2, s(1000))
tot[1, 1]

s1 <- function(n) {
   for (i in 1:n) {
      rdsmlock("totlock")
      tot[1, 1] <- tot[1, 1] + 1
      rdsmunlock("totlock")
   }
}

mgrmakelock(c2, "totlock")
tot[1, 1] <- 0
clusterExport(c2, "s1")
clusterEvalQ(c2, s1(1000))
tot[1, 1]

# 屏障
maxburst <- function(x, k, mas, rslts) {
   require(Rdsm)
   require(zoo)
   n <- length(x)
   myidxs <- getidxs(n-k+1)
   myfirst <- myidxs[1]
   mylast <- myidxs[length(myidxs)]
   mas[1, myfirst:mylast] <- rollmean(x[myfirst:(mylast+k-1)], k)
   barr()
   if (myinfo$id == 1) {
      rslts[1, 1] <- which.max(mas[ , ])
      rslts[1, 2] <- mas[1, rslts[1, 1]]
   }
}

test <- function(cls) {
   require(Rdsm)
   mgrinit(cls)
   mgrmakevar(cls, "mas", 1, 9)
   mgrmakevar(cls, "rslts", 1, 2)
   x <<- c(5, 7, 6, 20, 4, 14, 11, 12, 15, 17)
   clusterExport(cls, "maxburst")
   clusterExport(cls, "x")
   clusterEvalQ(cls, maxburst(x, 2, mas, rslts))
   print(rslts[, ])
}

test(c2)



####################
##    应用示例     ##
####################

# 邻接矩阵
findlinks <- function(adj,lnks,counts) {
   require(parallel)
   nr <- nrow(adj)
   myidxs <- getidxs(nr)
   myout <- apply(adj[myidxs,],1,function(rw) which(rw==1))
   tmp <- matrix(nrow=0, ncol=2)
   my1strow <- myidxs[1]
   for (idx in myidxs) {
      tmp <- rbind(tmp, convert1row(idx, myout[[idx - my1strow + 1]]))
   }
   nmyedges <- Reduce(sum,lapply(myout,length)) 
   me <- myinfo$id
   counts[1,me] <- nmyedges
   barr()
   if (me == 1) {
      counts[1,] <- cumsum(counts[1,])
   }
   barr()
   mystart <- if(me == 1) 1 else counts[1, me-1] + 1
   myend <- mystart + nmyedges - 1
   lnks[mystart:myend, ] <- tmp
   0 
}

convert1row <- function(rownum, colswith1s) {
   if (is.null(colswith1s)) return(NULL)
   cbind(rownum, colswith1s)
}

test <- function(cls) {
   mgrinit(cls)
   mgrmakevar(cls,"x",6,6)
   mgrmakevar(cls,"lnks",36,2)
   mgrmakevar(cls,"counts",1,length(cls))
   x[,] <- matrix(sample(0:1,36,replace=T),ncol=6)
   clusterExport(cls,"findlinks")
   clusterExport(cls,"convert1row")
   clusterEvalQ(cls,findlinks(x,lnks,counts))
   print(lnks[1:counts[1,length(cls)],])
}

test(c2)


getlinksnonpar <- function(a,lnks) {
   nr <- nrow(a)
   myout <- apply(a[,],1,function(rw) which(rw==1))
   nmyedges <- Reduce(sum,lapply(myout,length)) 
   lnksidx <- 1
   for (idx in 1:nr) {
      jdx <- idx
      myoj <- myout[[jdx]]
      endwrite <- lnksidx + length(myoj) - 1
      if (!is.null(myoj)) {
         lnks[lnksidx:endwrite, ] <- cbind(idx, myoj)
      }
      lnksidx <- endwrite + 1
   }
   0
}




# KMeans
kmeans <- function(x,k,ni,cntrds,sums,lck,cinit=NULL) {
   require(parallel)
   require(pdist)
   nx <- nrow(x)
   myidxs <- getidxs(nx)
   myx <- x[myidxs,]  
   if (is.null(cinit)) {
      if (myinfo$id == 1) 
         cntrds[,] <- x[sample(1:nx,k,replace=F),]
      barr()
   } else cntrds[,] <- cinit
   mysum <- function(idxs,myx) {
      c(length(idxs),colSums(myx[idxs,,drop=F]))
   }
   for (i in 1:ni) {  # ni iterations
      if (myinfo$id == 1) {
         sums[] <- 0
      }
      barr() 
      dsts <- matrix(pdist(myx,cntrds[,])@dist,ncol=nrow(myx))
      nrst <- apply(dsts,2,which.min)
      tmp <- tapply(1:nrow(myx),nrst,mysum,myx)
      realrdsmlock(lck)
      for (j in as.integer(names(tmp))) {
         sums[j,] <- sums[j,] + tmp[[j]]
      }
      realrdsmunlock(lck)
      barr() 
      if (myinfo$id == 1) {
         for (j in 1:k) {
           if (sums[j,1] > 0) {
              cntrds[j,] <- sums[j,-1] / sums[j,1] 
            } else cntrds[j] <<- x[sample(1:nx,1),]
         }
      }
   }
   0 
}

test <- function(cls) {
   library(parallel)
   mgrinit(cls)
   mgrmakevar(cls,"x",6,2)
   mgrmakevar(cls,"cntrds",2,2)
   mgrmakevar(cls,"sms",2,3)
   mgrmakelock(cls,"lck")
   x[,] <- matrix(sample(1:20,12),ncol=2)
   clusterExport(cls,"kmeans")
   clusterEvalQ(cls,kmeans(x,2,1,cntrds,sms,"lck",
      cinit=rbind(c(5,5),c(15,15))))
}

test1 <- function(cls) {
   mgrinit(cls)
   mgrmakevar(cls,"x",10000,3)
   mgrmakevar(cls,"cntrds",3,3)
   mgrmakevar(cls,"sms",3,4)
   mgrmakelock(cls,"lck")
   x[,] <- matrix(rnorm(30000),ncol=3)
   ri <- sample(1:10000,3000)
   x[ri,1] <- x[ri,1] + 5
   ri <- sample(1:10000,3000)
   x[ri,2] <- x[ri,2] + 5
   clusterExport(cls,"kmeans")
   clusterEvalQ(cls,kmeans(x,3,50,cntrds,sms,"lck"))
}

test(c2)
test1(c2)
