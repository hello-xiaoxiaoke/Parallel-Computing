

env1 <- new.env()

is.environment(env1)

ls()
ls(envir = env1)

assign("x", 2, envir = env1)
 

search()

x <- 1
f1 <- function() {
	x <- 3
	x
}
x
f2 <- function() {
	x <<- 3
	x
}
x

options()

getOption("digits")

pi

options(digits = 10)



Sys.info()
Sys.getlocale()
.Platform
R.home()
R.Version()
.libPaths()

names(Sys.getenv())



test.trycatch <- function(arg1) {
	returnthis <- tryCatch({
		if (missing(arg1)) Sys.sleep(5)
		abs(log(arg1))
	}, interrupt = function(i) {
		returnstr <- gettext(i)
		cat(paste("Status :", returnstr))
		return(-1)
	}, error = function(e) {
		returnstr <- gettext(e)
		cat(paste("Status :", returnstr))
		if(grepl("mathematical",returnstr)) {
			return(-2.1)
		} else {
			return(-2.2)
		}
	}, warning = function(w) {
		returnstr <- gettext(w)
		cat(paste("Status :", returnstr))
		return(-3)
	}, finally = {
		cat("Done...\n")
	}
	)
	return(returnthis) 
}		

test.trycatch(2)
test.trycatch()
test.trycatch("1")
test.trycatch(1 + "1")
test.trycatch(-1)

f3 <- function() {
	x1 = 1
	x2 = 2
	browser()
	x3 = 3
	x1 + x2 + x3
}



