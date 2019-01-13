
# mapper.R

trimWhiteSpace <- function(line) {
	gsub("(^ +)|( +$)", "", line)
}

splitIntoWords <- function(line) {
	unlist(strsplit(line, "[[:space:]]+"))
}

con <- file("stdin", open = "r") # 确保数据在hadoop上通信
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
	line <- trimWhiteSpace(line)  # 首尾空格去掉
	words <- splitIntoWords(line)
	for (w in words) {
		cat(w, "\t1\n", sep="")  # 在屏幕里打印
	}
}
close(con)
# line <- "hello world"



# reducer.R
trimWhiteSpace <- function(line) {
	gsub("(^ +)|( +$)", "", line)
}
splitLine <- function(line) {
	val <- unlist(strsplit(line, "\t"))
	list(word = val[1], count = as.integer(val[2]))
}
env <- new.env(hash = TRUE)

con <- file("stdin", open = "r")
while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
	line <- trimWhiteSpace(line)
	split <- splitLine(line)
	word <- split$word
	count <- split$count
	if (exists(word, envir = env, inherits = FALSE)) {
		oldcount <- get(word, envir = env)
		assign(word, oldcount + count, envir = env) # 已经有word就+1,没有就新建一个
	} else {
		assign(word, count, envir = env)
	}
}
close(con)

for (w in ls(env, all = TRUE)) {
	cat(w, "\t", get(w, envir = env), "\n", sep = "")
}

#line <- "hello\t1\n"



# RHadoop

library(rhdfs)
library(rmr2)
wordcount <- function(input, output = NULL){
	mapreduce(input = input, output = output,
		map = function(k, v){
			v2=unlist(strsplit(x=v,split="[[:space:]]+"))
			lapply(v2, function(w){keyval(w,1)})
		},
		reduce=function(k,v) {
			keyval(k,sum(unlist(v)))
		}
	)
}
lines <- c("hello world", "bye world")
lines_dfs <- to.dfs(lines)
wc1 <- wordcount(input=lines_dfs)
do.call(rbind, from.dfs(wc1))


# v <- "hello world"










