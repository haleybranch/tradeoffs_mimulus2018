

#For vector
example <- c(1,2,3,4,4,6,7,8,9,10)
vec_l<-length(example[example>=5])
per_pval1 <- vec_l/length(example) 
per_pval1

#For Dataframe
ex_dat <- as.data.frame(example)
ex_l <- length(ex_dat[ex_dat >=5.1])
per_pval2 <- ex_l/dim(ex_dat)[1] 
per_pval2

