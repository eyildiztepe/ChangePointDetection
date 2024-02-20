set.seed(1)
p <- 20 #number of samples
nj <- round(rnorm(p,2000,500)) #sample size
njk <- round(rpois(p,2.8)) #number of changepoints
njk <- ifelse(njk==0, njk+1, njk)
tj <- list() #position of changepoints
for (i in 1:length(nj)){
  tj[[i]] <- sort(round(runif(njk[i],50,nj[i]-50)))}
tj

data_sim <- list()
for (k in 1:p){
  for (j in tj[[k]]){
    n <- c(tj[[k]][1]-1,diff(tj[[k]]),nj[k]-tj[[k]][length(tj[[k]])]+1)
    x <- sapply(n, function(y) rnorm(y,rnorm(1,0,10),runif(1,1,10)))
    data_sim[[k]] <- unlist(x)
  }
}
str(data_sim)
nj[5]
tj[[5]]
plot(data_sim[[5]],type="l",col="blue")
