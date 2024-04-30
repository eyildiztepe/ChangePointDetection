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

## yuklenecek paketler ##

install.packages('RJSONIO')
library(RJSONIO)
library(jsonlite)
install.packages("changepoint")
library(changepoint)
library(strucchange)
library(ggplot2)
install.packages("segmented")
library(segmented)
library(zoo)
install.packages('prophet')
library(prophet)

# AMOC #

#default
cpts_amoc <- list()
par(mfrow = c(5, 4), mar = c(2, 2, 1, 1))
for (k in 1:p) {
  d_amoc <- cpt.mean(data_sim[[k]], method = "AMOC")
  cpts_amoc[[k]] <- cpts(d_amoc)
  plot(data_sim[[k]], type = "l", col = "blue", main = paste("Dataset ", k))
  abline(v = cpts_amoc[[k]], col = "red", lty = 2, lwd = 2)
}
print(cpts_amoc)

# oracle settings
cpts_amoc1 <- list()
par(mfrow = c(1, 3))
for (k in 1:p) {
  if (k %in% c(7, 15, 16)) {
    o_amoc1 <- cpt.mean(data_sim[[k]], method = "AMOC")
    cpts_amoc1[[k]] <- cpts(o_amoc1)
    plot(data_sim[[k]], type = "l", col = "blue", main = paste("Dataset ", k))
    abline(v = cpts_amoc1[[k]], col = "red", lty = 2, lwd = 2)
  }
}
cpts_amoc1 <- cpts_amoc1[!sapply(cpts_amoc1, is.null)]

print(cpts_amoc1)

# BINSEG #

# default
cpts_binseg <- list()
par(mfrow = c(5, 4), mar = c(2, 2, 1, 1))
for (k in 1:p) {
  d_binseg <- cpt.mean(data_sim[[k]], method = "BinSeg")
  cpts_binseg[[k]] <- cpts(d_binseg)
  plot(data_sim[[k]], type = "l", col = "blue", main = paste("Dataset ", k))
  abline(v = cpts_binseg[[k]], col = "red", lty = 2, lwd = 2)
}
print(cpts_binseg)

# oracle settings
# 1 cp icin
cpts_binseg1 <- list()
par(mfrow = c(1,3))
for (k in 1:p) {
  if (k %in% c(7, 15, 16)) {
    o_binseg1 <- cpt.mean(data_sim[[k]], method = "BinSeg", Q = 1)
    cpts_binseg1[[k]] <- cpts(o_binseg1)
    plot(data_sim[[k]], type = "l", col = "blue", main = paste("Dataset ", k))
    abline(v = cpts_binseg1[[k]], col = "red", lty = 2, lwd = 2)
  }
}
cpts_binseg1 <- cpts_binseg1[!sapply(cpts_binseg1, is.null)]

print(cpts_binseg1)

#2 cp icin

cpts_binseg2 <- list()
par(mfrow = c(2,2))
for (k in 1:p) {
  if (k %in% c(13, 14, 17,20)) {
    o_binseg2 <- cpt.mean(data_sim[[k]], method = "BinSeg", Q = 2)
    cpts_binseg2[[k]] <- cpts(o_binseg2)
    plot(data_sim[[k]], type = "l", col = "blue", main = paste("Dataset ", k))
    abline(v = cpts_binseg2[[k]], col = "red", lty = 2, lwd = 2)
  }
}
cpts_binseg2 <- cpts_binseg2[!sapply(cpts_binseg2, is.null)]

print(cpts_binseg2)

#3 cp icin

cpts_binseg3 <- list()
par(mfrow = c(4, 2), mar = c(3, 3, 2, 1))
for (k in 1:p) {
  if (k %in% c(2,4,5,8,11,18,19)) {
    o_binseg3 <- cpt.mean(data_sim[[k]], method = "BinSeg", Q = 3)
    cpts_binseg3[[k]] <- cpts(o_binseg3)
    plot(data_sim[[k]], type = "l", col = "blue", main = paste("Dataset ", k))
    abline(v = cpts_binseg3[[k]], col = "red", lty = 2, lwd = 2)
  }
}
cpts_binseg3 <- cpts_binseg3[!sapply(cpts_binseg3, is.null)]

print(cpts_binseg3)

#4 cp icin

cpts_binseg4 <- list()
par(mfrow = c(3,2))
for (k in 1:p) {
  if (k %in% c(1,3,6,9,10)) {
    o_binseg4 <- cpt.mean(data_sim[[k]], method = "BinSeg", Q = 4)
    cpts_binseg4[[k]] <- cpts(o_binseg4)
    plot(data_sim[[k]], type = "l", col = "blue", main = paste("Dataset ", k))
    abline(v = cpts_binseg4[[k]], col = "red", lty = 2, lwd = 2)
  }
}
cpts_binseg4 <- cpts_binseg4[!sapply(cpts_binseg4, is.null)]

print(cpts_binseg4)

#5 cp icin

cpts_binseg5 <- list()
par(mfrow = c(1,1))
for (k in 1:p) {
  if (k %in% c(12)) {
    o_binseg5 <- cpt.mean(data_sim[[k]], method = "BinSeg", Q = 5)
    cpts_binseg5[[k]] <- cpts(o_binseg5)
    plot(data_sim[[k]], type = "l", col = "blue", main = paste("Dataset ", k))
    abline(v = cpts_binseg5[[k]], col = "red", lty = 2, lwd = 2)
  }
}
cpts_binseg5 <- cpts_binseg5[!sapply(cpts_binseg5, is.null)]

print(cpts_binseg5)

# PELT #

##veri_1 default 
nj[1]
tj[[1]]
plot(data_sim[[1]],type="l",col="blue")

m_pelt_1 <- cpt.mean(data_sim[[1]], method = "PELT",  minseglen = 250)
plot(m_pelt_1, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(m_pelt_1), col = "blue", lty = 2,lwd=2)
cpts(m_pelt_1) 

##veri_1 oracle 
o_pelt1 <- cpt.mean(data_sim[[1]], method = "PELT", penalty = "BIC", pen.value = 0.05, minseglen = 270)
plot(o_pelt1, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(o_pelt1), col = "blue", lty = 2,lwd=1)
cpts(o_pelt1) 

##veri_2 default
nj[2]
tj[[2]]
plot(data_sim[[2]],type="l",col="blue")

m_pelt_2 <- cpt.mean(data_sim[[2]], method = "PELT",  minseglen = 250)
plot(m_pelt_2, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(m_pelt_2), col = "blue", lty = 2,lwd=2)
cpts(m_pelt_2)   

##veri_2 oracle
o_pelt2 <- cpt.mean(data_sim[[2]], method = "PELT", penalty = "BIC", pen.value = 0.05, minseglen = 400)
plot(o_pelt2, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(o_pelt2), col = "blue", lty = 2,lwd=1)
cpts(o_pelt2) 

##veri_3 default
nj[3]
tj[[3]]
plot(data_sim[[3]],type="l",col="blue")

m_pelt_3 <- cpt.mean(data_sim[[3]], method = "PELT",  minseglen = 250)
plot(m_pelt_3, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(m_pelt_3), col = "blue", lty = 2,lwd=2)
cpts(m_pelt_3)

##veri_3 oracle
o_pelt3 <- cpt.mean(data_sim[[3]], method = "PELT", penalty = "BIC", pen.value = 0.05, minseglen = 200)
plot(o_pelt3, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(o_pelt3), col = "blue", lty = 2,lwd=1)
cpts(o_pelt3) 

##veri_4 default
nj[4]
tj[[4]]
plot(data_sim[[4]],type="l",col="blue")

m_pelt_4 <- cpt.mean(data_sim[[4]], method = "PELT",  minseglen = 250)
plot(m_pelt_4, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(m_pelt_4), col = "blue", lty = 2,lwd=2)
cpts(m_pelt_4)

##veri_4 oracle
o_pelt4 <- cpt.mean(data_sim[[4]], method = "PELT", penalty = "BIC", pen.value = 0.05, minseglen = 310)
plot(o_pelt4, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(o_pelt4), col = "blue", lty = 2,lwd=1)
cpts(o_pelt4) 

##veri_5 default
nj[5]
tj[[5]]
plot(data_sim[[5]],type="l",col="blue")

m_pelt_5 <- cpt.mean(data_sim[[5]], method = "PELT",  minseglen = 750)
plot(m_pelt_5, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(m_pelt_5), col = "blue", lty = 2,lwd=2)
cpts(m_pelt_5)

##veri_5 oracle 
o_pelt5 <- cpt.mean(data_sim[[5]], method = "PELT", penalty = "BIC", pen.value = 0.05, minseglen = 650)
plot(o_pelt5, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(o_pelt5), col = "blue", lty = 2,lwd=1)
cpts(o_pelt5) 

##veri_6 default
nj[6]
tj[[6]]
plot(data_sim[[6]],type="l",col="blue")

m_pelt_6 <- cpt.mean(data_sim[[6]], method = "PELT",  minseglen = 110)
plot(m_pelt_6, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(m_pelt_6), col = "blue", lty = 2,lwd=2)
cpts(m_pelt_6)

##veri_6 oracle
o_pelt6 <- cpt.mean(data_sim[[6]], method = "PELT", penalty = "BIC", pen.value = 0.05, minseglen = 107)
plot(o_pelt6, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(o_pelt6), col = "blue", lty = 2,lwd=1)
cpts(o_pelt6)


##veri_7 default 
nj[7]
tj[[7]]
plot(data_sim[[7]],type="l",col="blue")

m_pelt_7 <- cpt.mean(data_sim[[7]], method = "PELT",  minseglen = 850)
plot(m_pelt_7, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(m_pelt_7), col = "blue", lty = 2,lwd=2)
cpts(m_pelt_7) 

##veri_7 oracle
o_pelt7 <- cpt.mean(data_sim[[7]], method = "PELT", penalty = "BIC", pen.value = 0.05, minseglen = 703)
plot(o_pelt7, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(o_pelt7), col = "blue", lty = 2,lwd=1)
cpts(o_pelt7)   


##veri_8 default 
nj[8]
tj[[8]]
plot(data_sim[[8]],type="l",col="blue")

m_pelt_8 <- cpt.mean(data_sim[[8]], method = "PELT",  minseglen = 180)
plot(m_pelt_8, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(m_pelt_8), col = "blue", lty = 2,lwd=2)
cpts(m_pelt_8)

##veri8
o_pelt8 <- cpt.mean(data_sim[[8]], method = "PELT", penalty = "BIC", pen.value = 0.05, minseglen = 250)
plot(o_pelt8, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(o_pelt8), col = "blue", lty = 2,lwd=1)
cpts(o_pelt8) 


##veri_9 default 
nj[9]
tj[[9]]
plot(data_sim[[9]],type="l",col="blue")

m_pelt_9 <- cpt.mean(data_sim[[9]], method = "PELT",  minseglen = 300)
plot(m_pelt_9, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(m_pelt_9), col = "blue", lty = 2,lwd=2)
cpts(m_pelt_9)

##veri_9 oracle
o_pelt9 <- cpt.mean(data_sim[[9]], method = "PELT", penalty = "BIC", pen.value = 0.05, minseglen = 295)
plot(o_pelt9, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(o_pelt9), col = "blue", lty = 2,lwd=1)
cpts(o_pelt9) 

##veri_10 default 
nj[10]
tj[[10]]
plot(data_sim[[10]],type="l",col="blue")

m_pelt_10 <- cpt.mean(data_sim[[10]], method = "PELT",  minseglen = 200)
plot(m_pelt_10, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(m_pelt_10), col = "blue", lty = 2,lwd=2)
cpts(m_pelt_10)

##veri_10 oracle
o_pelt10 <- cpt.mean(data_sim[[10]], method = "PELT", penalty = "BIC", pen.value = 0.05, minseglen = 180)
plot(o_pelt10, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(o_pelt10), col = "blue", lty = 2,lwd=1)
cpts(o_pelt10)

## veri11 default
nj[11]
tj[[11]]
plot(data_sim[[11]],type="l",col="blue")

d_pelt11<- cpt.mean(data_sim[[11]], method = "PELT", penalty = "BIC", pen.value = 0.05, minseglen = 310)
plot(d_pelt11, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(d_pelt11), col = "blue", lty = 2,lwd=1)
cpts(d_pelt11)   

##veri11 oracle
o_pelt11 <- cpt.mean(data_sim[[11]], method = "PELT", penalty = "BIC", pen.value = 0.05, minseglen = 315)
plot(o_pelt11, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(o_pelt11), col = "blue", lty = 2,lwd=1)
cpts(o_pelt11) 

## veri12 default
nj[12]
tj[[12]]
plot(data_sim[[12]],type="l",col="blue")

d_pelt12<- cpt.mean(data_sim[[12]], method = "PELT", penalty = "BIC", pen.value = 0.05, minseglen = 280)
plot(d_pelt12, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(d_pelt12), col = "blue", lty = 2,lwd=1)
cpts(d_pelt12) 

##veri12 oracle
o_pelt12 <- cpt.mean(data_sim[[12]], method = "PELT", penalty = "BIC", pen.value = 0.05, minseglen = 265)
plot(o_pelt12, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(o_pelt12), col = "blue", lty = 2,lwd=1)
cpts(o_pelt12) 

## veri13 default
nj[13]
tj[[13]]
plot(data_sim[[13]],type="l",col="blue")

d_pelt13<- cpt.mean(data_sim[[13]], method = "PELT", penalty = "BIC", pen.value = 0.05, minseglen = 320)
plot(d_pelt13, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(d_pelt13), col = "blue", lty = 2,lwd=1)
cpts(d_pelt13) 

##veri13 oracle
o_pelt13 <- cpt.mean(data_sim[[13]], method = "PELT", penalty = "BIC", pen.value = 0.05, minseglen = 340)
plot(o_pelt13, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(o_pelt13), col = "blue", lty = 2,lwd=1)
cpts(o_pelt13)  

## veri14 default
nj[14]
tj[[14]]
plot(data_sim[[14]],type="l",col="blue")

d_pelt14<- cpt.mean(data_sim[[14]], method = "PELT", penalty = "BIC", pen.value = 0.05, minseglen = 140)
plot(d_pelt14, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(d_pelt14), col = "blue", lty = 2,lwd=1)
cpts(d_pelt14) 

##veri14 oracle
o_pelt14 <- cpt.mean(data_sim[[14]], method = "PELT", penalty = "BIC", pen.value = 0.05, minseglen = 145)
plot(o_pelt14, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(o_pelt14), col = "blue", lty = 2,lwd=1)
cpts(o_pelt14)  

## veri15 default
nj[15]
tj[[15]]
plot(data_sim[[15]],type="l",col="blue")

d_pelt15<- cpt.mean(data_sim[[15]], method = "PELT", penalty = "BIC", pen.value = 0.05, minseglen = 820)
plot(d_pelt15, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(d_pelt15), col = "blue", lty = 2,lwd=1)
cpts(d_pelt15) 

## veri15 oracle
o_pelt15 <- cpt.mean(data_sim[[15]], method = "PELT", penalty = "BIC", pen.value = 0.05, minseglen = 830)
plot(o_pelt15, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(o_pelt15), col = "blue", lty = 2,lwd=1)
cpts(o_pelt15)  

## veri16 default
nj[16]
tj[[16]]
plot(data_sim[[16]],type="l",col="blue")

d_pelt16<- cpt.mean(data_sim[[16]], method = "PELT", penalty = "BIC", pen.value = 0.05, minseglen = 650)
plot(d_pelt16, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(d_pelt16), col = "blue", lty = 2,lwd=1)
cpts(d_pelt16) 

## veri16 oracle
o_pelt16 <- cpt.mean(data_sim[[16]], method = "PELT", penalty = "BIC", pen.value = 0.05, minseglen = 660)
plot(o_pelt16, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(o_pelt16), col = "blue", lty = 2,lwd=1)
cpts(o_pelt16)

## veri17 default
nj[17]
tj[[17]]
plot(data_sim[[17]],type="l",col="blue")

d_pelt17<- cpt.mean(data_sim[[17]], method = "PELT", penalty = "BIC", pen.value = 0.05, minseglen = 450)
plot(d_pelt17, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(d_pelt17), col = "blue", lty = 2,lwd=1)
cpts(d_pelt17) 

##veri17 oracle
o_pelt17 <- cpt.mean(data_sim[[17]], method = "PELT", penalty = "BIC", pen.value = 0.05, minseglen = 480)
plot(o_pelt17, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(o_pelt17), col = "blue", lty = 2,lwd=1)
cpts(o_pelt17) 

## veri18 default
nj[18]
tj[[18]]
plot(data_sim[[18]],type="l",col="blue")

d_pelt18<- cpt.mean(data_sim[[18]], method = "PELT", penalty = "BIC", pen.value = 0.05, minseglen = 340)
plot(d_pelt18, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(d_pelt18), col = "blue", lty = 2,lwd=1)
cpts(d_pelt18) 

##veri18 oracle
o_pelt18 <- cpt.mean(data_sim[[18]], method = "PELT", penalty = "BIC", pen.value = 0.05, minseglen = 500)
plot(o_pelt18, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(o_pelt18), col = "blue", lty = 2,lwd=1)
cpts(o_pelt18)  

## veri19 default
nj[19]
tj[[19]]
plot(data_sim[[19]],type="l",col="blue")

d_pelt19<- cpt.mean(data_sim[[19]], method = "PELT", penalty = "BIC", pen.value = 0.05, minseglen = 400)
plot(d_pelt19, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(d_pelt19), col = "blue", lty = 2,lwd=1)
cpts(d_pelt19) 

##veri19 oracle
o_pelt19 <- cpt.mean(data_sim[[19]], method = "PELT", penalty = "BIC", pen.value = 0.05, minseglen = 500)
plot(o_pelt19, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(o_pelt19), col = "blue", lty = 2,lwd=1)
cpts(o_pelt19) 

## veri20 default
nj[20]
tj[[20]]
plot(data_sim[[20]],type="l",col="blue")

d_pelt20<- cpt.mean(data_sim[[20]], method = "PELT", penalty = "BIC", pen.value = 0.05, minseglen = 500)
plot(d_pelt20, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(d_pelt20), col = "blue", lty = 2,lwd=1)
cpts(d_pelt20) 

##veri20 oracle
o_pelt20 <- cpt.mean(data_sim[[20]], method = "PELT", penalty = "BIC", pen.value = 0.05, minseglen = 545)
plot(o_pelt20, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(o_pelt20), col = "blue", lty = 2,lwd=1)
cpts(o_pelt20)  

# PROPHET #

##veri_1 default 
nj[1]
tj[[1]]
plot(data_sim[[1]],type="l",col="blue")

df_prophet1 <- data.frame(
  ds = index(data_sim[[1]]), 
  y = data_sim[[1]] 
)

#veri_1 
d_model1 <- prophet(df_prophet1, n.changepoints = 2)
d_changepoints1 <- d_model1$changepoints
d_changepoints1 <- as.numeric(d_changepoints1)
print(d_changepoints1)

#veri_1 oracle
o_model1 <- prophet(df_prophet1,n.changepoints = 1)
o_changepoints1 <- o_model1$changepoints
o_changepoints1 <- as.numeric(o_changepoints1)
print(o_changepoints1)

##veri_2 
nj[2]
tj[[2]]
plot(data_sim[[2]],type="l",col="blue")

df_prophet2 <- data.frame(
  ds = index(data_sim[[2]]), 
  y = data_sim[[2]] 
)

#veri_2 default 
d_model2 <- prophet(df_prophet2, n.changepoints = 5)
d_changepoints2 <- d_model2$changepoints
d_changepoints2 <- as.numeric(d_changepoints2)
print(d_changepoints2)

#veri_2 oracle
o_model2 <- prophet(df_prophet2,n.changepoints = 6)
o_changepoints2 <- o_model2$changepoints
o_changepoints2 <- as.numeric(o_changepoints2)
print(o_changepoints2)

##veri_3 
nj[3]
tj[[3]]
plot(data_sim[[3]],type="l",col="blue")

df_prophet3 <- data.frame(
  ds = index(data_sim[[3]]), 
  y = data_sim[[3]] 
)

#veri_3 default 
d_model3 <- prophet(df_prophet3, n.changepoints = 4)
d_changepoints3 <- d_model3$changepoints
d_changepoints3 <- as.numeric(d_changepoints3)
print(d_changepoints3)

#veri_3 oracle
o_model3 <- prophet(df_prophet3,n.changepoints = 2)
o_changepoints3<- o_model3$changepoints
o_changepoints3 <- as.numeric(o_changepoints3)
print(o_changepoints3)

##veri_4 
nj[4]
tj[[4]]
plot(data_sim[[4]],type="l",col="blue")

df_prophet4 <- data.frame(
  ds = index(data_sim[[4]]), 
  y = data_sim[[4]] 
)

#veri_4 default 
d_model4 <- prophet(df_prophet4, n.changepoints = 4)
d_changepoints4 <- d_model4$changepoints
d_changepoints4 <- as.numeric(d_changepoints4)
print(d_changepoints4)

#veri_4 oracle
o_model4 <- prophet(df_prophet4,n.changepoints = 3)
o_changepoints4<- o_model4$changepoints
o_changepoints4 <- as.numeric(o_changepoints4)
print(o_changepoints4)

##veri_5 
nj[5]
tj[[5]]
plot(data_sim[[5]],type="l",col="blue")

df_prophet5 <- data.frame(
  ds = index(data_sim[[5]]), 
  y = data_sim[[5]] 
)

#veri_5 default 
d_model5 <- prophet(df_prophet5, n.changepoints = 3)
d_changepoints5 <- d_model5$changepoints
d_changepoints5 <- as.numeric(d_changepoints5)
print(d_changepoints5)

#veri_5 oracle
o_model5 <- prophet(df_prophet5,n.changepoints = 2)
o_changepoints5<- o_model5$changepoints
o_changepoints5 <- as.numeric(o_changepoints5)
print(o_changepoints5)

##veri_6 
nj[6]
tj[[6]]
plot(data_sim[[6]],type="l",col="blue")

df_prophet6 <- data.frame(
  ds = index(data_sim[[6]]), 
  y = data_sim[[6]] 
)

#veri_6 default 
d_model6 <- prophet(df_prophet6, n.changepoints = 7)
d_changepoints6 <- d_model6$changepoints
d_changepoints6 <- as.numeric(d_changepoints6)
print(d_changepoints6)

#veri_6 oracle
o_model6 <- prophet(df_prophet6,n.changepoints = 6)
o_changepoints6<- o_model6$changepoints
o_changepoints6 <- as.numeric(o_changepoints6)
print(o_changepoints6)

##veri_7 
nj[7]
tj[[7]]
plot(data_sim[[7]],type="l",col="blue")

df_prophet7 <- data.frame(
  ds = index(data_sim[[7]]), 
  y = data_sim[[7]] 
)

#veri_7 default 
d_model7 <- prophet(df_prophet7, n.changepoints = 3)
d_changepoints7 <- d_model7$changepoints
d_changepoints7 <- as.numeric(d_changepoints7)
print(d_changepoints7)

#veri_7 oracle
o_model7 <- prophet(df_prophet7,n.changepoints = 2)
o_changepoints7<- o_model7$changepoints
o_changepoints7 <- as.numeric(o_changepoints7)
print(o_changepoints7)

##veri_8 
nj[8]
tj[[8]]
plot(data_sim[[8]],type="l",col="blue")

df_prophet8 <- data.frame(
  ds = index(data_sim[[8]]), 
  y = data_sim[[8]] 
)

#veri_8 default 
d_model8 <- prophet(df_prophet8, n.changepoints = 4)
d_changepoints8 <- d_model8$changepoints
d_changepoints8 <- as.numeric(d_changepoints8)
print(d_changepoints8)

#veri_8 oracle
o_model8 <- prophet(df_prophet8,n.changepoints = 3)
o_changepoints8<- o_model8$changepoints
o_changepoints8 <- as.numeric(o_changepoints8)
print(o_changepoints8)


##veri_9 
nj[9]
tj[[9]]
plot(data_sim[[9]],type="l",col="blue")

df_prophet9 <- data.frame(
  ds = index(data_sim[[9]]), 
  y = data_sim[[9]] 
)

#veri_9 default 
d_model9 <- prophet(df_prophet9, n.changepoints = 5)
d_changepoints9 <- d_model9$changepoints
d_changepoints9 <- as.numeric(d_changepoints9)
print(d_changepoints9)

#veri_9 oracle
o_model9 <- prophet(df_prophet9,n.changepoints = 3)
o_changepoints9<- o_model9$changepoints
o_changepoints9 <- as.numeric(o_changepoints9)
print(o_changepoints9)


##veri_10 
nj[10]
tj[[10]]
plot(data_sim[[10]],type="l",col="blue")

df_prophet10 <- data.frame(
  ds = index(data_sim[[10]]), 
  y = data_sim[[10]] 
)

#veri_10 default 
d_model10 <- prophet(df_prophet10, n.changepoints = 6)
d_changepoints10 <- d_model10$changepoints
d_changepoints10 <- as.numeric(d_changepoints10)
print(d_changepoints10)

#veri_10 oracle
o_model10 <- prophet(df_prophet10,n.changepoints = 4)
o_changepoints10<- o_model10$changepoints
o_changepoints10 <- as.numeric(o_changepoints10)
print(o_changepoints10)

# veri 11
nj[11]
tj[[11]]
plot(data_sim[[11]],type="l",col="blue")

df_prophet11 <- data.frame(
  ds = index(data_sim[[11]]), # Tarih sC<tunu
  y = data_sim[[11]] # DeDer sC<tunu
)

# veri11 default
d_model11 <- prophet(df_prophet11,n.changepoints = 4)
d_changepoints11 <- d_model11$changepoints
d_changepoints11 <- as.numeric(d_changepoints11)
print(d_changepoints11)

# veri11 oracle
o_model11 <- prophet(df_prophet11,n.changepoints = 3)
o_changepoints11 <- o_model11$changepoints
o_changepoints11 <- as.numeric(o_changepoints11)
print(o_changepoints11)

# veri 12
nj[12]
tj[[12]]
plot(data_sim[[12]],type="l",col="blue")

df_prophet12 <- data.frame(
  ds = index(data_sim[[12]]), # Tarih sC<tunu
  y = data_sim[[12]] # DeDer sC<tunu
)

# veri12 default
d_model12 <- prophet(df_prophet12,n.changepoints = 4)
d_changepoints12 <- d_model12$changepoints
d_changepoints12 <- as.numeric(d_changepoints12)
print(d_changepoints12)

# veri12 oracle
o_model12 <- prophet(df_prophet12,n.changepoints = 5)
o_changepoints12 <- o_model12$changepoints
o_changepoints12 <- as.numeric(o_changepoints12)
print(o_changepoints12)

# veri 13
nj[13]
tj[[13]]
plot(data_sim[[13]],type="l",col="blue")

df_prophet13 <- data.frame(
  ds = index(data_sim[[13]]), # Tarih sC<tunu
  y = data_sim[[13]] # DeDer sC<tunu
)

# veri13 default
d_model13 <- prophet(df_prophet13,n.changepoints = 3)
d_changepoints13 <- d_model13$changepoints
d_changepoints13 <- as.numeric(d_changepoints13)
print(d_changepoints13)

# veri13 oracle
o_model13 <- prophet(df_prophet13,n.changepoints = 2)
o_changepoints13 <- o_model13$changepoints
o_changepoints13 <- as.numeric(o_changepoints13)
print(o_changepoints13)

# veri 14
nj[14]
tj[[14]]
plot(data_sim[[14]],type="l",col="blue")

df_prophet14 <- data.frame(
  ds = index(data_sim[[14]]), # Tarih sC<tunu
  y = data_sim[[14]] # DeDer sC<tunu
)

# veri14 default
d_model14 <- prophet(df_prophet14,n.changepoints = 3)
d_changepoints14 <- d_model14$changepoints
d_changepoints14 <- as.numeric(d_changepoints14)
print(d_changepoints14)

# veri14 oracle
o_model14 <- prophet(df_prophet14,n.changepoints = 2)
o_changepoints14 <- o_model14$changepoints
o_changepoints14<- as.numeric(o_changepoints14)
print(o_changepoints14)

# veri 15
nj[15]
tj[[15]]
plot(data_sim[[15]],type="l",col="blue")

df_prophet15 <- data.frame(
  ds = index(data_sim[[15]]), # Tarih sC<tunu
  y = data_sim[[15]] # DeDer sC<tunu
)

# veri15 default
d_model15 <- prophet(df_prophet15,n.changepoints = 2)
d_changepoints15 <- d_model15$changepoints
d_changepoints15 <- as.numeric(d_changepoints15)
print(d_changepoints15)

# veri15 oracle
o_model15 <- prophet(df_prophet15,n.changepoints = 1)
o_changepoints15 <- o_model15$changepoints
o_changepoints15 <- as.numeric(o_changepoints15)
print(o_changepoints15)

# veri 16
nj[16]
tj[[16]]
plot(data_sim[[16]],type="l",col="blue")

df_prophet16 <- data.frame(
  ds = index(data_sim[[16]]), # Tarih sC<tunu
  y = data_sim[[16]] # DeDer sC<tunu
)

# veri16 default
d_model16 <- prophet(df_prophet16,n.changepoints = 2)
d_changepoints16 <- d_model16$changepoints
d_changepoints16 <- as.numeric(d_changepoints16)
print(d_changepoints16)

# veri16 oracle
o_model16 <- prophet(df_prophet16,n.changepoints = 1)
o_changepoints16 <- o_model16$changepoints
o_changepoints16 <- as.numeric(o_changepoints16)
print(o_changepoints16)

# veri 17
nj[17]
tj[[17]]
plot(data_sim[[17]],type="l",col="blue")

df_prophet17 <- data.frame(
  ds = index(data_sim[[17]]), # Tarih sC<tunu
  y = data_sim[[17]] # DeDer sC<tunu
)

# veri17 default
d_model17 <- prophet(df_prophet17,n.changepoints = 3)
d_changepoints17 <- d_model17$changepoints
d_changepoints17 <- as.numeric(d_changepoints17)
print(d_changepoints17)

# veri17 oracle
o_model17<- prophet(df_prophet17,n.changepoints = 2)
o_changepoints17 <- o_model17$changepoints
o_changepoints17 <- as.numeric(o_changepoints17)
print(o_changepoints17)

# veri 18
nj[18]
tj[[18]]
plot(data_sim[[18]],type="l",col="blue")

df_prophet18 <- data.frame(
  ds = index(data_sim[[18]]), # Tarih sC<tunu
  y = data_sim[[18]] # DeDer sC<tunu
)

# veri18 default
d_model18 <- prophet(df_prophet18,n.changepoints = 4)
d_changepoints18 <- d_model18$changepoints
d_changepoints18 <- as.numeric(d_changepoints18)
print(d_changepoints18)

# veri18 oracle
o_model18 <- prophet(df_prophet18,n.changepoints = 3)
o_changepoints18 <- o_model18$changepoints
o_changepoints18 <- as.numeric(o_changepoints18)
print(o_changepoints18)

# veri 19
nj[19]
tj[[19]]
plot(data_sim[[19]],type="l",col="blue")

df_prophet19 <- data.frame(
  ds = index(data_sim[[19]]), # Tarih sC<tunu
  y = data_sim[[19]] # DeDer sC<tunu
)

# veri19 default
d_model19 <- prophet(df_prophet19,n.changepoints = 4)
d_changepoints19 <- d_model19$changepoints
d_changepoints19 <- as.numeric(d_changepoints19)
print(d_changepoints19)

# veri19 oracle
o_model19 <- prophet(df_prophet19,n.changepoints = 3)
o_changepoints19 <- o_model19$changepoints
o_changepoints19 <- as.numeric(o_changepoints19)
print(o_changepoints19)

# veri 20
nj[20]
tj[[20]]
plot(data_sim[[20]],type="l",col="blue")

df_prophet20 <- data.frame(
  ds = index(data_sim[[20]]), # Tarih sC<tunu
  y = data_sim[[20]] # DeDer sC<tunu
)

# veri20 default
d_model20 <- prophet(df_prophet20,n.changepoints = 3)
d_changepoints20 <- d_model20$changepoints
d_changepoints20 <- as.numeric(d_changepoints20)
print(d_changepoints20)

# veri20 oracle
o_model20 <- prophet(df_prophet20,n.changepoints = 2)
o_changepoints20 <- o_model20$changepoints
o_changepoints20 <- as.numeric(o_changepoints20)
print(o_changepoints20)

 # SEGMENTED REGRESSION #

# Organize simulated data into a data frame
set.seed(1)

# Simulate data for the first sample
n <- nj[1]
t_changepoints <- tj[[1]]

n_segments <- length(t_changepoints) + 1
n_obs <- c(t_changepoints[1] - 1, diff(t_changepoints), n - t_changepoints[length(t_changepoints)] + 1)

data_sim_first <- list()
for (i in 1:n_segments) {
  segment_data <- data.frame(
    Sample = rep(1, n_obs[i]),
    Time = seq_len(n_obs[i]),
    Value = rnorm(n_obs[i], mean = rnorm(1, 0, 10), sd = runif(1, 1, 10))
  )
  data_sim_first[[i]] <- segment_data
}

# Combine segments into a single data frame
simulated_data_first <- do.call(rbind, data_sim_first)

# Fit linear model
lm_model <- lm(Value ~ Time, data = simulated_data_first)

# Plot the simulated data and linear regression line
plot(simulated_data_first$Time, simulated_data_first$Value, type = "l", col = "blue", 
     xlab = "Time", ylab = "Value", main = "Simulated Data - First Sample")
abline(lm_model, col = "red")

# Apply segmented regression
library(segmented)
seg_model <- segmented(lm_model, seg.Z = ~Time)

# Extracting the changepoints
# Assuming 'df' is your dataframe
lm_model <- lm(value ~ time, data = df)
seg_model <- segmented(lm_model, seg.Z = ~ time)

# Extracting the changepoints
cp <- round(seg_model$psi[, 2])

# Finding the closest time points to the changepoints
cp_closest <- sapply(cp, function(bp) {
  d <- abs(df$time - bp)
  ind_closest <- which.min(d)
  closest_time <- df$time[ind_closest]
  c(index_pos = ind_closest, closest_time = closest_time)
}, df = df)

# Displaying the changepoints and their closest time points
cat("Changepoints:", cp, "\n")
cat("Changepoints and Closest Time Points:\n")
print(cp_closest)


data_sim[1]

str(data_sim)

#################
list_of_data_frames <- lapply(data_sim, function(sample_data) {
  data.frame(
    time = seq_along(sample_data),
    value = sample_data
  )
})

# Access the first data frame as an example
seg_data_frame <- list_of_data_frames[[14]]


attach(seg_data_frame)
lm2<-lm(value~time)
#def
seg_get2<-segmented(lm2,seg.Z = ~ time)
seg_get2
seg_get2$psi[,2]
cp <- round(seg_get2$psi[,2])
cp_closest2 <- sapply(cp, function(bp) {
  d<- abs( time - bp)
  indd <- which.min(d)
  e <- c(time[indd])
  c(index_pos=indd, closest_time=e)
})
cp_closest2

##########oracleee
seg_get2<-segmented(lm2,seg.Z = ~ time, npsi = 4)
seg_get2
seg_get2$psi[,2]
cp <- round(seg_get2$psi[,2])
cp_closest2 <- sapply(cp, function(bp) {
  d<- abs( time - bp)
  indd <- which.min(d)
  e <- c(time[indd])
  c(index_pos=indd, closest_time=e)
})
cp_closest2
detach(first_data_frame)

# Create an empty list to store segmented results for each data frame
all_segmented_results <- list()

# Loop through each data frame in the list
for (i in seq_along(list_of_data_frames)) {
  # Access the current data frame
  current_data_frame <- list_of_data_frames[[i]]
  
  # Fit linear model
  lm_model <- lm(value ~ time, data = current_data_frame)
  
  # Perform segmented regression
  seg_model <- segmented(lm_model, seg.Z = ~time)  # You can adjust the number of breakpoints (npsi) as needed
  
  # Store segmented results in the list
  all_segmented_results[[i]] <- seg_model
}

# View segmented results for the first data frame as an example
first_segmented_result <- all_segmented_results[[20]]

first_segmented_result

#oracle
# Create a vector representing the desired number of breakpoints for each data frame
npsi_values <- c(4, 3, 4, 5, 4, 4, 1, 3, 4, 4, 3, 5, 2, 2, 1, 1, 2, 3, 3, 2)

# Create an empty list to store segmented results for each data frame
all_segmented_results <- list()

# Loop through each data frame in the list
for (i in seq_along(list_of_data_frames)) {
  # Access the current data frame
  current_data_frame <- list_of_data_frames[[i]]
  
  # Fit linear model
  lm_model <- lm(value ~ time, data = current_data_frame)
  
  # Perform segmented regression with the corresponding npsi value
  seg_model <- segmented(lm_model, seg.Z = ~time, npsi = npsi_values[i])
  
  # Store segmented results in the list
  all_segmented_results[[i]] <- seg_model
}

# View segmented results for the first data frame as an example
first_segmented_result <- all_segmented_results[[1]]
print(first_segmented_result)
all_segmented_results[[2]]
all_segmented_results[[3]]
all_segmented_results[[4]]
all_segmented_results[[5]]
all_segmented_results[[6]]
all_segmented_results[[7]]
all_segmented_results[[8]]
all_segmented_results[[9]]
all_segmented_results[[10]]
all_segmented_results[[11]]
all_segmented_results[[12]]
all_segmented_results[[13]]
all_segmented_results[[14]]
all_segmented_results[[15]]
all_segmented_results[[16]]
all_segmented_results[[17]]
all_segmented_results[[18]]
all_segmented_results[[19]]
all_segmented_results[[20]]

                         #### FRIEDMAN-NEMENYI TEST ####

# SIM DATA COVER ORACLE #

sim <- matrix(c(0.5975, 0.5388, 0.4937, 0.5850, 0.7707, 0.6079, 0.9648, 0.6084, 0.6629,
                0.6269, 0.8764, 0.5639, 0.8771, 0.8932, 0.9992, 0.9950, 0.8408, 0.7744,
                0.6932, 0.5905, 0.9930, 0.8914, 0.9950, 0.9957, 0.9688, 0.8853, 0.9648,
                0.8831, 0.9467, 0.9092, 0.9689, 0.8220, 0.9976, 0.9567, 0.9992, 0.9950,
                0.9980, 0.9976, 0.9983, 0.9854, 0.9930, 0.8914, 0.9950, 0.9957, 0.9688,
                0.8853, 0.9648, 0.8831, 0.9467, 0.9092, 0.9689, 0.8220, 0.9976, 0.9567,
                0.9992, 0.9950, 0.9980, 0.9976, 0.9983, 0.9854, 0.6104, 0.6793, 0.6152,
                0.6830, 0.7494, 0.3967, 0.6002, 0.6700, 0.5665, 0.4866, 0.9998, 0.6338,
                0.6158, 0.5356, 0.5255, 0.5825, 0.7171, 0.4701, 0.4816, 0.5511, 0.7081,
                0.8194, 0.9246, 0.8840, 0.7706, 0.5599, 0.9670, 0.7364, 0.6067, 0.7201,
                0.9387, 0.5604, 0.6509, 0.8589, 0.8189, 0.7296, 0.7905, 0.6171, 0.6680,
                0.7184),nrow=20, ncol=5, 
              dimnames=list(1:20,c("AMOC","BinSeg","ParC'alD1 Reg","Prophet","PELT")))
print(sim)
result_sim<-friedman.test(sim)
result_sim
frdAllPairsNemenyiTest(sim, result_sim)
