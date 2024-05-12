install.packages("readxl")
install.packages("dplyr")
library(readxl)
library(dplyr)
install.packages("changepoint")
library(changepoint)
library(strucchange)
library(ggplot2)
install.packages("segmented")
library(segmented)
library(zoo)
library(tibble)

file <- file.path("C:", "Users", "PC", "Downloads", "VE_PETCO2_v1.xlsx")
sheet_names <- excel_sheets(file)
data_list <- lapply(sheet_names, function(sheet) {
  data <- read_excel(file, sheet = sheet)
  data <- data[, c("watt", "VE_PETCO2")]
  return(data)
})

data_list


## 1.sporcu 

P1_df <- data.frame(
  time = data_list[[1]]$watt,  
  value = as.numeric(data_list[[1]]$VE_PETCO2)
)

P1_df
P1_ts <- zoo::zoo(P1_df$value, order.by = P1_df$time)

### CP KONUMLAR ###
# 24 #
# 33 #

## BINSEG ##

# oracle settings
o_binseg1 <- cpt.mean(P1_ts, penalty = "BIC",pen.value = 10, method = "BinSeg", Q = 2)
plot(o_binseg1,xlab = "watt", ylab = "VE_PETCO2")
abline(v = cpts(o_binseg1), col = "red", lty = 2,lwd=2)
cpts(o_binseg1)

## PELT ##

# oracle settings
o_pelt1_1<- cpt.mean(P1_ts, method = "PELT", penalty = "BIC", pen.value = 0.05, minseglen = 12)
plot(o_pelt1_1, xlab = "watt", ylab = "VE_PETCO2")
abline(v = cpts(o_pelt1_1), col = "red", lty = 2,lwd=2)
cpts(o_pelt1_1)  

## SEGMENTED REGRESSION ##

# oracle settings
attach(P1_df)
with(P1_df, {
o_lm1<-lm(value~time)

o_seg_get1<-segmented(o_lm1,seg.Z = ~ time, npsi=2)
o_seg_get1
o_seg_get1$psi[,2]
o_cp1 <- round(o_seg_get1$psi[,2])
if (class(time) == "Date") {
  time <- as.numeric(time)
}
o_cp_closest1 <- sapply(o_cp1, function(bp) {
  d<- abs( time - bp)
  indd <- which.min(d)
  e <- c(time[indd])
  c(index_pos=indd, closest_time=e)
})
o_cp_closest1
})
detach(P1_df)


## 2.sporcu 

P2_df <- data.frame(
  time = data_list[[2]]$watt,  
  value = as.numeric(data_list[[2]]$VE_PETCO2)
)

P2_df
P2_ts <- zoo::zoo(P2_df$value, order.by = P2_df$time)

### CP KONUMLAR ###
# 16 #
# 23 #

## BINSEG ##

# oracle settings
o_binseg2 <- cpt.mean(P2_ts, penalty = "BIC",pen.value = 2 * log(29) , method = "BinSeg", Q = 2)
plot(o_binseg2,xlab = "watt", ylab = "VE_PETCO2")
abline(v = cpts(o_binseg2), col = "red", lty = 2,lwd=2)
cpts(o_binseg2)

## PELT ##

# oracle settings
o_pelt2<- cpt.mean(P2_ts, method = "PELT", penalty = "BIC", pen.value = 2 * log(29), minseglen = 5)
plot(o_pelt2, xlab = "watt", ylab = "VE_PETCO2")
abline(v = cpts(o_pelt2), col = "red", lty = 2,lwd=2)
cpts(o_pelt2)  


## SEGMENTED REGRESSION ##

# oracle settings
attach(P2_df)
with(P2_df, {
o_lm2<-lm(value~time)

o_seg_get2<-segmented(o_lm2,seg.Z = ~ time, npsi=2)
o_seg_get2
o_seg_get2$psi[,2]
o_cp2 <- round(o_seg_get2$psi[,2])
if (class(time) == "Date") {
  time <- as.numeric(time)
}
o_cp_closest2 <- sapply(o_cp2, function(bp) {
  d<- abs( time - bp)
  indd <- which.min(d)
  e <- c(time[indd])
  c(index_pos=indd, closest_time=e)
})
o_cp_closest2
})
detach(P2_df)


## 3.sporcu 

P3_df <- data.frame(
  time = data_list[[3]]$watt,  
  value = as.numeric(data_list[[3]]$VE_PETCO2)
)

P3_df
P3_ts <- zoo::zoo(P3_df$value, order.by = P3_df$time)

### CP KONUMLAR ###
# 10 #
# 18 #

## BINSEG ##

# oracle settings
o_binseg3 <- cpt.mean(P3_ts, penalty = "BIC",pen.value = 2 * log(28), method = "BinSeg", Q = 2)
plot(o_binseg3,xlab = "watt", ylab = "VE_PETCO2")
abline(v = cpts(o_binseg3), col = "red", lty = 2,lwd=2)
cpts(o_binseg3)

## PELT ##

# oracle settings
o_pelt3<- cpt.mean(P3_ts, method = "PELT", penalty = "BIC", pen.value = 2 * log(28), minseglen = 5)
plot(o_pelt3, xlab = "watt", ylab = "VE_PETCO2")
abline(v = cpts(o_pelt3), col = "red", lty = 2,lwd=2)
cpts(o_pelt3)  

## SEGMENTED REGRESSION ##

# oracle settings
attach(P3_df)
with(P3_df, {
o_lm3<-lm(value~time)

o_seg_get3<-segmented(o_lm3,seg.Z = ~ time, npsi=2)
o_seg_get3
o_seg_get3$psi[,2]
o_cp3 <- round(o_seg_get3$psi[,2])
if (class(time) == "Date") {
  time <- as.numeric(time)
}
o_cp_closest3 <- sapply(o_cp3, function(bp) {
  d<- abs( time - bp)
  indd <- which.min(d)
  e <- c(time[indd])
  c(index_pos=indd, closest_time=e)
})
o_cp_closest3
})
detach(P3_df)


## 4.sporcu 

P4_df <- data.frame(
  time = data_list[[4]]$watt,  
  value = as.numeric(data_list[[4]]$VE_PETCO2)
)
P4_df
P4_ts <- zoo::zoo(P4_df$value, order.by = P4_df$time)

### CP KONUMLAR ###
# 23 #
# 33 #

## BINSEG ##

# oracle settings
o_binseg4 <- cpt.mean(P4_ts, penalty = "BIC",pen.value = 2 * log(42), method = "BinSeg", Q = 2)
plot(o_binseg4,xlab = "watt", ylab = "VE_PETCO2")
abline(v = cpts(o_binseg4), col = "red", lty = 2,lwd=2)
cpts(o_binseg4)

## PELT ##

# oracle settings1
o_pelt4<- cpt.mean(P4_ts, method = "PELT", penalty = "BIC", pen.value = 2 * log(42), minseglen = 11)
plot(o_pelt4, xlab = "watt", ylab = "VE_PETCO2")
abline(v = cpts(o_pelt4), col = "red", lty = 2,lwd=2)
cpts(o_pelt4)  

## SEGMENTED REGRESSION ##

# oracle settings
attach(P4_df)
with(P4_df, {
o_lm4<-lm(value~time)

o_seg_get4<-segmented(o_lm4,seg.Z = ~ time,npsi = 2)
o_seg_get4
o_seg_get4$psi[,2]
o_cp4 <- round(o_seg_get4$psi[,2])
if (class(time) == "Date") {
  time <- as.numeric(time)
}
o_cp_closest4 <- sapply(o_cp4, function(bp) {
  d<- abs( time- bp)
  indd <- which.min(d)
  e <- c(time[indd])
  c(index_pos=indd, closest_time=e)
})
o_cp_closest4
})
detach(P4_df)


## 5.sporcu 

P5_df <- data.frame(
  time = data_list[[5]]$watt,  
  value = as.numeric(data_list[[5]]$VE_PETCO2)
)
P5_df
P5_ts <- zoo::zoo(P5_df$value, order.by = P5_df$time)


### CP KONUMLAR ###
# 28 #
# 45 #

## BINSEG ##

# oracle settings
o_binseg5 <- cpt.mean(P5_ts, penalty = "BIC",pen.value = 2 * log(54), method = "BinSeg", Q = 2)
plot(o_binseg5,xlab = "watt", ylab = "VE_PETCO2")
abline(v = cpts(o_binseg5), col = "red", lty = 2,lwd=2)
cpts(o_binseg5)

## PELT ##

# oracle settings1
o_pelt5<- cpt.mean(P5_ts, method = "PELT", penalty = "BIC", pen.value = 2 * log(54), minseglen = 14)
plot(o_pelt5, xlab = "watt", ylab = "VE_PETCO2")
abline(v = cpts(o_pelt5), col = "red", lty = 2,lwd=2)
cpts(o_pelt5)  

## SEGMENTED REGRESSION ##

# oracle settings
attach(P5_df)
with(P5_df, {
o_lm5<-lm(value~time)

o_seg_get5<-segmented(o_lm5,seg.Z = ~ time,npsi = 2)
o_seg_get5
o_seg_get5$psi[,2]
o_cp5 <- round(o_seg_get5$psi[,2])
if (class(time) == "Date") {
  time <- as.numeric(time)
}
o_cp_closest5 <- sapply(o_cp5, function(bp) {
  d<- abs( time - bp)
  indd <- which.min(d)
  e <- c(time[indd])
  c(index_pos=indd, closest_time=e)
})
o_cp_closest5
})
detach(P5_df)


## 6.sporcu 

P6_df <- data.frame(
  time = data_list[[6]]$watt,  
  value = as.numeric(data_list[[6]]$VE_PETCO2)
)

P6_df
P6_ts <- zoo::zoo(P6_df$value, order.by = P6_df$time)

### CP KONUMLAR ###
# 18 #
# 24 #


## BINSEG ##

# oracle settings
o_binseg6 <- cpt.mean(P6_ts, penalty = "BIC",pen.value = 2 * log(29), method = "BinSeg", Q = 2)
plot(o_binseg6,xlab = "watt", ylab = "VE_PETCO2")
abline(v = cpts(o_binseg6), col = "red", lty = 2,lwd=2)
cpts(o_binseg6)

## PELT ##

# oracle settings
o_pelt6<- cpt.mean(P6_ts, method = "PELT", penalty = "BIC", pen.value = 2 * log(29), minseglen = 10, Q = 2)
plot(o_pelt6, xlab = "watt", ylab = "VE_PETCO2")
abline(v = cpts(o_pelt6), col = "red", lty = 2,lwd=2)
cpts(o_pelt6)  

## SEGMENTED REGRESSION ##

# oracle settings
attach(P6_df)
with(P6_df, {
o_lm6<-lm(value~time)

o_seg_get6<-segmented(o_lm6,seg.Z = ~ time,npsi = 2)
o_seg_get6
o_seg_get6$psi[,2]
o_cp6 <- round(o_seg_get6$psi[,2])
if (class(time) == "Date") {
  time <- as.numeric(time)
}
o_cp_closest6 <- sapply(o_cp6, function(bp) {
  d<- abs( time- bp)
  indd <- which.min(d)
  e <- c(time[indd])
  c(index_pos=indd, closest_time=e)
})
o_cp_closest6
})
detach(P6_df)


## 7.sporcu 

P7_df <- data.frame(
  time = data_list[[7]]$watt,  
  value = as.numeric(data_list[[7]]$VE_PETCO2)
)
P7_df
P7_ts <- zoo::zoo(P7_df$value, order.by = P7_df$time)


### CP KONUMLAR ###
# 25 #
# 32 #


## BINSEG ##

# oracle settings
o_binseg7 <- cpt.mean(P7_ts, penalty = "BIC",pen.value = 2 * log(37), method = "BinSeg", Q = 2)
plot(o_binseg7,xlab = "watt", ylab = "VE_PETCO2")
abline(v = cpts(o_binseg7), col = "red", lty = 2,lwd=2)
cpts(o_binseg7)

## PELT ##

# oracle settings1
o_pelt7<- cpt.mean(P7_ts, method = "PELT", penalty = "BIC", pen.value = 2 * log(37), minseglen = 13)
plot(o_pelt7, xlab = "watt", ylab = "VE_PETCO2")
abline(v = cpts(o_pelt7), col = "red", lty = 2,lwd=2)
cpts(o_pelt7)  

## SEGMENTED REGRESSION ##

# oracle settings
attach(P7_df)
with(P7_df, {
o_lm7<-lm(value~time)

o_seg_get7<-segmented(o_lm7,seg.Z = ~ time,npsi = 2)
o_seg_get7
o_seg_get7$psi[,2]
o_cp7 <- round(o_seg_get7$psi[,2])
if (class(time) == "Date") {
  time <- as.numeric(time)
}
o_cp_closest7 <- sapply(o_cp7, function(bp) {
  d<- abs( time - bp)
  indd <- which.min(d)
  e <- c(time[indd])
  c(index_pos=indd, closest_time=e)
})
o_cp_closest7
})
detach(P7_df)


## 8.sporcu 

P8_df <- data.frame(
  time = data_list[[8]]$watt,  
  value = as.numeric(data_list[[8]]$VE_PETCO2)
)
P8_ts <- zoo::zoo(P8_df$value, order.by = P8_df$time)

### CP KONUMLAR ###
# 16 #
# 25 #

## BINSEG ##

# oracle settings
o_binseg8 <- cpt.mean(P8_ts, penalty = "BIC", pen.value = 2 * log(34), method = "BinSeg", Q = 2)
plot(o_binseg8, main = "BinSeg", xlab = "watt", ylab = "VE_PETCO2")
abline(v = cpts(o_binseg8), col = "red", lty = 2,lwd=2)
cpts(o_binseg8)

## PELT ##
 
# oracle settings1
o_pelt8<- cpt.mean(P8_ts, method = "PELT", penalty = "BIC", pen.value = 2 * log(34), minseglen = 10)
plot(o_pelt8, main = "PELT", xlab = "watt", ylab = "VE_PETCO2")
abline(v = cpts(o_pelt8), col = "red", lty = 2,lwd=2)
cpts(o_pelt8)  

## SEGMENTED REGRESSION ##

# oracle settings
attach(P8_df)
with(P8_df, {
o_lm8<-lm(value~time)

o_seg_get8<-segmented(o_lm8,seg.Z = ~ time,npsi = 2)
o_seg_get8
o_seg_get8$psi[,2]
o_cp8 <- round(o_seg_get8$psi[,2])
if (class(time) == "Date") {
  time <- as.numeric(time)
}
o_cp_closest8 <- sapply(o_cp8, function(bp) {
  d<- abs( time- bp)
  indd <- which.min(d)
  e <- c(time[indd])
  c(index_pos=indd, closest_time=e)
})
o_cp_closest8
})
detach(P8_df)

## 9.SPORCU
P9_df <- data.frame(
  time = data_list[[9]]$watt,  
  value = as.numeric(data_list[[9]]$VE_PETCO2)
)
P9_df

P9_ts <- zoo::zoo(P9_df$value, order.by = P9_df$time)

### CP KONUMLAR ###
# 19 #
# 34 #

## BINSEG ##

# oracle settings
o_binseg9 <- cpt.mean(P9_ts, penalty = "BIC",pen.value = 2 * log(44), method = "BinSeg", Q = 2)
plot(o_binseg9, main = "BinSeg",xlab = "watt", ylab = "VE_PETCO2")
abline(v = cpts(o_binseg9), col = "red", lty = 2,lwd=2)
cpts(o_binseg9)

## PELT ##

# oracle settings
o_pelt9<- cpt.mean(P9_ts, method = "PELT", penalty = "BIC", pen.value = 2 * log(44), minseglen = 12)
plot(o_pelt9,main = "PELT", xlab = "watt", ylab = "VE_PETCO2")
abline(v = cpts(o_pelt9), col = "red", lty = 2,lwd=2)
cpts(o_pelt9)  

## SEGMENTED REGRESSION ##

# oracle settings
attach(P9_df)
with(P9_df, {
o_lm9<-lm(value~time)

o_seg_get9<-segmented(o_lm9,seg.Z = ~ time, npsi=2)
o_seg_get9
o_seg_get9$psi[,2]
o_cp9 <- round(o_seg_get9$psi[,2])
if (class(time) == "Date") {
  time <- as.numeric(time)
}
o_cp_closest9 <- sapply(o_cp9, function(bp) {
  d<- abs( time - bp)
  indd <- which.min(d)
  e <- c(time[indd])
  c(index_pos=indd, closest_time=e)
})
o_cp_closest9
})
detach(P9_df)

## 10.SPORCU
P10_df <- data.frame(
  time = data_list[[10]]$watt,  
  value = as.numeric(data_list[[10]]$VE_PETCO2)
)
P10_df
P10_ts <- zoo::zoo(P10_df$value, order.by = P10_df$time)

### CP KONUMLAR ###
# 19 #
# 27 #

## BINSEG ##

# oracle settings
o_binseg10 <- cpt.mean(P10_ts, penalty = "BIC",pen.value = 2 * log(34), method = "BinSeg", Q = 2)
plot(o_binseg10,xlab = "watt", ylab = "VE_PETCO2")
abline(v = cpts(o_binseg10), col = "red", lty = 2,lwd=2)
cpts(o_binseg10)

## PELT ##

# oracle settings
o_pelt10<- cpt.mean(P10_ts, method = "PELT", penalty = "BIC", pen.value = 2 * log(34), minseglen = 9)
plot(o_pelt10, xlab = "watt", ylab = "VE_PETCO2")
abline(v = cpts(o_pelt10), col = "red", lty = 2,lwd=2)
cpts(o_pelt10)  

## SEGMENTED REGRESSION ##

# oracle settings
attach(P10_df)
with(P10_df, {
o_lm10<-lm(value~time)

o_seg_get10<-segmented(o_lm10,seg.Z = ~ time, npsi=2)
o_seg_get10
o_seg_get10$psi[,2]
o_cp10 <- round(o_seg_get10$psi[,2])
if (class(time) == "Date") {
  time <- as.numeric(time)
}
o_cp_closest10 <- sapply(o_cp10, function(bp) {
  d<- abs( time - bp)
  indd <- which.min(d)
  e <- c(time[indd])
  c(index_pos=indd, closest_time=e)
})
o_cp_closest10
})
detach(P10_df)

## 11.SPORCU
P11_df <- data.frame(
  time = data_list[[11]]$watt,  
  value = as.numeric(data_list[[11]]$VE_PETCO2)
)
P11_df
P11_ts <- zoo::zoo(P11_df$value, order.by = P11_df$time)

### CP KONUMLAR ###
# 24 #
# 30 #

## BINSEG ##

# oracle settings
o_binseg11 <- cpt.mean(P11_ts, penalty = "BIC",pen.value = 2 * log(37), method = "BinSeg", Q = 2)
plot(o_binseg11,xlab = "watt", ylab = "VE_PETCO2")
abline(v = cpts(o_binseg11), col = "red", lty = 2,lwd=2)
cpts(o_binseg11)

## PELT ##

# oracle settings
o_pelt11<- cpt.mean(P11_ts, method = "PELT", penalty = "BIC", pen.value = 2 * log(37), minseglen = 10)
plot(o_pelt11, xlab = "watt", ylab = "VE_PETCO2")
abline(v = cpts(o_pelt11), col = "red", lty = 2,lwd=2)
cpts(o_pelt11)  

## SEGMENTED REGRESSION ##

# oracle settings
attach(P11_df)
with(P11_df, {
  o_lm11<-lm(value~time)
  
  o_seg_get11<-segmented(o_lm11,seg.Z = ~ time, npsi=2)
  o_seg_get11
  o_seg_get11$psi[,2]
  o_cp11 <- round(o_seg_get11$psi[,2])
  if (class(time) == "Date") {
    time <- as.numeric(time)
  }
  o_cp_closest11 <- sapply(o_cp11, function(bp) {
    d<- abs( time - bp)
    indd <- which.min(d)
    e <- c(time[indd])
    c(index_pos=indd, closest_time=e)
  })
  o_cp_closest11
})
detach(P11_df)

## 12.SPORCU
P12_df <- data.frame(
  time = data_list[[12]]$watt,  
  value = as.numeric(data_list[[12]]$VE_PETCO2)
)
P12_df
P12_ts <- zoo::zoo(P12_df$value, order.by = P12_df$time)

### CP KONUMLAR ###
# 14 #
# 21 #

## BINSEG ##

# oracle settings
o_binseg12 <- cpt.mean(P12_ts, penalty = "BIC",pen.value = 2 * log(29), method = "BinSeg", Q = 2)
plot(o_binseg12,xlab = "watt", ylab = "VE_PETCO2")
abline(v = cpts(o_binseg12), col = "red", lty = 2,lwd=2)
cpts(o_binseg12)

## PELT ##

# oracle settings
o_pelt12<- cpt.mean(P12_ts, method = "PELT", penalty = "BIC", pen.value = 2 * log(29), minseglen = 8)
plot(o_pelt12, xlab = "watt", ylab = "VE_PETCO2")
abline(v = cpts(o_pelt12), col = "red", lty = 2,lwd=2)
cpts(o_pelt12)  

## SEGMENTED REGRESSION ##

# oracle settings
attach(P12_df)
with(P12_df, {
o_lm12<-lm(value~time)

o_seg_get12<-segmented(o_lm12,seg.Z = ~ time, npsi=2)
o_seg_get12
o_seg_get12$psi[,2]
o_cp12 <- round(o_seg_get12$psi[,2])
if (class(time) == "Date") {
  time <- as.numeric(time)
}
o_cp_closest12 <- sapply(o_cp12, function(bp) {
  d<- abs( time - bp)
  indd <- which.min(d)
  e <- c(time[indd])
  c(index_pos=indd, closest_time=e)
})
o_cp_closest12
})
detach(P12_df)

## 13.SPORCU
P13_df <- data.frame(
  time = data_list[[13]]$watt,  
  value = as.numeric(data_list[[13]]$VE_PETCO2)
)

P13_df
P13_ts <- zoo::zoo(P13_df$value, order.by = P13_df$time)

### CP KONUMLAR ###

# 54 #
# 84 #

## BINSEG ##

# oracle settings
o_binseg13 <- cpt.mean(P13_ts, penalty = "BIC",pen.value = 2 * log(101), method = "BinSeg", Q = 2)
plot(o_binseg13,xlab = "watt", ylab = "VE_PETCO2")
abline(v = cpts(o_binseg13), col = "red", lty = 2,lwd=2)
cpts(o_binseg13)

## PELT ##

# oracle settings
o_pelt13<- cpt.mean(P13_ts, method = "PELT", penalty = "BIC", pen.value = 2 * log(101), minseglen = 12)
plot(o_pelt13, xlab = "watt", ylab = "VE_PETCO2")
abline(v = cpts(o_pelt13), col = "red", lty = 2,lwd=2)
cpts(o_pelt13)  

## SEGMENTED REGRESSION ##

# oracle settings
attach(P13_df)
with(P13_df, {
o_lm13<-lm(value~time)

o_seg_get13<-segmented(o_lm13,seg.Z = ~ time, npsi=2)
o_seg_get13
o_seg_get13$psi[,2]
o_cp13 <- round(o_seg_get13$psi[,2])
if (class(time) == "Date") {
  time <- as.numeric(time)
}
o_cp_closest13 <- sapply(o_cp13, function(bp) {
  d<- abs( time - bp)
  indd <- which.min(d)
  e <- c(time[indd])
  c(index_pos=indd, closest_time=e)
})
o_cp_closest13
})
detach(P13_df)


## 14.SPORCU
P14_df <- data.frame(
  time = data_list[[14]]$watt,  
  value = as.numeric(data_list[[14]]$VE_PETCO2)
)
P14_df
P14_ts <- zoo::zoo(P14_df$value, order.by = P14_df$time)

### CP KONUMLAR ###
# 48 #
# 72 #


## BINSEG ##

# oracle settings
o_binseg14 <- cpt.mean(P14_ts, penalty = "BIC",pen.value = 2 * log(92), method = "BinSeg", Q = 2)
plot(o_binseg14,xlab = "watt", ylab = "VE_PETCO2")
abline(v = cpts(o_binseg14), col = "red", lty = 2,lwd=2)
cpts(o_binseg14)

## PELT ##

# oracle settings
o_pelt14<- cpt.mean(P14_ts, method = "PELT", penalty = "BIC", pen.value = 2 * log(92), minseglen = 20)
plot(o_pelt14, xlab = "watt", ylab = "VE_PETCO2")
abline(v = cpts(o_pelt14), col = "red", lty = 2,lwd=2)
cpts(o_pelt14)  

## SEGMENTED REGRESSION ##

# oracle settings
attach(P14_df)
with(P14_df, {
o_lm14<-lm(value~time)

o_seg_get14<-segmented(o_lm14,seg.Z = ~ time, npsi=2)
o_seg_get14
o_seg_get14$psi[,2]
o_cp14 <- round(o_seg_get14$psi[,2])
if (class(time) == "Date") {
  time <- as.numeric(time)
}
o_cp_closest14 <- sapply(o_cp14, function(bp) {
  d<- abs( time - bp)
  indd <- which.min(d)
  e <- c(time[indd])
  c(index_pos=indd, closest_time=e)
})
o_cp_closest14
})
detach(P14_df)

## 15.SPORCU
P15_df <- data.frame(
  time = data_list[[15]]$watt,  
  value = as.numeric(data_list[[15]]$VE_PETCO2)
)
P15_df
P15_ts <- zoo::zoo(P15_df$value, order.by = P15_df$time)

### CP KONUMLAR ###
# 78 #
# 104 #


## BINSEG ##

# oracle settings
o_binseg15 <- cpt.mean(P15_ts, penalty = "BIC",pen.value = 2 * log(120), method = "BinSeg", Q = 2)
plot(o_binseg15,xlab = "watt", ylab = "VE_PETCO2")
abline(v = cpts(o_binseg15), col = "red", lty = 2,lwd=2)
cpts(o_binseg15)

## PELT ##

# oracle settings
o_pelt15<- cpt.mean(P15_ts, method = "PELT", penalty = "BIC", pen.value = 2 * log(120), minseglen = 2)
plot(o_pelt15, xlab = "watt", ylab = "VE_PETCO2")
abline(v = cpts(o_pelt15), col = "red", lty = 2,lwd=2)
cpts(o_pelt15)  

## SEGMENTED REGRESSION ##

# oracle settings
attach(P15_df)
with(P15_df, {
o_lm15<-lm(value~time)

o_seg_get15<-segmented(o_lm15,seg.Z = ~ time, npsi=2)
o_seg_get15
o_seg_get15$psi[,2]
o_cp15 <- round(o_seg_get15$psi[,2])
if (class(time) == "Date") {
  time <- as.numeric(time)
}
o_cp_closest15 <- sapply(o_cp15, function(bp) {
  d<- abs( time - bp)
  indd <- which.min(d)
  e <- c(time[indd])
  c(index_pos=indd, closest_time=e)
})
o_cp_closest15
})
detach(P15_df)

if(!require('PMCMRplus')) {
  install.packages('PMCMRplus')
  library('PMCMRplus')
}

ve_petco2 <- matrix(c(0.7023, NA, NA, 0.7485, 0.9326, NA, NA, 1.0000, 0.8181, 0.8039,
                      1.0000, 1.0000, 0.6520, 0.5434, 0.7141,
                      0.6688, 0.8232, 0.5097, 0.9547, 0.8972, 0.7793, 0.9493, 0.8930,
                      0.9545, 0.6843, 0.6111, 0.7270, 0.8486, 0.7423, 0.4571,
                      0.7340, NA, NA, 0.7308, 0.7314, NA, NA, 0.8425, 0.8723, 0.7189,
                      0.5965, 0.9353, 0.9805, 1.0000, 0.8194),
                    nrow = 15, ncol = 3,
                    dimnames = list(1:15, c("BinSeg", "Parcali_Regresyon", "PELT")))
ve_petco2
result<-friedman.test(ve_petco2)
result
