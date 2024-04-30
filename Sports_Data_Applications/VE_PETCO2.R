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

file <- file.path("C:", "Users", "USER", "Desktop", "VE_PETCO2_v1.xlsx")
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
plot(P1_df$time, P1_df$value, type = "l", xlab = "watt", ylab = "VE_PETCO2", main = "P1 Time Series")

P1_df$time <- as.Date(P1_df$time)

tsdata1 <- ts(P1_df$value, start = min(P1_df$time), frequency = 1)
tsdata1
plot(tsdata1)

### CP KONUMLAR ###
# 24 #
# 33 #

## BINSEG ##

# oracle settings
o_binseg1 <- cpt.mean(tsdata1, penalty = "BIC",pen.value = 10, method = "BinSeg", Q = 2)
plot(o_binseg1)
abline(v = cpts(o_binseg1), col = "red", lty = 2,lwd=2)
cpts(o_binseg1)

## PELT ##

# oracle settings
o_pelt1_1<- cpt.mean(tsdata1, method = "PELT", penalty = "BIC", pen.value = 0.05, minseglen = 12)
plot(o_pelt1_1, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(o_pelt1_1), col = "blue", lty = 2,lwd=1)
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
plot(P2_df$time, P2_df$value, type = "l", xlab = "watt", ylab = "VE_PETCO2", main = "P2 Time Series")

P2_df$time <- as.Date(P2_df$time)

tsdata2 <- ts(P2_df$value, start = min(P2_df$time), frequency = 1)
tsdata2
plot(tsdata2)

### CP KONUMLAR ###
# 16 #
# 23 #

## BINSEG ##

# oracle settings
o_binseg2 <- cpt.mean(tsdata2, penalty = "BIC",pen.value = 2 * log(29) , method = "BinSeg", Q = 2)
plot(o_binseg2)
abline(v = cpts(o_binseg2), col = "red", lty = 2,lwd=2)
cpts(o_binseg2)

## PELT ##

# oracle settings
o_pelt2<- cpt.mean(tsdata2, method = "PELT", penalty = "BIC", pen.value = 2 * log(29), minseglen = 5)
plot(o_pelt2, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(o_pelt2), col = "blue", lty = 2,lwd=1)
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
plot(P3_df$time, P3_df$value, type = "l", xlab = "watt", ylab = "VE_PETCO2", main = "P3 Time Series")

P3_df$time <- as.Date(P3_df$time)

tsdata3 <- ts(P3_df$value, start = min(P3_df$time), frequency = 1)
tsdata3
plot(tsdata3)

### CP KONUMLAR ###
# 10 #
# 18 #

## BINSEG ##

# oracle settings
o_binseg3 <- cpt.mean(tsdata3, penalty = "BIC",pen.value = 2 * log(28), method = "BinSeg", Q = 2)
plot(o_binseg3)
abline(v = cpts(o_binseg3), col = "red", lty = 2,lwd=2)
cpts(o_binseg3)

## PELT ##

# oracle settings
o_pelt3<- cpt.mean(tsdata3, method = "PELT", penalty = "BIC", pen.value = 2 * log(28), minseglen = 5)
plot(o_pelt3, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(o_pelt3), col = "blue", lty = 2,lwd=1)
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
plot(P4_df$time, P4_df$value, type = "l", xlab = "watt", ylab = "VE_PETCO2", main = "P4 Time Series")

P4_df$time <- as.Date(P4_df$time)

tsdata4 <- ts(P4_df$value, start = min(P4_df$time), frequency = 1)
tsdata4
plot(tsdata4)


### CP KONUMLAR ###
# 23 #
# 33 #

## BINSEG ##

# oracle settings
o_binseg4 <- cpt.mean(tsdata4, penalty = "BIC",pen.value = 2 * log(42), method = "BinSeg", Q = 2)
plot(o_binseg4)
abline(v = cpts(o_binseg4), col = "red", lty = 2,lwd=2)
cpts(o_binseg4)

## PELT ##

# oracle settings1
o_pelt4<- cpt.mean(tsdata4, method = "PELT", penalty = "BIC", pen.value = 2 * log(42), minseglen = 11)
plot(o_pelt4, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(o_pelt4), col = "blue", lty = 2,lwd=1)
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
plot(P5_df$time, P5_df$value, type = "l", xlab = "watt", ylab = "VE_PETCO2", main = "P5 Time Series")

P5_df$time <- as.Date(P5_df$time)

tsdata5 <- ts(P5_df$value, start = min(P5_df$time), frequency = 1)
tsdata5
plot(tsdata5)


### CP KONUMLAR ###
# 28 #
# 45 #

## BINSEG ##

# oracle settings
o_binseg5 <- cpt.mean(tsdata5, penalty = "BIC",pen.value = 2 * log(54), method = "BinSeg", Q = 2)
plot(o_binseg5)
abline(v = cpts(o_binseg5), col = "red", lty = 2,lwd=2)
cpts(o_binseg5)

## PELT ##

# oracle settings1
o_pelt5<- cpt.mean(tsdata5, method = "PELT", penalty = "BIC", pen.value = 2 * log(54), minseglen = 14)
plot(o_pelt5, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(o_pelt5), col = "blue", lty = 2,lwd=1)
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
plot(P6_df$time, P6_df$value, type = "l", xlab = "watt", ylab = "VE_PETCO2", main = "P6 Time Series")

P6_df$time <- as.Date(P6_df$time)

tsdata6 <- ts(P6_df$value, start = min(P6_df$time), frequency = 1)
tsdata6
plot(tsdata6)


### CP KONUMLAR ###
# 18 #
# 24 #


## BINSEG ##

# oracle settings
o_binseg6 <- cpt.mean(tsdata6, penalty = "BIC",pen.value = 2 * log(29), method = "BinSeg", Q = 2)
plot(o_binseg6)
abline(v = cpts(o_binseg6), col = "red", lty = 2,lwd=2)
cpts(o_binseg6)

## PELT ##

# oracle settings
o_pelt6<- cpt.mean(tsdata6, method = "PELT", penalty = "BIC", pen.value = 2 * log(29), minseglen = 10, Q = 2)
plot(o_pelt6, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(o_pelt6), col = "blue", lty = 2,lwd=1)
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
plot(P7_df$time, P7_df$value, type = "l", xlab = "watt", ylab = "VE_PETCO2", main = "P7 Time Series")

P7_df$time <- as.Date(P7_df$time)

tsdata7 <- ts(P7_df$value, start = min(P7_df$time), frequency = 1)
tsdata7
plot(tsdata7)


### CP KONUMLAR ###
# 25 #
# 32 #


## BINSEG ##

# oracle settings
o_binseg7 <- cpt.mean(tsdata7, penalty = "BIC",pen.value = 2 * log(37), method = "BinSeg", Q = 2)
plot(o_binseg7)
abline(v = cpts(o_binseg7), col = "red", lty = 2,lwd=2)
cpts(o_binseg7)

## PELT ##

# oracle settings1
o_pelt7<- cpt.mean(tsdata7, method = "PELT", penalty = "BIC", pen.value = 2 * log(37), minseglen = 13)
plot(o_pelt7, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(o_pelt7), col = "blue", lty = 2,lwd=1)
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

P8_df
plot(P8_df$time, P8_df$value, type = "l", xlab = "watt", ylab = "VE_PETCO2", main = "P8 Time Series")

P8_df$time <- as.Date(P8_df$time)

tsdata8 <- ts(P8_df$value, start = min(P8_df$time), frequency = 1)
tsdata8
plot(tsdata8)


### CP KONUMLAR ###
# 16 #
# 25 #

## BINSEG ##

# oracle settings
o_binseg8 <- cpt.mean(tsdata8, penalty = "BIC",pen.value = 2 * log(34), method = "BinSeg", Q = 2)
plot(o_binseg8)
abline(v = cpts(o_binseg8), col = "red", lty = 2,lwd=2)
cpts(o_binseg8)

## PELT ##
 
# oracle settings1
o_pelt8<- cpt.mean(tsdata8, method = "PELT", penalty = "BIC", pen.value = 2 * log(34), minseglen = 10)
plot(o_pelt8, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(o_pelt8), col = "blue", lty = 2,lwd=1)
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
plot(P9_df$time, P9_df$value, type = "l", xlab = "watt", ylab = "VE_PETCO2", main = "P9 Time Series")

P9_df$time <- as.Date(P9_df$time)

tsdata9 <- ts(P9_df$value, start = min(P9_df$time), frequency = 1)
tsdata9
plot(tsdata9)

### CP KONUMLAR ###
# 19 #
# 34 #

## BINSEG ##

# oracle settings
o_binseg9 <- cpt.mean(tsdata9, penalty = "BIC",pen.value = 2 * log(44), method = "BinSeg", Q = 2)
plot(o_binseg9)
abline(v = cpts(o_binseg9), col = "red", lty = 2,lwd=2)
cpts(o_binseg9)

## PELT ##

# oracle settings
o_pelt9<- cpt.mean(tsdata9, method = "PELT", penalty = "BIC", pen.value = 2 * log(44), minseglen = 12)
plot(o_pelt9, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(o_pelt9), col = "blue", lty = 2,lwd=1)
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
plot(P10_df$time, P10_df$value, type = "l", xlab = "watt", ylab = "VE_PETCO2", main = "P10 Time Series")

P10_df$time <- as.Date(P10_df$time)

tsdata10 <- ts(P10_df$value, start = min(P10_df$time), frequency = 1)
tsdata10
plot(tsdata10)

### CP KONUMLAR ###
# 19 #
# 27 #

## BINSEG ##

# oracle settings
o_binseg10 <- cpt.mean(tsdata10, penalty = "BIC",pen.value = 2 * log(34), method = "BinSeg", Q = 2)
plot(o_binseg10)
abline(v = cpts(o_binseg10), col = "red", lty = 2,lwd=2)
cpts(o_binseg10)

## PELT ##

# oracle settings
o_pelt10<- cpt.mean(tsdata10, method = "PELT", penalty = "BIC", pen.value = 2 * log(34), minseglen = 9)
plot(o_pelt10, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(o_pelt10), col = "blue", lty = 2,lwd=1)
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
plot(P11_df$time, P11_df$value, type = "l", xlab = "watt", ylab = "VE_PETCO2", main = "P11 Time Series")

P11_df$time <- as.Date(P11_df$time)

tsdata11 <- ts(P11_df$value, start = min(P11_df$time), frequency = 1)
tsdata11
plot(tsdata11)

### CP KONUMLAR ###
# 24 #
# 30 #

## BINSEG ##

# oracle settings
o_binseg11 <- cpt.mean(tsdata11, penalty = "BIC",pen.value = 2 * log(37), method = "BinSeg", Q = 2)
plot(o_binseg11)
abline(v = cpts(o_binseg11), col = "red", lty = 2,lwd=2)
cpts(o_binseg11)

## PELT ##

# oracle settings
o_pelt11<- cpt.mean(tsdata11, method = "PELT", penalty = "BIC", pen.value = 2 * log(37), minseglen = 10)
plot(o_pelt11, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(o_pelt11), col = "blue", lty = 2,lwd=1)
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
plot(P12_df$time, P12_df$value, type = "l", xlab = "watt", ylab = "VE_PETCO2", main = "P12 Time Series")

P12_df$time <- as.Date(P12_df$time)

tsdata12 <- ts(P12_df$value, start = min(P12_df$time), frequency = 1)
tsdata12
plot(tsdata12)

### CP KONUMLAR ###
# 14 #
# 21 #

## BINSEG ##

# oracle settings
o_binseg12 <- cpt.mean(tsdata12, penalty = "BIC",pen.value = 2 * log(29), method = "BinSeg", Q = 2)
plot(o_binseg12)
abline(v = cpts(o_binseg12), col = "red", lty = 2,lwd=2)
cpts(o_binseg12)

## PELT ##

# oracle settings
o_pelt12<- cpt.mean(tsdata12, method = "PELT", penalty = "BIC", pen.value = 2 * log(29), minseglen = 8)
plot(o_pelt12, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(o_pelt12), col = "blue", lty = 2,lwd=1)
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
plot(P13_df$time, P13_df$value, type = "l", xlab = "watt", ylab = "VE_PETCO2", main = "P13 Time Series")

P13_df$time <- as.Date(P13_df$time)

tsdata13 <- ts(P13_df$value, start = min(P13_df$time), frequency = 1)
tsdata13
plot(tsdata13)

### CP KONUMLAR ###
# 54#
# 84 #

## BINSEG ##

# oracle settings
o_binseg13 <- cpt.mean(tsdata13, penalty = "BIC",pen.value = 2 * log(101), method = "BinSeg", Q = 2)
plot(o_binseg13)
abline(v = cpts(o_binseg13), col = "red", lty = 2,lwd=2)
cpts(o_binseg13)

## PELT ##

# oracle settings
o_pelt13<- cpt.mean(tsdata13, method = "PELT", penalty = "BIC", pen.value = 2 * log(101), minseglen = 12)
plot(o_pelt13, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(o_pelt13), col = "blue", lty = 2,lwd=1)
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
plot(P14_df$time, P14_df$value, type = "l", xlab = "watt", ylab = "VE_PETCO2", main = "P14 Time Series")

P14_df$time <- as.Date(P14_df$time)

tsdata14 <- ts(P14_df$value, start = min(P14_df$time), frequency = 1)
tsdata14
plot(tsdata14)

### CP KONUMLAR ###
# 48 #
# 72 #


## BINSEG ##

# oracle settings
o_binseg14 <- cpt.mean(tsdata14, penalty = "BIC",pen.value = 2 * log(92), method = "BinSeg", Q = 2)
plot(o_binseg14)
abline(v = cpts(o_binseg14), col = "red", lty = 2,lwd=2)
cpts(o_binseg14)

## PELT ##

# oracle settings
o_pelt14<- cpt.mean(tsdata14, method = "PELT", penalty = "BIC", pen.value = 2 * log(92), minseglen = 20)
plot(o_pelt14, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(o_pelt14), col = "blue", lty = 2,lwd=1)
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
plot(P15_df$time, P15_df$value, type = "l", xlab = "watt", ylab = "VE_PETCO2", main = "P15 Time Series")

P15_df$time <- as.Date(P15_df$time)

tsdata15 <- ts(P15_df$value, start = min(P15_df$time), frequency = 1)
tsdata15
plot(tsdata15)

### CP KONUMLAR ###
# 78 #
# 104 #


## BINSEG ##

# oracle settings
o_binseg15 <- cpt.mean(tsdata15, penalty = "BIC",pen.value = 2 * log(120), method = "BinSeg", Q = 2)
plot(o_binseg15)
abline(v = cpts(o_binseg15), col = "red", lty = 2,lwd=2)
cpts(o_binseg15)

## PELT ##

# oracle settings
o_pelt15<- cpt.mean(tsdata15, method = "PELT", penalty = "BIC", pen.value = 2 * log(120), minseglen = 2)
plot(o_pelt15, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(o_pelt15), col = "blue", lty = 2,lwd=1)
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

