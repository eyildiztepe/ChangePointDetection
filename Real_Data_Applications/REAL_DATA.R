### Yuklenecejk paketler
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

                               #### BITCOIN DATA ####

Dosya1 <- "bitcoin.json"
parsed_data<- fromJSON(Dosya1)

cat("Name:", parsed_data$name, "\n")
cat("Longname:", parsed_data$longname, "\n")
cat("Number of Observations:", parsed_data$n_obs, "\n")
cat("Number of Dimensions:", parsed_data$n_dim, "\n")

cat("Time Format:", parsed_data$time$format, "\n")
cat("First 5 Time Indices:", head(parsed_data$time$index), "\n")
cat("First 5 Time Values:", head(parsed_data$time$raw), "\n")
cat("Series Label:", parsed_data$series$label, "\n")
cat("First 5 Series Values:", head(parsed_data$series$raw[[1]], 5), "\n")
parsed_data

# ggplot2 kC<tC<phanesini yC<kle
library(ggplot2)

# Time series data


time_raw <- as.numeric(as.Date(parsed_data$time$raw))
series_raw <- as.numeric(parsed_data$series$raw[[1]])

# Create a data frame
df <- data.frame(
  index = 1:length(time_raw),
  time = time_raw,
  value = series_raw
)


plot(df$index, df$value, type = "l", xlab = "Index", ylab = "Value", main = "Time Series")

#ORTALAMADA DEGISIM NOKTASI ARAYAN ALGORITMALAR

#Zaman serisi oluEturma
tsdata <- ts(df$value, start = min(df$index), end = max(df$index), frequency = 1)

# Zaman serisini gC6ster
print(tsdata)

#####bD1tcoD1n annotator 
##bD1tcoD1n
#
#6 <- 502,580,702,747
##8<-583
#12<-597
#13<-522,579,591,629,703,747,760
#14<-93,522,540,701,747,760,772

#####AMOC######
d.m_amoc <- cpt.mean(tsdata, method = "AMOC")
plot(d.m_amoc)
abline(v = cpts(d.m_amoc), col = "red", lty = 2,lwd=2)
cpts(d.m_amoc)

# oracle settings
o.m_amoc <- cpt.mean(tsdata, penalty = "BIC", method = "AMOC")
plot(o.m_amoc)
abline(v = cpts(o.m_amoc), col = "red", lty = 2,lwd=2)
cpts(o.m_amoc)

###BINSEG######
#default
x<-cpt.mean(tsdata,method = "BinSeg")
cpts(x)

#oracle
m_binseg <- cpt.mean(tsdata, penalty = "BIC", method = "BinSeg", Q = 5)
plot(m_binseg)
abline(v = cpts(m_binseg), col = "red", lty = 2,lwd=2)
cpts(m_binseg)


###Q deDiEim noktasD1 sayD1sD1nD1 ayarlar

###BinSeg_1
m_binseg1 <- cpt.mean(tsdata, penalty = "BIC", method = "BinSeg", Q = 10)
plot(m_binseg1)
abline(v = cpts(m_binseg1), col = "red", lty = 2,lwd=2)
cpts(m_binseg1)

###BinSeg_2 en iyisi bu

m_binseg2 <- cpt.mean(tsdata, penalty = "BIC", method = "BinSeg", Q = 25)
plot(m_binseg2)
abline(v = cpts(m_binseg2), col = "red", lty = 2,lwd=2)
cpts(m_binseg2)

###BinSeg_3 

m_binseg3 <- cpt.mean(tsdata, penalty = "BIC", method = "BinSeg", Q = 30)
plot(m_binseg3)
abline(v = cpts(m_binseg3), col = "red", lty = 2,lwd=2)
cpts(m_binseg3)

###BinSeg_4 
m_binseg4 <- cpt.mean(tsdata, penalty = "AIC", method = "BinSeg", Q = 30)
plot(m_binseg4)
abline(v = cpts(m_binseg4), col = "red", lty = 2,lwd=2)
cpts(m_binseg4)

###BinSeg_5 
m_binseg5 <- cpt.mean(tsdata, penalty = "Manual", method = "BinSeg", Q = 30)
plot(m_binseg5)
abline(v = cpts(m_binseg5), col = "red", lty = 2,lwd=2)
cpts(m_binseg5)

#PELT########

###default
m_pelt <- cpt.mean(tsdata, method = "PELT")
plot(m_pelt, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(m_pelt), col = "blue", lty = 2,lwd=2)
cpts(m_pelt) 

###oracle
m_pelt1 <- cpt.mean(tsdata, penalty = "MBIC", method = "PELT", minseglen = 20)
plot(m_pelt1, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(m_pelt1), col = "blue", lty = 2,lwd=2)
cpts(m_pelt1) 

###PELT_2 en iyi bu
m_pelt2 <- cpt.mean(tsdata, penalty = "MBIC", method = "PELT", minseglen = 30, pen.value = 5)
plot(m_pelt2, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(m_pelt2), col = "blue", lty = 2,lwd=2)
cpts(m_pelt2) 

###PELT_3
m_pelt3 <- cpt.mean(tsdata, penalty = "MBIC", method = "PELT", minseglen = 25, pen.value = 1)
plot(m_pelt3, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(m_pelt3), col = "blue", lty = 2,lwd=2)
cpts(m_pelt3)


#VARYANSTA DEGISIM NOKTALARI ARAYAN ALGORITMA
v_pelt <- cpt.var(tsdata, method = "PELT")
plot(v_pelt, type = "l", cpt.col = "blue", xlab = "Index")
cpts(v_pelt)  

#HEM ORTALAMADA HEM DE VARYANSTA DEGISIM NOKTASI ARAMA
mv_pelt <- cpt.meanvar(tsdata, method = "PELT")
mv_pelt 
plot(mv_pelt)
cpts(mv_pelt)   

###SEGMENTED REGRESYON#######
install.packages("segmented")
library(segmented)

#default
attach(df)
lm1<-lm(value~time)
seg_get<-segmented(lm1,seg.Z = ~ time)
seg_get
seg_get$psi[,2]
cp <- round(seg_get$psi[,2])
cp_closest <- sapply(cp, function(bp) {
  d<- abs( time - bp)
  indd <- which.min(d)
  e <- c(time[indd])
  c(index_pos=indd, closest_time=e)
})
cp_closest
detach(df)


#oracle
attach(df)
lm2<-lm(value~time)

seg_get2<-segmented(lm2,seg.Z = ~ time, npsi=15)
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
detach(df)


######PROPHET####
#default
df_prophet <- data.frame(
  ds = as.Date(time_raw), # Tarih sC<tunu
  y = as.numeric(series_raw) # DeDer sC<tunu
)

model <- prophet(df_prophet)

changepoints <- model$changepoints
changepoints <- as.numeric(as.Date(changepoints))
print(changepoints)

indeksler <- which(time_raw %in% changepoints)
print(indeksler)



# Time series grafiDini C'izme model 
changepoints_df <- data.frame(ds = as.Date(changepoints), y = max(df_prophet$y))

p <- ggplot(df_prophet, aes(x = ds, y = y)) +
  geom_line() +
  geom_vline(data = changepoints_df, aes(xintercept = as.numeric(ds)), color = "red", linetype = "dashed") +
  labs(title = "Time Series with Changepoints",
       x = "Date",
       y = "Value") +
  theme_minimal()

print(p)

## oracle

model2 <- prophet(df_prophet, n.changepoints = 2)
changepoints2 <- model2$changepoints
changepoints2 <- as.numeric(as.Date(changepoints2))
print(changepoints2)

indeksler2 <- which(time_raw %in% changepoints2)
print(indeksler2)




# Time series grafiDini C'izme model2

changepoints_df2 <- data.frame(ds = as.Date(changepoints2), y = max(df_prophet$y))

p2 <- ggplot(df_prophet, aes(x = ds, y = y)) +
  geom_line() +
  geom_vline(data = changepoints_df2, aes(xintercept = as.numeric(ds)), color = "red", linetype = "dashed") +
  labs(title = "Time Series with Changepoints",
       x = "Date",
       y = "Value") +
  theme_minimal()

print(p2)

##

model3 <- prophet(df_prophet,n.changepoints = 9)
changepoints3 <- model3$changepoints
changepoints3 <- as.numeric(as.Date(changepoints3))
print(changepoints3)

indeksler3 <- which(time_raw %in% changepoints3)
print(indeksler3)




# Time series grafiDini C'izme model3

changepoints_df3 <- data.frame(ds = as.Date(changepoints3), y = max(df_prophet$y))

p3 <- ggplot(df_prophet, aes(x = ds, y = y)) +
  geom_line() +
  geom_vline(data = changepoints_df3, aes(xintercept = as.numeric(ds)), color = "red", linetype = "dashed") +
  labs(title = "Time Series with Changepoints",
       x = "Date",
       y = "Value") +
  theme_minimal()

print(p3)

## bu en iyisi

model4 <- prophet(df_prophet,n.changepoints = 15)
changepoints4 <- model4$changepoints
changepoints4 <- as.numeric(as.Date(changepoints4))
print(changepoints4)

indeksler4 <- which(time_raw %in% changepoints4)
print(indeksler4)



# Time series grafiDini C'izme model3

changepoints_df4 <- data.frame(ds = as.Date(changepoints4), y = max(df_prophet$y))

p4 <- ggplot(df_prophet, aes(x = ds, y = y)) +
  geom_line() +
  geom_vline(data = changepoints_df4, aes(xintercept = as.numeric(ds)), color = "red", linetype = "dashed") +
  labs(title = "Time Series with Changepoints",
       x = "Date",
       y = "Value") +
  theme_minimal()

print(p4)

## en iyi

model5 <- prophet(df_prophet,n.changepoints = 20)
changepoints5 <- model5$changepoints
changepoints5 <- as.numeric(as.Date(changepoints5))
print(changepoints5)

indeksler5 <- which(time_raw %in% changepoints5)
print(indeksler5)



# Time series grafiDini C'izme model3

changepoints_df5 <- data.frame(ds = as.Date(changepoints5), y = max(df_prophet$y))

p5 <- ggplot(df_prophet, aes(x = ds, y = y)) +
  geom_line() +
  geom_vline(data = changepoints_df5, aes(xintercept = as.numeric(ds)), color = "red", linetype = "dashed") +
  labs(title = "Time Series with Changepoints",
       x = "Date",
       y = "Value") +
  theme_minimal()

print(p5)

                              #### BRENT SPOT DATA ####

brent_spot<-'{
  "name": "brent_spot",
  "longname": "Brent Spot Price",
  "n_obs": 500,
  "n_dim": 1,
  "time": {
    "type": "string",
    "format": "%Y-%m-%d",
    "index": [
      0,
      1,
      2,
      3,
      4,
      5,
      6,
      7,
      8,
      9,
      10,
      11,
      12,
      13,
      14,
      15,
      16,
      17,
      18,
      19,
      20,
      21,
      22,
      23,
      24,
      25,
      26,
      27,
      28,
      29,
      30,
      31,
      32,
      33,
      34,
      35,
      36,
      37,
      38,
      39,
      40,
      41,
      42,
      43,
      44,
      45,
      46,
      47,
      48,
      49,
      50,
      51,
      52,
      53,
      54,
      55,
      56,
      57,
      58,
      59,
      60,
      61,
      62,
      63,
      64,
      65,
      66,
      67,
      68,
      69,
      70,
      71,
      72,
      73,
      74,
      75,
      76,
      77,
      78,
      79,
      80,
      81,
      82,
      83,
      84,
      85,
      86,
      87,
      88,
      89,
      90,
      91,
      92,
      93,
      94,
      95,
      96,
      97,
      98,
      99,
      100,
      101,
      102,
      103,
      104,
      105,
      106,
      107,
      108,
      109,
      110,
      111,
      112,
      113,
      114,
      115,
      116,
      117,
      118,
      119,
      120,
      121,
      122,
      123,
      124,
      125,
      126,
      127,
      128,
      129,
      130,
      131,
      132,
      133,
      134,
      135,
      136,
      137,
      138,
      139,
      140,
      141,
      142,
      143,
      144,
      145,
      146,
      147,
      148,
      149,
      150,
      151,
      152,
      153,
      154,
      155,
      156,
      157,
      158,
      159,
      160,
      161,
      162,
      163,
      164,
      165,
      166,
      167,
      168,
      169,
      170,
      171,
      172,
      173,
      174,
      175,
      176,
      177,
      178,
      179,
      180,
      181,
      182,
      183,
      184,
      185,
      186,
      187,
      188,
      189,
      190,
      191,
      192,
      193,
      194,
      195,
      196,
      197,
      198,
      199,
      200,
      201,
      202,
      203,
      204,
      205,
      206,
      207,
      208,
      209,
      210,
      211,
      212,
      213,
      214,
      215,
      216,
      217,
      218,
      219,
      220,
      221,
      222,
      223,
      224,
      225,
      226,
      227,
      228,
      229,
      230,
      231,
      232,
      233,
      234,
      235,
      236,
      237,
      238,
      239,
      240,
      241,
      242,
      243,
      244,
      245,
      246,
      247,
      248,
      249,
      250,
      251,
      252,
      253,
      254,
      255,
      256,
      257,
      258,
      259,
      260,
      261,
      262,
      263,
      264,
      265,
      266,
      267,
      268,
      269,
      270,
      271,
      272,
      273,
      274,
      275,
      276,
      277,
      278,
      279,
      280,
      281,
      282,
      283,
      284,
      285,
      286,
      287,
      288,
      289,
      290,
      291,
      292,
      293,
      294,
      295,
      296,
      297,
      298,
      299,
      300,
      301,
      302,
      303,
      304,
      305,
      306,
      307,
      308,
      309,
      310,
      311,
      312,
      313,
      314,
      315,
      316,
      317,
      318,
      319,
      320,
      321,
      322,
      323,
      324,
      325,
      326,
      327,
      328,
      329,
      330,
      331,
      332,
      333,
      334,
      335,
      336,
      337,
      338,
      339,
      340,
      341,
      342,
      343,
      344,
      345,
      346,
      347,
      348,
      349,
      350,
      351,
      352,
      353,
      354,
      355,
      356,
      357,
      358,
      359,
      360,
      361,
      362,
      363,
      364,
      365,
      366,
      367,
      368,
      369,
      370,
      371,
      372,
      373,
      374,
      375,
      376,
      377,
      378,
      379,
      380,
      381,
      382,
      383,
      384,
      385,
      386,
      387,
      388,
      389,
      390,
      391,
      392,
      393,
      394,
      395,
      396,
      397,
      398,
      399,
      400,
      401,
      402,
      403,
      404,
      405,
      406,
      407,
      408,
      409,
      410,
      411,
      412,
      413,
      414,
      415,
      416,
      417,
      418,
      419,
      420,
      421,
      422,
      423,
      424,
      425,
      426,
      427,
      428,
      429,
      430,
      431,
      432,
      433,
      434,
      435,
      436,
      437,
      438,
      439,
      440,
      441,
      442,
      443,
      444,
      445,
      446,
      447,
      448,
      449,
      450,
      451,
      452,
      453,
      454,
      455,
      456,
      457,
      458,
      459,
      460,
      461,
      462,
      463,
      464,
      465,
      466,
      467,
      468,
      469,
      470,
      471,
      472,
      473,
      474,
      475,
      476,
      477,
      478,
      479,
      480,
      481,
      482,
      483,
      484,
      485,
      486,
      487,
      488,
      489,
      490,
      491,
      492,
      493,
      494,
      495,
      496,
      497,
      498,
      499
    ],
    "raw": [
      "2000-01-04",
      "2000-01-18",
      "2000-02-01",
      "2000-02-15",
      "2000-02-29",
      "2000-03-14",
      "2000-03-28",
      "2000-04-11",
      "2000-04-27",
      "2000-05-12",
      "2000-05-26",
      "2000-06-12",
      "2000-06-26",
      "2000-07-10",
      "2000-07-24",
      "2000-08-07",
      "2000-08-21",
      "2000-09-04",
      "2000-09-18",
      "2000-10-02",
      "2000-10-16",
      "2000-10-30",
      "2000-11-13",
      "2000-11-27",
      "2000-12-11",
      "2000-12-27",
      "2001-01-11",
      "2001-01-25",
      "2001-02-08",
      "2001-02-22",
      "2001-03-08",
      "2001-03-22",
      "2001-04-05",
      "2001-04-19",
      "2001-05-03",
      "2001-05-17",
      "2001-06-01",
      "2001-06-15",
      "2001-06-29",
      "2001-07-13",
      "2001-07-27",
      "2001-08-10",
      "2001-08-24",
      "2001-09-07",
      "2001-09-21",
      "2001-10-05",
      "2001-10-19",
      "2001-11-02",
      "2001-11-16",
      "2001-11-30",
      "2001-12-14",
      "2002-01-02",
      "2002-01-16",
      "2002-01-30",
      "2002-02-13",
      "2002-02-27",
      "2002-03-13",
      "2002-03-27",
      "2002-04-10",
      "2002-04-24",
      "2002-05-09",
      "2002-05-23",
      "2002-06-10",
      "2002-06-24",
      "2002-07-08",
      "2002-07-22",
      "2002-08-05",
      "2002-08-19",
      "2002-09-02",
      "2002-09-16",
      "2002-09-30",
      "2002-10-14",
      "2002-10-28",
      "2002-11-11",
      "2002-11-25",
      "2002-12-09",
      "2002-12-23",
      "2003-01-09",
      "2003-01-23",
      "2003-02-06",
      "2003-02-20",
      "2003-03-06",
      "2003-03-20",
      "2003-04-03",
      "2003-04-17",
      "2003-05-01",
      "2003-05-15",
      "2003-05-29",
      "2003-06-12",
      "2003-06-26",
      "2003-07-10",
      "2003-07-24",
      "2003-08-07",
      "2003-08-21",
      "2003-09-04",
      "2003-09-18",
      "2003-10-02",
      "2003-10-16",
      "2003-10-30",
      "2003-11-13",
      "2003-11-27",
      "2003-12-11",
      "2003-12-29",
      "2004-01-13",
      "2004-01-27",
      "2004-02-10",
      "2004-02-24",
      "2004-03-09",
      "2004-03-23",
      "2004-04-06",
      "2004-04-20",
      "2004-05-04",
      "2004-05-18",
      "2004-06-01",
      "2004-06-15",
      "2004-06-29",
      "2004-07-13",
      "2004-07-27",
      "2004-08-10",
      "2004-08-24",
      "2004-09-07",
      "2004-09-21",
      "2004-10-05",
      "2004-10-19",
      "2004-11-02",
      "2004-11-16",
      "2004-11-30",
      "2004-12-14",
      "2004-12-28",
      "2005-01-12",
      "2005-01-26",
      "2005-02-09",
      "2005-02-23",
      "2005-03-09",
      "2005-03-23",
      "2005-04-07",
      "2005-04-21",
      "2005-05-05",
      "2005-05-19",
      "2005-06-02",
      "2005-06-16",
      "2005-06-30",
      "2005-07-14",
      "2005-07-28",
      "2005-08-11",
      "2005-08-25",
      "2005-09-08",
      "2005-09-22",
      "2005-10-06",
      "2005-10-20",
      "2005-11-03",
      "2005-11-17",
      "2005-12-01",
      "2005-12-15",
      "2005-12-30",
      "2006-01-16",
      "2006-01-30",
      "2006-02-13",
      "2006-02-27",
      "2006-03-13",
      "2006-03-27",
      "2006-04-10",
      "2006-04-26",
      "2006-05-10",
      "2006-05-24",
      "2006-06-07",
      "2006-06-21",
      "2006-07-05",
      "2006-07-19",
      "2006-08-02",
      "2006-08-16",
      "2006-08-30",
      "2006-09-13",
      "2006-09-27",
      "2006-10-11",
      "2006-10-25",
      "2006-11-08",
      "2006-11-22",
      "2006-12-06",
      "2006-12-20",
      "2007-01-08",
      "2007-01-23",
      "2007-02-06",
      "2007-02-21",
      "2007-03-07",
      "2007-03-21",
      "2007-04-04",
      "2007-04-20",
      "2007-05-04",
      "2007-05-18",
      "2007-06-04",
      "2007-06-18",
      "2007-07-02",
      "2007-07-17",
      "2007-07-31",
      "2007-08-14",
      "2007-08-28",
      "2007-09-12",
      "2007-09-26",
      "2007-10-10",
      "2007-10-24",
      "2007-11-07",
      "2007-11-21",
      "2007-12-06",
      "2007-12-20",
      "2008-01-08",
      "2008-01-23",
      "2008-02-06",
      "2008-02-21",
      "2008-03-06",
      "2008-03-20",
      "2008-04-04",
      "2008-04-18",
      "2008-05-02",
      "2008-05-16",
      "2008-06-02",
      "2008-06-16",
      "2008-06-30",
      "2008-07-15",
      "2008-07-29",
      "2008-08-12",
      "2008-08-26",
      "2008-09-10",
      "2008-09-24",
      "2008-10-08",
      "2008-10-22",
      "2008-11-05",
      "2008-11-19",
      "2008-12-04",
      "2008-12-18",
      "2009-01-05",
      "2009-01-20",
      "2009-02-03",
      "2009-02-18",
      "2009-03-04",
      "2009-03-18",
      "2009-04-01",
      "2009-04-16",
      "2009-04-30",
      "2009-05-14",
      "2009-05-29",
      "2009-06-12",
      "2009-06-26",
      "2009-07-13",
      "2009-07-27",
      "2009-08-10",
      "2009-08-24",
      "2009-09-08",
      "2009-09-22",
      "2009-10-06",
      "2009-10-20",
      "2009-11-03",
      "2009-11-17",
      "2009-12-02",
      "2009-12-16",
      "2009-12-31",
      "2010-01-15",
      "2010-02-01",
      "2010-02-16",
      "2010-03-02",
      "2010-03-16",
      "2010-03-30",
      "2010-04-14",
      "2010-04-28",
      "2010-05-12",
      "2010-05-26",
      "2010-06-10",
      "2010-06-24",
      "2010-07-09",
      "2010-07-23",
      "2010-08-06",
      "2010-08-20",
      "2010-09-03",
      "2010-09-20",
      "2010-10-04",
      "2010-10-18",
      "2010-11-01",
      "2010-11-15",
      "2010-11-30",
      "2010-12-14",
      "2010-12-29",
      "2011-01-12",
      "2011-01-27",
      "2011-02-10",
      "2011-02-25",
      "2011-03-11",
      "2011-03-25",
      "2011-04-08",
      "2011-04-26",
      "2011-05-11",
      "2011-05-25",
      "2011-06-09",
      "2011-06-23",
      "2011-07-08",
      "2011-07-22",
      "2011-08-05",
      "2011-08-19",
      "2011-09-06",
      "2011-09-20",
      "2011-10-04",
      "2011-10-18",
      "2011-11-01",
      "2011-11-15",
      "2011-11-30",
      "2011-12-14",
      "2011-12-30",
      "2012-01-17",
      "2012-01-31",
      "2012-02-14",
      "2012-02-29",
      "2012-03-14",
      "2012-03-28",
      "2012-04-13",
      "2012-04-27",
      "2012-05-14",
      "2012-05-29",
      "2012-06-12",
      "2012-06-26",
      "2012-07-11",
      "2012-07-25",
      "2012-08-08",
      "2012-08-22",
      "2012-09-06",
      "2012-09-20",
      "2012-10-04",
      "2012-10-18",
      "2012-11-01",
      "2012-11-15",
      "2012-11-30",
      "2012-12-14",
      "2012-12-31",
      "2013-01-15",
      "2013-01-30",
      "2013-02-13",
      "2013-02-28",
      "2013-03-14",
      "2013-03-28",
      "2013-04-12",
      "2013-04-26",
      "2013-05-10",
      "2013-05-24",
      "2013-06-10",
      "2013-06-24",
      "2013-07-09",
      "2013-07-23",
      "2013-08-06",
      "2013-08-20",
      "2013-09-04",
      "2013-09-18",
      "2013-10-02",
      "2013-10-16",
      "2013-10-30",
      "2013-11-13",
      "2013-11-27",
      "2013-12-12",
      "2013-12-27",
      "2014-01-13",
      "2014-01-27",
      "2014-02-10",
      "2014-02-25",
      "2014-03-11",
      "2014-03-25",
      "2014-04-08",
      "2014-04-23",
      "2014-05-07",
      "2014-05-21",
      "2014-06-04",
      "2014-06-18",
      "2014-07-02",
      "2014-07-17",
      "2014-07-31",
      "2014-08-14",
      "2014-08-28",
      "2014-09-12",
      "2014-09-26",
      "2014-10-15",
      "2014-10-29",
      "2014-11-12",
      "2014-11-26",
      "2014-12-11",
      "2014-12-26",
      "2015-01-12",
      "2015-01-27",
      "2015-02-10",
      "2015-02-24",
      "2015-03-10",
      "2015-03-24",
      "2015-04-08",
      "2015-04-22",
      "2015-05-06",
      "2015-05-20",
      "2015-06-04",
      "2015-06-18",
      "2015-07-02",
      "2015-07-16",
      "2015-07-30",
      "2015-08-13",
      "2015-08-27",
      "2015-09-10",
      "2015-09-24",
      "2015-10-08",
      "2015-10-22",
      "2015-11-05",
      "2015-11-19",
      "2015-12-03",
      "2015-12-17",
      "2015-12-31",
      "2016-01-14",
      "2016-01-28",
      "2016-02-11",
      "2016-02-26",
      "2016-03-11",
      "2016-03-28",
      "2016-04-11",
      "2016-04-25",
      "2016-05-09",
      "2016-05-23",
      "2016-06-07",
      "2016-06-21",
      "2016-07-05",
      "2016-07-19",
      "2016-08-02",
      "2016-08-16",
      "2016-08-30",
      "2016-09-13",
      "2016-09-27",
      "2016-10-11",
      "2016-10-25",
      "2016-11-09",
      "2016-11-23",
      "2016-12-07",
      "2016-12-21",
      "2017-01-06",
      "2017-01-20",
      "2017-02-03",
      "2017-02-17",
      "2017-03-03",
      "2017-03-17",
      "2017-03-31",
      "2017-04-17",
      "2017-05-01",
      "2017-05-15",
      "2017-05-29",
      "2017-06-12",
      "2017-06-26",
      "2017-07-10",
      "2017-07-24",
      "2017-08-07",
      "2017-08-21",
      "2017-09-04",
      "2017-09-18",
      "2017-10-02",
      "2017-10-16",
      "2017-10-30",
      "2017-11-13",
      "2017-11-27",
      "2017-12-11",
      "2017-12-27",
      "2018-01-11",
      "2018-01-25",
      "2018-02-08",
      "2018-02-22",
      "2018-03-08",
      "2018-03-22",
      "2018-04-06",
      "2018-04-20",
      "2018-05-04",
      "2018-05-18",
      "2018-06-04",
      "2018-06-18",
      "2018-07-02",
      "2018-07-16",
      "2018-07-30",
      "2018-08-13",
      "2018-08-27",
      "2018-09-10",
      "2018-09-24",
      "2018-10-08",
      "2018-10-22",
      "2018-11-05",
      "2018-11-19",
      "2018-12-03",
      "2018-12-17",
      "2019-01-07",
      "2019-01-21",
      "2019-02-04",
      "2019-02-18",
      "2019-03-04",
      "2019-03-18",
      "2019-04-01",
      "2019-04-15",
      "2019-04-30",
      "2019-05-14",
      "2019-05-28",
      "2019-06-11",
      "2019-06-25",
      "2019-07-09",
      "2019-07-23",
      "2019-08-06",
      "2019-08-20"
    ]
  },
  "series": [
    {
      "label": "Dollars/Barrel",
      "type": "float",
      "raw": [
        23.95,
        26.31,
        27.35,
        28.12,
        29.01,
        28.83,
        24.8,
        21.05,
        23.07,
        27.98,
        30.09,
        30.1,
        30.9,
        30.58,
        26.84,
        27.56,
        31.34,
        36.02,
        34.55,
        29.65,
        32.03,
        31.09,
        32.5,
        33.47,
        27.28,
        22.58,
        25.6,
        26.94,
        30.68,
        25.75,
        26.37,
        23.45,
        24.87,
        25.86,
        27.46,
        28.57,
        28.86,
        28.13,
        26.21,
        24.02,
        24.82,
        25.78,
        26.33,
        27.54,
        25.17,
        21.46,
        19.91,
        18.63,
        16.86,
        18.92,
        18.54,
        20.13,
        18.5,
        18.71,
        21.41,
        20.73,
        23.6,
        25.13,
        25.13,
        26.2,
        26.09,
        24.01,
        22.83,
        24.99,
        25.08,
        25.7,
        25.2,
        27.44,
        27.45,
        28.38,
        29.11,
        28.56,
        25.44,
        23.86,
        24.85,
        26.06,
        30.61,
        30.26,
        31.4,
        31.81,
        32.76,
        34.39,
        28.0,
        26.75,
        25.36,
        23.79,
        26.77,
        26.39,
        28.48,
        27.06,
        29.37,
        27.73,
        30.06,
        29.95,
        27.49,
        25.56,
        28.9,
        31.05,
        27.47,
        29.01,
        28.9,
        29.79,
        29.17,
        32.55,
        31.05,
        30.06,
        31.6,
        33.72,
        34.4,
        31.48,
        33.56,
        36.07,
        38.43,
        39.05,
        34.66,
        32.61,
        36.68,
        40.09,
        42.5,
        42.99,
        40.19,
        44.56,
        47.1,
        49.21,
        46.25,
        40.48,
        44.23,
        37.03,
        40.24,
        43.75,
        45.94,
        42.49,
        48.16,
        54.11,
        51.52,
        52.57,
        51.73,
        49.81,
        46.92,
        51.3,
        54.3,
        55.36,
        56.79,
        58.28,
        65.9,
        65.88,
        61.66,
        64.64,
        57.2,
        57.04,
        59.55,
        53.8,
        53.65,
        59.67,
        58.34,
        62.34,
        65.14,
        59.16,
        59.44,
        60.99,
        62.59,
        68.2,
        73.46,
        69.83,
        68.45,
        67.17,
        68.17,
        72.88,
        71.62,
        77.63,
        72.67,
        67.15,
        61.28,
        57.6,
        57.75,
        58.79,
        57.18,
        58.66,
        63.7,
        63.13,
        52.82,
        53.93,
        58.04,
        57.74,
        61.13,
        60.18,
        68.09,
        66.34,
        65.14,
        69.26,
        70.9,
        72.33,
        72.9,
        77.59,
        77.01,
        69.29,
        69.66,
        77.15,
        76.21,
        77.85,
        83.47,
        94.85,
        94.97,
        88.46,
        91.06,
        96.37,
        87.06,
        88.73,
        97.52,
        103.47,
        99.78,
        102.21,
        110.67,
        111.92,
        122.98,
        128.5,
        133.9,
        138.4,
        136.02,
        125.77,
        108.98,
        112.2,
        96.0,
        102.09,
        80.77,
        62.95,
        61.09,
        48.35,
        43.83,
        40.19,
        45.84,
        41.22,
        43.15,
        39.41,
        46.07,
        45.22,
        45.92,
        51.83,
        50.3,
        56.25,
        64.98,
        70.62,
        68.1,
        58.25,
        69.78,
        73.79,
        74.34,
        69.2,
        69.65,
        68.51,
        76.51,
        75.68,
        77.36,
        76.96,
        73.34,
        77.91,
        76.85,
        71.58,
        74.82,
        77.5,
        79.45,
        79.46,
        85.81,
        84.59,
        78.7,
        70.59,
        74.33,
        75.17,
        75.2,
        77.27,
        81.28,
        73.48,
        75.03,
        79.42,
        83.42,
        82.3,
        84.06,
        85.49,
        86.02,
        90.63,
        93.52,
        97.86,
        96.48,
        100.74,
        111.47,
        114.07,
        115.45,
        126.3,
        124.55,
        115.66,
        114.47,
        119.95,
        108.27,
        117.4,
        118.99,
        106.92,
        109.37,
        113.29,
        114.39,
        101.84,
        112.08,
        106.97,
        111.9,
        111.22,
        105.72,
        108.09,
        110.55,
        110.26,
        118.3,
        122.23,
        126.98,
        124.41,
        120.62,
        119.3,
        110.79,
        107.55,
        96.59,
        90.19,
        99.23,
        102.35,
        113.42,
        115.77,
        114.5,
        109.41,
        110.48,
        112.58,
        108.84,
        110.07,
        110.84,
        109.28,
        110.8,
        111.72,
        115.42,
        118.43,
        112.2,
        108.1,
        108.46,
        100.58,
        102.39,
        101.31,
        101.24,
        103.87,
        99.8,
        107.9,
        109.27,
        108.77,
        110.74,
        115.65,
        109.09,
        109.09,
        110.79,
        108.41,
        106.9,
        111.32,
        108.99,
        112.06,
        108.02,
        108.72,
        110.18,
        109.19,
        108.35,
        107.01,
        105.83,
        108.48,
        108.17,
        111.32,
        109.07,
        114.25,
        110.18,
        106.04,
        104.94,
        101.15,
        100.71,
        96.31,
        95.08,
        84.02,
        86.91,
        80.42,
        77.39,
        63.65,
        58.72,
        46.9,
        46.55,
        55.79,
        60.33,
        55.95,
        53.61,
        56.42,
        60.12,
        66.22,
        63.52,
        60.34,
        61.37,
        61.73,
        57.31,
        54.29,
        48.01,
        44.46,
        47.77,
        47.06,
        52.13,
        46.59,
        47.19,
        42.22,
        42.0,
        36.29,
        36.61,
        28.84,
        33.01,
        28.82,
        35.76,
        39.41,
        38.33,
        41.58,
        42.97,
        42.43,
        47.77,
        49.76,
        48.18,
        45.64,
        45.7,
        40.0,
        48.27,
        47.94,
        46.48,
        44.95,
        50.48,
        49.08,
        43.88,
        46.54,
        51.9,
        53.01,
        55.9,
        55.04,
        55.92,
        54.48,
        54.12,
        50.58,
        52.2,
        54.79,
        50.41,
        51.29,
        52.25,
        47.18,
        44.09,
        46.57,
        47.81,
        51.42,
        51.94,
        52.6,
        55.5,
        55.67,
        57.49,
        60.65,
        62.94,
        63.25,
        65.62,
        66.03,
        70.36,
        71.08,
        64.26,
        66.12,
        63.87,
        68.98,
        66.51,
        74.62,
        74.75,
        78.38,
        73.41,
        74.87,
        76.71,
        71.03,
        74.99,
        70.62,
        74.41,
        76.77,
        80.89,
        84.22,
        80.45,
        72.68,
        64.14,
        60.17,
        57.59,
        57.1,
        62.18,
        62.26,
        66.41,
        64.44,
        66.65,
        69.08,
        70.9,
        72.19,
        72.53,
        70.19,
        63.56,
        66.24,
        64.3,
        62.28,
        58.63,
        59.03
      ]
    }
  ]
}'

brent_spot
parsed_data <- fromJSON(brent_spot)

cat("Name:", parsed_data$name, "\n")
cat("Longname:", parsed_data$longname, "\n")
cat("Number of Observations:", parsed_data$n_obs, "\n")
cat("Number of Dimensions:", parsed_data$n_dim, "\n")

cat("Time Format:", parsed_data$time$format, "\n")
cat("First 5 Time Indices:", head(parsed_data$time$index), "\n")
cat("First 5 Time Values:", head(parsed_data$time$raw), "\n")
cat("Series Label:", parsed_data$series$label, "\n")
cat("First 5 Series Values:", head(parsed_data$series$raw[[1]], 5), "\n")

# Check for non-numeric values
brent_spot_list <- fromJSON(brent_spot)

date_raw <- as.numeric(as.Date(brent_spot_list$time$raw))
price_raw <- brent_spot_list$series$raw[[1]]

# Creating a data frame
df <- data.frame(
  index = 1:length(date_raw),
  time = as.numeric(date_raw),
  value = as.numeric(price_raw)
  
)


#ORTALAMADA DEGISIM NOKTASI ARAYAN ALGORITMALAR
###binary segmentation
tsdata <- ts(df$value, start = min(df$index), end = max(df$index), frequency = 1)

###Zaman serisini gC6ster
print(tsdata)


###brent_spot annotations
###6-> 219,230,288
###8->227,381
###9->86,219,230,279,375
###12->169,172,217,228,287,368,382,389,409
###13->170,180,219,229,246,271,286,379,409,444,483


###Amoc
d.m_amoc <- cpt.mean(tsdata, method = "AMOC")
plot(d.m_amoc)
abline(v = cpts(d.m_amoc), col = "red", lty = 2,lwd=2)
cpts(d.m_amoc)

###oracle settings
o.m_amoc <- cpt.mean(tsdata, penalty = "MBIC", method = "AMOC")
plot(o.m_amoc)
abline(v = cpts(o.m_amoc), col = "red", lty = 2,lwd=2)
cpts(o.m_amoc)

###BinSeg_1
x<-cpt.mean(tsdata,method = "BinSeg",Q=8)
plot(x)
abline(v = cpts(x), col = "red", lty = 2,lwd=2)
cpts(x)


###Q deDiEim noktasD1 sayD1sD1nD1 ayarlar
###BinSeg_2
m_binseg2 <- cpt.mean(tsdata, penalty = "BIC", method = "BinSeg", Q = 10)
plot(m_binseg2)
abline(v = cpts(m_binseg2), col = "red", lty = 2,lwd=2)
cpts(m_binseg2)

###BinSeg_3 en iyii

m_binseg3 <- cpt.mean(tsdata, penalty = "BIC", method = "BinSeg", Q = 20)
plot(m_binseg3)
abline(v = cpts(m_binseg3), col = "red", lty = 2,lwd=2)
cpts(m_binseg3)

###BinSeg_4

m_binseg4 <- cpt.mean(tsdata, penalty = "BIC", method = "BinSeg", Q = 15)
plot(m_binseg4)
abline(v = cpts(m_binseg4), col = "red", lty = 2,lwd=2)
cpts(m_binseg4)

###PELT_1
m_pelt <- cpt.mean(tsdata, method = "PELT",minseglen = 40)
plot(m_pelt, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(m_pelt), col = "blue", lty = 2,lwd=2)
cpts(m_pelt) 

###PELT_2
m_pelt2 <- cpt.mean(tsdata, penalty = "MBIC", method = "PELT", minseglen = 20)
plot(m_pelt2, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(m_pelt2), col = "blue", lty = 2,lwd=2)
cpts(m_pelt2) 

###PELT_3
m_pelt3 <- cpt.mean(tsdata, penalty = "MBIC", method = "PELT", minseglen = 30, pen.value = 5)
plot(m_pelt3, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(m_pelt3), col = "blue", lty = 2,lwd=2)
cpts(m_pelt3) 

###PELT_3 en iyiii
m_pelt4 <- cpt.mean(tsdata, penalty = "MBIC", method = "PELT", minseglen = 25, pen.value = 1)
plot(m_pelt4, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(m_pelt4), col = "blue", lty = 2,lwd=2)
cpts(m_pelt4) 

#VARYANSTA DEGISIM NOKTALARI ARAYAN ALGORITMA
v_pelt <- cpt.var(tsdata, method = "PELT")
plot(v_pelt, type = "l", cpt.col = "blue", xlab = "Index")
cpts(v_pelt)  

#HEM ORTALAMADA HEM DE VARYANSTA DEGISIM NOKTASI ARAMA
mv_pelt <- cpt.meanvar(tsdata, method = "PELT")
mv_pelt 
plot(mv_pelt)
cpts(mv_pelt)

###Segmented regression

#default
attach(df)
lm1<-lm(value~time)

seg_get<-segmented(lm1,seg.Z = ~ time,npsi = 5)
seg_get
seg_get$psi[,2]
cp <- round(seg_get$psi[,2])
cp_closest <- sapply(cp, function(bp) {
  d<- abs( time - bp)
  indd <- which.min(d)
  e <- c(time[indd])
  c(index_pos=indd, closest_time=e)
})
cp_closest
detach(df)

#oracle 1
attach(df)
lm2<-lm(value~time)

seg_get2<-segmented(lm2,seg.Z = ~ time, npsi=3)
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
detach(df)

# en iyi seg bu 
attach(df)
lm3<-lm(value~time)

seg_get3<-segmented(lm3,seg.Z = ~ time, npsi=10)
seg_get3
seg_get3$psi[,2]
cp <- round(seg_get3$psi[,2])
cp_closest3 <- sapply(cp, function(bp) {
  d<- abs( time - bp)
  indd <- which.min(d)
  e <- c(time[indd])
  c(index_pos=indd, closest_time=e)
})
cp_closest3
detach(df)
## PROPHET ##
###Cok kC6tC< buluyor(bulamD1yor)

df_prophet <- data.frame(
  ds = as.Date(date_raw),
  y = as.numeric(price_raw)
)

model <- prophet(df_prophet,n.changepoints = 10)

changepoints <- model$changepoints
changepoints <- as.numeric(as.Date(changepoints))
print(changepoints)

indeksler <- which(date_raw %in% changepoints)
print(indeksler)


# Time series grafiDini C'izme model

changepoints_df <- data.frame(ds = as.Date(changepoints), y = max(df_prophet$y))

p <- ggplot(df_prophet, aes(x = ds, y = y)) +
  geom_line() +
  geom_vline(data = changepoints_df, aes(xintercept = as.numeric(ds)), color = "red", linetype = "dashed") +
  labs(title = "Time Series with Changepoints",
       x = "Date",
       y = "Value") +
  theme_minimal()

print(p)

## en iyi bu

model2 <- prophet(df_prophet, n.changepoints = 20)
changepoints2 <- model2$changepoints
changepoints2 <- as.numeric(as.Date(changepoints2))
print(changepoints2)

indeksler2 <- which(date_raw %in% changepoints2)
print(indeksler2)

# Time series grafiDini C'izme model2

changepoints_df2 <- data.frame(ds = as.Date(changepoints2), y = max(df_prophet$y))

p2 <- ggplot(df_prophet, aes(x = ds, y = y)) +
  geom_line() +
  geom_vline(data = changepoints_df2, aes(xintercept = as.numeric(ds)), color = "red", linetype = "dashed") +
  labs(title = "Time Series with Changepoints",
       x = "Date",
       y = "Value") +
  theme_minimal()

print(p2)

##

model3 <- prophet(df_prophet,n.changepoints = 15)
changepoints3 <- model3$changepoints
changepoints3 <- as.numeric(as.Date(changepoints3))
print(changepoints3)

indeksler3 <- which(date_raw %in% changepoints3)
print(indeksler3)

# Time series grafiDini C'izme model3

changepoints_df3 <- data.frame(ds = as.Date(changepoints3), y = max(df_prophet$y))

p3 <- ggplot(df_prophet, aes(x = ds, y = y)) +
  geom_line() +
  geom_vline(data = changepoints_df3, aes(xintercept = as.numeric(ds)), color = "red", linetype = "dashed") +
  labs(title = "Time Series with Changepoints",
       x = "Date",
       y = "Value") +
  theme_minimal()

print(p3)

                        #### CHILDREN PER WOMAN DATA ####

children_per_woman<-'{
    "name": "children_per_woman",
    "longname": "Average Children per Woman",
    "n_obs": 301,
    "n_dim": 1,
    "time": {
        "format": "%Y",
        "index": [
            0,
            1,
            2,
            3,
            4,
            5,
            6,
            7,
            8,
            9,
            10,
            11,
            12,
            13,
            14,
            15,
            16,
            17,
            18,
            19,
            20,
            21,
            22,
            23,
            24,
            25,
            26,
            27,
            28,
            29,
            30,
            31,
            32,
            33,
            34,
            35,
            36,
            37,
            38,
            39,
            40,
            41,
            42,
            43,
            44,
            45,
            46,
            47,
            48,
            49,
            50,
            51,
            52,
            53,
            54,
            55,
            56,
            57,
            58,
            59,
            60,
            61,
            62,
            63,
            64,
            65,
            66,
            67,
            68,
            69,
            70,
            71,
            72,
            73,
            74,
            75,
            76,
            77,
            78,
            79,
            80,
            81,
            82,
            83,
            84,
            85,
            86,
            87,
            88,
            89,
            90,
            91,
            92,
            93,
            94,
            95,
            96,
            97,
            98,
            99,
            100,
            101,
            102,
            103,
            104,
            105,
            106,
            107,
            108,
            109,
            110,
            111,
            112,
            113,
            114,
            115,
            116,
            117,
            118,
            119,
            120,
            121,
            122,
            123,
            124,
            125,
            126,
            127,
            128,
            129,
            130,
            131,
            132,
            133,
            134,
            135,
            136,
            137,
            138,
            139,
            140,
            141,
            142,
            143,
            144,
            145,
            146,
            147,
            148,
            149,
            150,
            151,
            152,
            153,
            154,
            155,
            156,
            157,
            158,
            159,
            160,
            161,
            162,
            163,
            164,
            165,
            166,
            167,
            168,
            169,
            170,
            171,
            172,
            173,
            174,
            175,
            176,
            177,
            178,
            179,
            180,
            181,
            182,
            183,
            184,
            185,
            186,
            187,
            188,
            189,
            190,
            191,
            192,
            193,
            194,
            195,
            196,
            197,
            198,
            199,
            200,
            201,
            202,
            203,
            204,
            205,
            206,
            207,
            208,
            209,
            210,
            211,
            212,
            213,
            214,
            215,
            216,
            217,
            218,
            219,
            220,
            221,
            222,
            223,
            224,
            225,
            226,
            227,
            228,
            229,
            230,
            231,
            232,
            233,
            234,
            235,
            236,
            237,
            238,
            239,
            240,
            241,
            242,
            243,
            244,
            245,
            246,
            247,
            248,
            249,
            250,
            251,
            252,
            253,
            254,
            255,
            256,
            257,
            258,
            259,
            260,
            261,
            262,
            263,
            264,
            265,
            266,
            267,
            268,
            269,
            270,
            271,
            272,
            273,
            274,
            275,
            276,
            277,
            278,
            279,
            280,
            281,
            282,
            283,
            284,
            285,
            286,
            287,
            288,
            289,
            290,
            291,
            292,
            293,
            294,
            295,
            296,
            297,
            298,
            299,
            300
        ],
        "raw": [
            "1800",
            "1801",
            "1802",
            "1803",
            "1804",
            "1805",
            "1806",
            "1807",
            "1808",
            "1809",
            "1810",
            "1811",
            "1812",
            "1813",
            "1814",
            "1815",
            "1816",
            "1817",
            "1818",
            "1819",
            "1820",
            "1821",
            "1822",
            "1823",
            "1824",
            "1825",
            "1826",
            "1827",
            "1828",
            "1829",
            "1830",
            "1831",
            "1832",
            "1833",
            "1834",
            "1835",
            "1836",
            "1837",
            "1838",
            "1839",
            "1840",
            "1841",
            "1842",
            "1843",
            "1844",
            "1845",
            "1846",
            "1847",
            "1848",
            "1849",
            "1850",
            "1851",
            "1852",
            "1853",
            "1854",
            "1855",
            "1856",
            "1857",
            "1858",
            "1859",
            "1860",
            "1861",
            "1862",
            "1863",
            "1864",
            "1865",
            "1866",
            "1867",
            "1868",
            "1869",
            "1870",
            "1871",
            "1872",
            "1873",
            "1874",
            "1875",
            "1876",
            "1877",
            "1878",
            "1879",
            "1880",
            "1881",
            "1882",
            "1883",
            "1884",
            "1885",
            "1886",
            "1887",
            "1888",
            "1889",
            "1890",
            "1891",
            "1892",
            "1893",
            "1894",
            "1895",
            "1896",
            "1897",
            "1898",
            "1899",
            "1900",
            "1901",
            "1902",
            "1903",
            "1904",
            "1905",
            "1906",
            "1907",
            "1908",
            "1909",
            "1910",
            "1911",
            "1912",
            "1913",
            "1914",
            "1915",
            "1916",
            "1917",
            "1918",
            "1919",
            "1920",
            "1921",
            "1922",
            "1923",
            "1924",
            "1925",
            "1926",
            "1927",
            "1928",
            "1929",
            "1930",
            "1931",
            "1932",
            "1933",
            "1934",
            "1935",
            "1936",
            "1937",
            "1938",
            "1939",
            "1940",
            "1941",
            "1942",
            "1943",
            "1944",
            "1945",
            "1946",
            "1947",
            "1948",
            "1949",
            "1950",
            "1951",
            "1952",
            "1953",
            "1954",
            "1955",
            "1956",
            "1957",
            "1958",
            "1959",
            "1960",
            "1961",
            "1962",
            "1963",
            "1964",
            "1965",
            "1966",
            "1967",
            "1968",
            "1969",
            "1970",
            "1971",
            "1972",
            "1973",
            "1974",
            "1975",
            "1976",
            "1977",
            "1978",
            "1979",
            "1980",
            "1981",
            "1982",
            "1983",
            "1984",
            "1985",
            "1986",
            "1987",
            "1988",
            "1989",
            "1990",
            "1991",
            "1992",
            "1993",
            "1994",
            "1995",
            "1996",
            "1997",
            "1998",
            "1999",
            "2000",
            "2001",
            "2002",
            "2003",
            "2004",
            "2005",
            "2006",
            "2007",
            "2008",
            "2009",
            "2010",
            "2011",
            "2012",
            "2013",
            "2014",
            "2015",
            "2016",
            "2017",
            "2018",
            "2019",
            "2020",
            "2021",
            "2022",
            "2023",
            "2024",
            "2025",
            "2026",
            "2027",
            "2028",
            "2029",
            "2030",
            "2031",
            "2032",
            "2033",
            "2034",
            "2035",
            "2036",
            "2037",
            "2038",
            "2039",
            "2040",
            "2041",
            "2042",
            "2043",
            "2044",
            "2045",
            "2046",
            "2047",
            "2048",
            "2049",
            "2050",
            "2051",
            "2052",
            "2053",
            "2054",
            "2055",
            "2056",
            "2057",
            "2058",
            "2059",
            "2060",
            "2061",
            "2062",
            "2063",
            "2064",
            "2065",
            "2066",
            "2067",
            "2068",
            "2069",
            "2070",
            "2071",
            "2072",
            "2073",
            "2074",
            "2075",
            "2076",
            "2077",
            "2078",
            "2079",
            "2080",
            "2081",
            "2082",
            "2083",
            "2084",
            "2085",
            "2086",
            "2087",
            "2088",
            "2089",
            "2090",
            "2091",
            "2092",
            "2093",
            "2094",
            "2095",
            "2096",
            "2097",
            "2098",
            "2099",
            "2100"
        ]
    },
    "series": [
        {
            "label": "V1",
            "type": "float",
            "raw":
                [
                    5.77,
                    5.77,
                    5.78,
                    5.78,
                    5.78,
                    5.77,
                    5.77,
                    5.77,
                    5.76,
                    5.75,
                    5.76,
                    5.76,
                    5.76,
                    5.76,
                    5.76,
                    5.76,
                    5.76,
                    5.76,
                    5.76,
                    5.76,
                    5.75,
                    5.76,
                    5.76,
                    5.75,
                    5.74,
                    5.76,
                    5.75,
                    5.74,
                    5.74,
                    5.73,
                    5.72,
                    5.72,
                    5.72,
                    5.73,
                    5.73,
                    5.72,
                    5.73,
                    5.73,
                    5.72,
                    5.73,
                    5.73,
                    5.73,
                    5.73,
                    5.73,
                    5.73,
                    5.72,
                    5.72,
                    5.71,
                    5.72,
                    5.73,
                    5.73,
                    5.72,
                    5.71,
                    5.7,
                    5.69,
                    5.67,
                    5.68,
                    5.69,
                    5.69,
                    5.71,
                    5.7,
                    5.71,
                    5.71,
                    5.71,
                    5.71,
                    5.7,
                    5.69,
                    5.68,
                    5.66,
                    5.67,
                    5.67,
                    5.66,
                    5.66,
                    5.68,
                    5.68,
                    5.69,
                    5.68,
                    5.66,
                    5.63,
                    5.65,
                    5.63,
                    5.63,
                    5.64,
                    5.63,
                    5.62,
                    5.61,
                    5.58,
                    5.59,
                    5.6,
                    5.59,
                    5.56,
                    5.59,
                    5.54,
                    5.57,
                    5.56,
                    5.56,
                    5.56,
                    5.56,
                    5.53,
                    5.52,
                    5.52,
                    5.51,
                    5.5,
                    5.48,
                    5.45,
                    5.41,
                    5.41,
                    5.43,
                    5.43,
                    5.4,
                    5.38,
                    5.37,
                    5.34,
                    5.31,
                    5.29,
                    5.07,
                    5.11,
                    5.06,
                    5.07,
                    4.97,
                    5.22,
                    5.13,
                    5.17,
                    5.19,
                    5.17,
                    5.16,
                    5.14,
                    5.1,
                    5.09,
                    5.04,
                    5.02,
                    4.97,
                    4.93,
                    4.83,
                    4.82,
                    4.83,
                    4.83,
                    4.85,
                    4.83,
                    4.82,
                    4.8,
                    4.79,
                    4.71,
                    4.66,
                    4.63,
                    4.58,
                    4.77,
                    4.89,
                    4.79,
                    4.95,
                    4.84,
                    4.85,
                    5.03,
                    4.97,
                    5.06,
                    5.07,
                    5,
                    5.15,
                    5,
                    4.74,
                    4.68,
                    4.53,
                    5.1,
                    5.4,
                    5.1,
                    5.05,
                    5.07,
                    4.83,
                    5.06,
                    4.88,
                    4.88,
                    4.76,
                    4.61,
                    4.47,
                    4.35,
                    4.18,
                    4.07,
                    3.94,
                    3.88,
                    3.86,
                    3.72,
                    3.77,
                    3.72,
                    3.69,
                    3.66,
                    3.64,
                    3.6,
                    3.55,
                    3.49,
                    3.42,
                    3.34,
                    3.25,
                    3.16,
                    3.08,
                    3.01,
                    2.95,
                    2.89,
                    2.84,
                    2.81,
                    2.77,
                    2.75,
                    2.72,
                    2.7,
                    2.68,
                    2.66,
                    2.65,
                    2.63,
                    2.62,
                    2.6,
                    2.58,
                    2.56,
                    2.54,
                    2.53,
                    2.52,
                    2.5,
                    2.49,
                    2.47,
                    2.46,
                    2.44,
                    2.43,
                    2.42,
                    2.41,
                    2.39,
                    2.38,
                    2.37,
                    2.36,
                    2.35,
                    2.34,
                    2.33,
                    2.32,
                    2.31,
                    2.3,
                    2.29,
                    2.28,
                    2.27,
                    2.26,
                    2.25,
                    2.24,
                    2.23,
                    2.23,
                    2.22,
                    2.21,
                    2.2,
                    2.2,
                    2.19,
                    2.18,
                    2.17,
                    2.17,
                    2.16,
                    2.15,
                    2.15,
                    2.14,
                    2.13,
                    2.13,
                    2.12,
                    2.11,
                    2.11,
                    2.1,
                    2.1,
                    2.09,
                    2.09,
                    2.08,
                    2.08,
                    2.07,
                    2.07,
                    2.06,
                    2.06,
                    2.05,
                    2.05,
                    2.04,
                    2.04,
                    2.04,
                    2.03,
                    2.03,
                    2.02,
                    2.02,
                    2.01,
                    2.01,
                    2.01,
                    2,
                    2,
                    2,
                    1.99,
                    1.99,
                    1.98,
                    1.98,
                    1.98,
                    1.97,
                    1.97,
                    1.97,
                    1.96,
                    1.96,
                    1.96,
                    1.95,
                    1.95,
                    1.95,
                    1.94,
                    1.94,
                    1.94,
                    1.93,
                    1.93
                ]
        }
    ]
}'

children_per_woman
parsed_data <- fromJSON(children_per_woman)

cat("Name:", parsed_data$name, "\n")
cat("Longname:", parsed_data$longname, "\n")
cat("Number of Observations:", parsed_data$n_obs, "\n")
cat("Number of Dimensions:", parsed_data$n_dim, "\n")

cat("Time Format:", parsed_data$time$format, "\n")
cat("First 5 Time Indices:", head(parsed_data$time$index), "\n")
cat("First 5 Time Values:", head(parsed_data$time$raw), "\n")
cat("Series Label:", parsed_data$series$label, "\n")
cat("First 5 Series Values:", head(parsed_data$series$raw[[1]], 5), "\n")

# Time series data
time_raw <- as.numeric(parsed_data$time$raw)
series_raw <- as.numeric(parsed_data$series$raw[[1]])

# Create a data frame
df <- data.frame(
  index = 1:length(time_raw),
  time = as.integer(time_raw),
  value = series_raw
)

plot(df$index, df$value, type = "l", xlab = "Index", ylab = "Value", main = "Time Series")

#ORTALAMADA DEGISIM NOKTASI ARAYAN ALGORITMALAR
#binary segmentation
tsdata <- ts(df$value, start = min(df$index), end = max(df$index), frequency = 1)

# Zaman serisini gC6ster
print(tsdata)

###children_per_woman annotations
###6->146,177
###8->146
###12->145,168
###13->146,170,180,197
###14->146,170

###Amoc
d.m_amoc <- cpt.mean(tsdata, method = "AMOC")
plot(d.m_amoc)
abline(v = cpts(d.m_amoc), col = "red", lty = 2,lwd=2)
cpts(d.m_amoc)

# oracle settings
o.m_amoc <- cpt.mean(tsdata, penalty = "BIC", method = "AMOC")
plot(o.m_amoc)
abline(v = cpts(o.m_amoc), col = "red", lty = 2,lwd=2)
cpts(o.m_amoc)

###BinSeg_1
x<-cpt.mean(tsdata,method = "BinSeg",Q=3)
plot(x)
abline(v = cpts(x), col = "red", lty = 2,lwd=2)
cpts(x)

m_binseg <- cpt.mean(tsdata, penalty = "BIC", method = "BinSeg", Q = 2)
plot(m_binseg)
abline(v = cpts(m_binseg), col = "red", lty = 2,lwd=2)
cpts(m_binseg)

###BinSeg_2 en iyi bu

m_binseg2 <- cpt.mean(tsdata, penalty = "BIC", method = "BinSeg", Q = 4)
plot(m_binseg2)
abline(v = cpts(m_binseg2), col = "red", lty = 2,lwd=2)
cpts(m_binseg2)


#Pelt_1
m_pelt <- cpt.mean(tsdata, method = "PELT",minseglen = 10)
plot(m_pelt, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(m_pelt), col = "blue", lty = 2,lwd=2)
cpts(m_pelt) 

#Pelt_2
m_pelt_more <- cpt.mean(tsdata, penalty = "Manual", pen.value = 2, method = "PELT", minseglen = 5)
plot(m_pelt_more, cpt.col = "green", xlab = "Index")
abline(v = cpts(m_pelt_more), col = "red", lty = 2, lwd = 2)
print(cpts(m_pelt_more))

#Pelt_3 bu en iyisi
m_pelt <- cpt.mean(tsdata, penalty = "BIC", method = "PELT")
plot(m_pelt, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(m_pelt), col = "blue", lty = 2,lwd=2)
cpts(m_pelt)   

#VARYANSTA DEG?S?M NOKTALARI ARAYAN ALGOR?TMA
v_pelt <- cpt.var(tsdata, method = "PELT")
plot(v_pelt, type = "l", cpt.col = "blue", xlab = "Index")
cpts(v_pelt)  

#HEM ORTALAMADA HEM DE VARYANSTA DEG?S?M NOKTASI ARAMA
mv_pelt <- cpt.meanvar(tsdata, method = "PELT")
mv_pelt 
plot(mv_pelt)
cpts(mv_pelt)   

###Segmented regression
#default
attach(df)
lm1<-lm(value~time)

seg_get<-segmented(lm1,seg.Z = ~ time,npsi = 3)
seg_get
seg_get$psi[,2]
cp <- round(seg_get$psi[,2])
cp_closest <- sapply(cp, function(bp) {
  d<- abs( time - bp)
  indd <- which.min(d)
  e <- c(time[indd])
  c(index_pos=indd, closest_time=e)
})
cp_closest
detach(df)

#oracle 1
attach(df)
lm2<-lm(value~time)

seg_get2<-segmented(lm2,seg.Z = ~ time, npsi=4)
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
detach(df)

###PROPHET
#default
df_prophet <- data.frame(
  ds = as.Date(time_raw), 
  y = as.numeric(series_raw) 
)

model <- prophet(df_prophet,n.changepoints = 5)

changepoints <- model$changepoints
changepoints <- as.numeric(as.Date(changepoints))
print(changepoints)

indeksler <- which(time_raw %in% changepoints)
print(indeksler)

# Time series grafiDini C'izme model

changepoints_df <- data.frame(ds = as.Date(changepoints), y = max(df_prophet$y))

p <- ggplot(df_prophet, aes(x = ds, y = y)) +
  geom_line() +
  geom_vline(data = changepoints_df, aes(xintercept = as.numeric(ds)), color = "red", linetype = "dashed") +
  labs(title = "Time Series with Changepoints",
       x = "Date",
       y = "Value") +
  theme_minimal()

print(p)

## oracle 1

model2 <- prophet(df_prophet, n.changepoints = 4)
changepoints2 <- model2$changepoints
changepoints2 <- as.numeric(as.Date(changepoints2))
print(changepoints2)

indeksler2 <- which(time_raw %in% changepoints2)
print(indeksler2)

# Time series grafiDini C'izme model2

changepoints_df2 <- data.frame(ds = as.Date(changepoints2), y = max(df_prophet$y))

p2 <- ggplot(df_prophet, aes(x = ds, y = y)) +
  geom_line() +
  geom_vline(data = changepoints_df2, aes(xintercept = as.numeric(ds)), color = "red", linetype = "dashed") +
  labs(title = "Time Series with Changepoints",
       x = "Date",
       y = "Value") +
  theme_minimal()

print(p2)

## oracle 2 en iyisi bu
model3 <- prophet(df_prophet,n.changepoints = 10)
changepoints3 <- model3$changepoints
changepoints3 <- as.numeric(as.Date(changepoints3))
print(changepoints3)

indeksler3 <- which(time_raw %in% changepoints3)
print(indeksler3)

# Time series grafiDini C'izme model3

changepoints_df3 <- data.frame(ds = as.Date(changepoints3), y = max(df_prophet$y))

p3 <- ggplot(df_prophet, aes(x = ds, y = y)) +
  geom_line() +
  geom_vline(data = changepoints_df3, aes(xintercept = as.numeric(ds)), color = "red", linetype = "dashed") +
  labs(title = "Time Series with Changepoints",
       x = "Date",
       y = "Value") +
  theme_minimal()

print(p3)

                            #### CO2 CANADA DATA ####

Dosya1 <- "co2_canada.json"
parsed_data <- fromJSON(Dosya1)

cat("Name:", parsed_data$name, "\n")
cat("Longname:", parsed_data$longname, "\n")
cat("Number of Observations:", parsed_data$n_obs, "\n")
cat("Number of Dimensions:", parsed_data$n_dim, "\n")

cat("Time Format:", parsed_data$time$format, "\n")
cat("First 5 Time Indices:", head(parsed_data$time$index), "\n")
cat("First 5 Time Values:", head(parsed_data$time$raw), "\n")
cat("Series Label:", parsed_data$series$label, "\n")
cat("First 5 Series Values:", head(parsed_data$series$raw[[1]], 5), "\n")


# ggplot2 kC<tC<phanesini yC<kle
library(ggplot2)

# Time series data
time_raw <- as.numeric(parsed_data$time$raw)
series_raw <- as.numeric(parsed_data$series$raw[[1]])

# Create a data frame
df <- data.frame(
  index = 1:length(time_raw),
  time = as.integer(time_raw),
  value = series_raw
)

plot(df$index, df$value, type = "l", xlab = "Index", ylab = "Value", main = "Time Series")



#ORTALAMADA DEGISIM NOKTASI ARAYAN ALGORITMALAR

#Zaman serisi oluEturma
tsdata <- ts(df$value, start = min(df$index), end = max(df$index), frequency = 1)

# Zaman serisini gC6ster
print(tsdata)


###CO2 CANADA annotator 
#co2 canada 
#6<- 80,107
#7<- 80,107,133,149,164,173
#8<-80,164
#9<-80,110,145,164,173
#13<-67,80,107,134,144,163,173





#####AMOC######
d.m_amoc <- cpt.mean(tsdata, method = "AMOC")
plot(d.m_amoc)
abline(v = cpts(d.m_amoc), col = "red", lty = 2,lwd=2)
cpts(d.m_amoc)

# oracle settings
o.m_amoc <- cpt.mean(tsdata, penalty = "BIC", method = "AMOC")
plot(o.m_amoc)
abline(v = cpts(o.m_amoc), col = "red", lty = 2,lwd=2)
cpts(o.m_amoc)

###BINSEG######
#default
x<-cpt.mean(tsdata,method = "BinSeg")
cpts(x)

#oracle
m_binseg <- cpt.mean(tsdata, penalty = "BIC", method = "BinSeg", Q = 5)
plot(m_binseg)
abline(v = cpts(m_binseg), col = "red", lty = 2,lwd=2)
cpts(m_binseg)


###Q deDiEim noktasD1 sayD1sD1nD1 ayarlar
###BinSeg_2
#5 tane 
m_binseg1 <- cpt.mean(tsdata, penalty = "BIC", method = "BinSeg", Q = 10)
plot(m_binseg1)
abline(v = cpts(m_binseg1), col = "red", lty = 2,lwd=2)
cpts(m_binseg1)

###BinSeg_3 **5

m_binseg2 <- cpt.mean(tsdata, penalty = "BIC", method = "BinSeg", Q = 25)
plot(m_binseg2)
abline(v = cpts(m_binseg2), col = "red", lty = 2,lwd=2)
cpts(m_binseg2)

###BinSeg_3 *5

m_binseg3 <- cpt.mean(tsdata, penalty = "BIC", method = "BinSeg", Q = 30)
plot(m_binseg3)
abline(v = cpts(m_binseg3), col = "red", lty = 2,lwd=2)
cpts(m_binseg3)

###BinSeg_4 en iyisi bu
m_binseg4 <- cpt.mean(tsdata, penalty = "AIC", method = "BinSeg", Q = 10)
plot(m_binseg4)
abline(v = cpts(m_binseg4), col = "red", lty = 2,lwd=2)
cpts(m_binseg4)

###BinSeg_5 
m_binseg5 <- cpt.mean(tsdata, penalty = "Manual", method = "BinSeg", Q = 30)
plot(m_binseg5)
abline(v = cpts(m_binseg5), col = "red", lty = 2,lwd=2)
cpts(m_binseg5)

#PELT########
###default **
m_pelt <- cpt.mean(tsdata, method = "PELT")
plot(m_pelt, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(m_pelt), col = "blue", lty = 2,lwd=2)
cpts(m_pelt) 

###oracle
m_pelt <- cpt.mean(tsdata, penalty = "MBIC", method = "PELT", minseglen = 20)
plot(m_pelt, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(m_pelt), col = "blue", lty = 2,lwd=2)
cpts(m_pelt) 

###PELT_3
m_pelt1 <- cpt.mean(tsdata, penalty = "MBIC", method = "PELT", minseglen = 20, pen.value = 5)
plot(m_pelt1, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(m_pelt1), col = "blue", lty = 2,lwd=2)
cpts(m_pelt1) 

###PELT_3
m_pelt2 <- cpt.mean(tsdata, penalty = "MBIC", method = "PELT", minseglen = 20, pen.value = 1)
plot(m_pelt2, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(m_pelt2), col = "blue", lty = 2,lwd=2)
cpts(m_pelt2)

###PELT_4 en iyisi buu
m_pelt3 <- cpt.mean(tsdata, penalty = "SIC", method = "PELT", minseglen = 20, pen.value = 1)
plot(m_pelt3, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(m_pelt3), col = "blue", lty = 2,lwd=2)
cpts(m_pelt3)

###PELT_5
m_pelt4 <- cpt.mean(tsdata, penalty = "AIC", method = "PELT", minseglen = 20, pen.value = 1)
plot(m_pelt4, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(m_pelt4), col = "blue", lty = 2,lwd=2)
cpts(m_pelt4)

###PELT_6
m_pelt5 <- cpt.mean(tsdata, penalty = "BIC", method = "PELT", minseglen = 20, pen.value = 1)
plot(m_pelt5, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(m_pelt5), col = "blue", lty = 2,lwd=2)
cpts(m_pelt5)

###PELT_7
m_pelt6 <- cpt.mean(tsdata, penalty = "Hannan-Quinn", method = "PELT", minseglen = 20, pen.value = 1)
plot(m_pelt6, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(m_pelt6), col = "blue", lty = 2,lwd=2)
cpts(m_pelt6)  

#VARYANSTA DEGISIM NOKTALARI ARAYAN ALGORITMA
v_pelt <- cpt.var(tsdata, method = "PELT")
plot(v_pelt, type = "l", cpt.col = "blue", xlab = "Index")
cpts(v_pelt)  

#HEM ORTALAMADA HEM DE VARYANSTA DEGISIM NOKTASI ARAMA
mv_pelt <- cpt.meanvar(tsdata, method = "PELT")
mv_pelt 
plot(mv_pelt)
cpts(mv_pelt)   

###SEGMENTED REGRESYON#######
install.packages("segmented")
library(segmented)

#default
attach(df)
lm1<-lm(value~time)

seg_get<-segmented(lm1,seg.Z = ~ time)
seg_get
seg_get$psi[,2]
cp <- round(seg_get$psi[,2])
cp_closest <- sapply(cp, function(bp) {
  d<- abs( time - bp)
  indd <- which.min(d)
  e <- c(time[indd])
  c(index_pos=indd, closest_time=e)
})
cp_closest
detach(df)


#oracle
attach(df)
lm2<-lm(value~time)

seg_get2<-segmented(lm2,seg.Z = ~ time, npsi=6)
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
detach(df)


######PROPHET####
df_prophet <- data.frame(
  ds = as.Date(time_raw), # Tarih sC<tunu
  y = as.numeric(series_raw) # DeDer sC<tunu
)

model <- prophet(df_prophet)

changepoints <- model$changepoints
changepoints <- as.numeric(as.Date(changepoints))
print(changepoints)

indeksler <- which(time_raw %in% changepoints)
print(indeksler)



# Time series grafiDini C'izme model 
changepoints_df <- data.frame(ds = as.Date(changepoints), y = max(df_prophet$y))

p <- ggplot(df_prophet, aes(x = ds, y = y)) +
  geom_line() +
  geom_vline(data = changepoints_df, aes(xintercept = as.numeric(ds)), color = "red", linetype = "dashed") +
  labs(title = "Time Series with Changepoints",
       x = "Date",
       y = "Value") +
  theme_minimal()

print(p)

##

model2 <- prophet(df_prophet, n.changepoints = 2)
changepoints2 <- model2$changepoints
changepoints2 <- as.numeric(as.Date(changepoints2))
print(changepoints2)

indeksler2 <- which(time_raw %in% changepoints2)
print(indeksler2)




# Time series grafiDini C'izme model2

changepoints_df2 <- data.frame(ds = as.Date(changepoints2), y = max(df_prophet$y))

p2 <- ggplot(df_prophet, aes(x = ds, y = y)) +
  geom_line() +
  geom_vline(data = changepoints_df2, aes(xintercept = as.numeric(ds)), color = "red", linetype = "dashed") +
  labs(title = "Time Series with Changepoints",
       x = "Date",
       y = "Value") +
  theme_minimal()

print(p2)

##

model3 <- prophet(df_prophet,n.changepoints = 9)
changepoints3 <- model3$changepoints
changepoints3 <- as.numeric(as.Date(changepoints3))
print(changepoints3)

indeksler3 <- which(time_raw %in% changepoints3)
print(indeksler3)




# Time series grafiDini C'izme model3

changepoints_df3 <- data.frame(ds = as.Date(changepoints3), y = max(df_prophet$y))

p3 <- ggplot(df_prophet, aes(x = ds, y = y)) +
  geom_line() +
  geom_vline(data = changepoints_df3, aes(xintercept = as.numeric(ds)), color = "red", linetype = "dashed") +
  labs(title = "Time Series with Changepoints",
       x = "Date",
       y = "Value") +
  theme_minimal()

print(p3)

## en iyi bu

model4 <- prophet(df_prophet,n.changepoints = 11)
changepoints4 <- model4$changepoints
changepoints4 <- as.numeric(as.Date(changepoints4))
print(changepoints4)

indeksler4 <- which(time_raw %in% changepoints4)
print(indeksler4)



# Time series grafiDini C'izme model3

changepoints_df4 <- data.frame(ds = as.Date(changepoints4), y = max(df_prophet$y))

p4 <- ggplot(df_prophet, aes(x = ds, y = y)) +
  geom_line() +
  geom_vline(data = changepoints_df4, aes(xintercept = as.numeric(ds)), color = "red", linetype = "dashed") +
  labs(title = "Time Series with Changepoints",
       x = "Date",
       y = "Value") +
  theme_minimal()

print(p4)

#

model5 <- prophet(df_prophet,n.changepoints = 17)
changepoints5 <- model5$changepoints
changepoints5 <- as.numeric(as.Date(changepoints5))
print(changepoints5)

indeksler5 <- which(time_raw %in% changepoints5)
print(indeksler5)


# Time series grafiDini C'izme model3

changepoints_df5 <- data.frame(ds = as.Date(changepoints5), y = max(df_prophet$y))

p5 <- ggplot(df_prophet, aes(x = ds, y = y)) +
  geom_line() +
  geom_vline(data = changepoints_df5, aes(xintercept = as.numeric(ds)), color = "red", linetype = "dashed") +
  labs(title = "Time Series with Changepoints",
       x = "Date",
       y = "Value") +
  theme_minimal()

print(p5)

                               #### DEBT IRELAND DATA ####

debt_ireland<-'{
  "name": "debt_ireland",
  "longname": "Debt Ireland",
  "n_obs": 21,
  "n_dim": 1,
  "time": {
    "type": "string",
    "format": "%Y",
    "index": [
      0,
      1,
      2,
      3,
      4,
      5,
      6,
      7,
      8,
      9,
      10,
      11,
      12,
      13,
      14,
      15,
      16,
      17,
      18,
      19,
      20
    ],
    "raw": [
      "2000",
      "2001",
      "2002",
      "2003",
      "2004",
      "2005",
      "2006",
      "2007",
      "2008",
      "2009",
      "2010",
      "2011",
      "2012",
      "2013",
      "2014",
      "2015",
      "2016",
      "2017",
      "2018",
      "2019",
      "2020"
    ]
  },
  "series": [
    {
      "label": "V1",
      "type": "float",
      "raw": [
        36.0732199,
        33.2394627,
        30.5521068,
        29.9296861,
        28.2148891,
        26.0766114,
        23.618314,
        23.9083721,
        42.4036869,
        61.5433048,
        85.9938449,
        110.861647,
        119.8646655,
        119.6837014,
        104.1283774,
        76.8191392,
        73.4443135,
        68.4403541,
        63.8514846,
        61.1353388,
        56.0118623
      ]
    }
  ]
}'


debt_ireland
parsed_data <- fromJSON(debt_ireland)

cat("Name:", parsed_data$name, "\n")
cat("Longname:", parsed_data$longname, "\n")
cat("Number of Observations:", parsed_data$n_obs, "\n")
cat("Number of Dimensions:", parsed_data$n_dim, "\n")

cat("Time Format:", parsed_data$time$format, "\n")
cat("First 5 Time Indices:", head(parsed_data$time$index), "\n")
cat("First 5 Time Values:", head(parsed_data$time$raw), "\n")
cat("Series Label:", parsed_data$series$label, "\n")
cat("First 5 Series Values:", head(parsed_data$series$raw[[1]], 5), "\n")

# Time series data
time_raw <- as.numeric(parsed_data$time$raw)
series_raw <- as.numeric(parsed_data$series$raw[[1]])

# Create a data frame
df <- data.frame(
  index = 1:length(time_raw),
  time = as.integer(time_raw),
  value = series_raw
)

plot(df$index, df$value, type = "l", xlab = "Index", ylab = "Value", main = "Time Series")

#ORTALAMADA DEGISIM NOKTASI ARAYAN ALGORITMALAR
#binary segmentation
tsdata <- ts(df$value, start = min(df$index), end = max(df$index), frequency = 1)

# Zaman serisini gC6ster
print(tsdata)

###debt_ireland annotations
###6->8,15
###7->7,14
###8->8,15
###9->8,12,14,16
###12->7,15

###Amoc
d.m_amoc <- cpt.mean(tsdata, method = "AMOC")
plot(d.m_amoc)
abline(v = cpts(d.m_amoc), col = "red", lty = 2,lwd=2)
cpts(d.m_amoc)

# oracle settings
o.m_amoc <- cpt.mean(tsdata, penalty = "BIC" , method = "AMOC")
plot(o.m_amoc)
abline(v = cpts(o.m_amoc), col = "red", lty = 2,lwd=2)
cpts(o.m_amoc)

###BinSeg EN iyi olarak bi buna bak
x<-cpt.mean(tsdata,method = "BinSeg",Q=3)
plot(x)
abline(v = cpts(x), col = "red", lty = 2,lwd=2)
cpts(x)

m_binseg <- cpt.mean(tsdata, penalty = "BIC", method = "BinSeg", Q = 2)
plot(m_binseg)
abline(v = cpts(m_binseg), col = "red", lty = 2,lwd=2)
cpts(m_binseg)

###Binseg_1 EN iyi olarak bi de buna bak

m_binseg1 <- cpt.mean(tsdata, penalty = "BIC", method = "BinSeg", Q = 4)
plot(m_binseg1)
abline(v = cpts(m_binseg1), col = "red", lty = 2,lwd=2)
cpts(m_binseg1)

#Pelt_1
m_pelt <- cpt.mean(tsdata,  method = "PELT",minseglen = 10)
plot(m_pelt, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(m_pelt), col = "blue", lty = 2,lwd=2)
cpts(m_pelt) 

###MD0NSEGLEN minimum segment uzunluDunu belirtir,mesela 2 olarak belirlenince algoritma iki
###ardD1ED1k deDiEim noktasD1 arasD1ndaki segment uzunluDunun en az 2 olmasD1nD1 saDlar. 

#Pelt_2 peltelerin hepsini deneeeee
m_pelt2 <- cpt.mean(tsdata, penalty = "MBIC", method = "PELT", minseglen = 1)
plot(m_pelt2, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(m_pelt2), col = "blue", lty = 2,lwd=2)
cpts(m_pelt2) 

#Pelt_3
m_pelt3 <- cpt.mean(tsdata, penalty = "MBIC", method = "PELT", minseglen = 3)
plot(m_pelt3, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(m_pelt3), col = "blue", lty = 2,lwd=2)
cpts(m_pelt3) 

#Pelt_4
m_pelt4 <- cpt.mean(tsdata, penalty = "MBIC", method = "PELT", minseglen = 2, pen.value = 5)
plot(m_pelt4, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(m_pelt4), col = "blue", lty = 2,lwd=2)
cpts(m_pelt4) 

#Pelt_5
m_pelt5 <- cpt.mean(tsdata, penalty = "MBIC", method = "PELT", pen.value = 25,minseglen = 5)
plot(m_pelt5, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(m_pelt5), col = "blue", lty = 2,lwd=2)
cpts(m_pelt5) 

#VARYANSTA DEGISIM NOKTALARI ARAYAN ALGOR?TMA
v_pelt <- cpt.var(tsdata, method = "PELT")
plot(v_pelt, type = "l", cpt.col = "blue", xlab = "Index")
cpts(v_pelt)  

#HEM ORTALAMADA HEM DE VARYANSTA DEGISIM NOKTASI ARAMA
mv_pelt <- cpt.meanvar(tsdata, method = "PELT")
mv_pelt 
plot(mv_pelt)
cpts(mv_pelt)

###Segmented regression
#default
attach(df)
lm1<-lm(value~time)
seg_get<-segmented(lm1,seg.Z = ~ time)
seg_get
seg_get$psi[,2]
cp <- round(seg_get$psi[,2])
cp_closest <- sapply(cp, function(bp) {
  d<- abs( time - bp)
  indd <- which.min(d)
  e <- c(time[indd])
  c(index_pos=indd, closest_time=e)
})
cp_closest
detach(df)

#oracle
attach(df)
lm2<-lm(value~time)
seg_get2<-segmented(lm2,seg.Z = ~ time, npsi=2)
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
detach(df)

##

attach(df)
lm3<-lm(value~time)

seg_get3<-segmented(lm3,seg.Z = ~ time, npsi=3)
seg_get3
seg_get3$psi[,2]
cp <- round(seg_get3$psi[,2])
cp_closest3 <- sapply(cp, function(bp) {
  d<- abs( time - bp)
  indd <- which.min(d)
  e <- c(time[indd])
  c(index_pos=indd, closest_time=e)
})
cp_closest3
detach(df)


## PROPHET 

df_prophet <- data.frame(
  ds = as.Date(time_raw), 
  y = as.numeric(series_raw) 
)

model <- prophet(df_prophet,n.changepoints = 2)

changepoints <- model$changepoints
changepoints <- as.numeric(as.Date(changepoints))
print(changepoints)

indeksler <- which(time_raw %in% changepoints)
print(indeksler)

# Time series grafiDini C'izme model

changepoints_df <- data.frame(ds = as.Date(changepoints), y = max(df_prophet$y))

p <- ggplot(df_prophet, aes(x = ds, y = y)) +
  geom_line() +
  geom_vline(data = changepoints_df, aes(xintercept = as.numeric(ds)), color = "red", linetype = "dashed") +
  labs(title = "Time Series with Changepoints",
       x = "Date",
       y = "Value") +
  theme_minimal()

print(p)

## bunlarD1n hepsini dene

model2 <- prophet(df_prophet, n.changepoints = 4)
changepoints2 <- model2$changepoints
changepoints2 <- as.numeric(as.Date(changepoints2))
print(changepoints2)

indeksler2 <- which(time_raw %in% changepoints2)
print(indeksler2)

# Time series grafiDini C'izme model2

changepoints_df2 <- data.frame(ds = as.Date(changepoints2), y = max(df_prophet$y))

p2 <- ggplot(df_prophet, aes(x = ds, y = y)) +
  geom_line() +
  geom_vline(data = changepoints_df2, aes(xintercept = as.numeric(ds)), color = "red", linetype = "dashed") +
  labs(title = "Time Series with Changepoints",
       x = "Date",
       y = "Value") +
  theme_minimal()

print(p2)

##
model3 <- prophet(df_prophet,n.changepoints = 3)
changepoints3 <- model3$changepoints
changepoints3 <- as.numeric(as.Date(changepoints3))
print(changepoints3)

indeksler3 <- which(time_raw %in% changepoints3)
print(indeksler3)

# Time series grafiDini C'izme model3

changepoints_df3 <- data.frame(ds = as.Date(changepoints3), y = max(df_prophet$y))

p3 <- ggplot(df_prophet, aes(x = ds, y = y)) +
  geom_line() +
  geom_vline(data = changepoints_df3, aes(xintercept = as.numeric(ds)), color = "red", linetype = "dashed") +
  labs(title = "Time Series with Changepoints",
       x = "Date",
       y = "Value") +
  theme_minimal()

print(p3)

                              #### RAIL LINES DATA ####

rail_lines<- '{
  "name": "rail_lines",
  "longname": "Rail Lines",
  "n_obs": 37,
  "n_dim": 1,
  "time": {
    "format": "%Y",
    "index": [
      0,
      1,
      2,
      3,
      4,
      5,
      6,
      7,
      8,
      9,
      10,
      11,
      12,
      13,
      14,
      15,
      16,
      17,
      18,
      19,
      20,
      21,
      22,
      23,
      24,
      25,
      26,
      27,
      28,
      29,
      30,
      31,
      32,
      33,
      34,
      35,
      36
    ],
    "raw": [
      "1980",
      "1981",
      "1982",
      "1983",
      "1984",
      "1985",
      "1986",
      "1987",
      "1988",
      "1989",
      "1990",
      "1991",
      "1992",
      "1993",
      "1994",
      "1995",
      "1996",
      "1997",
      "1998",
      "1999",
      "2000",
      "2001",
      "2002",
      "2003",
      "2004",
      "2005",
      "2006",
      "2007",
      "2008",
      "2009",
      "2010",
      "2011",
      "2012",
      "2013",
      "2014",
      "2015",
      "2016"
    ]
  },
  "series": [
    {
      "label": "V1",
      "type": "float",
      "raw": [
        1000507.33548387,
        996153.287096774,
        994910,
        992092.841935484,
        983302.464516129,
        975342.625806452,
        963878.138709677,
        949388.514516129,
        941808.890967742,
        938664.31516129,
        977074.383,
        973210.707096774,
        964581.342580645,
        973468.754193548,
        970988.050967742,
        968160.260322581,
        971143.774885996,
        970154.472375981,
        964146.915693211,
        963074.49690094,
        968935.935483871,
        956609.233390473,
        962635.639187575,
        959885.943535401,
        958123.943535401,
        989329.943535401,
        1062032.93548387,
        1060970.93548387,
        1060664.93548387,
        1056107.93548387,
        1076589.93548387,
        1057710.93548387,
        1051859.93548387,
        1051798.67548387,
        1055263.93548387,
        1051968.08548387,
        1051767.60548387
      ]
    }
  ]
}'
rail_lines
parsed_data <- fromJSON(rail_lines)

cat("Name:", parsed_data$name, "\n")
cat("Longname:", parsed_data$longname, "\n")
cat("Number of Observations:", parsed_data$n_obs, "\n")
cat("Number of Dimensions:", parsed_data$n_dim, "\n")

cat("Time Format:", parsed_data$time$format, "\n")
cat("First 5 Time Indices:", head(parsed_data$time$index), "\n")
cat("First 5 Time Values:", head(parsed_data$time$raw), "\n")
cat("Series Label:", parsed_data$series$label, "\n")
cat("First 5 Series Values:", head(parsed_data$series$raw[[1]], 5), "\n")



# ggplot2 kC<tC<phanesini yC<kle
library(ggplot2)
library(changepoint)
# Time series data
time_raw <- as.numeric(parsed_data$time$raw)
series_raw <- as.numeric(parsed_data$series$raw[[1]])

# Create a data frame
df <- data.frame(
  index = 1:length(time_raw),
  time = as.integer(time_raw),
  value = series_raw
)

plot(df$index, df$value, type = "l", xlab = "Index", ylab = "Value", main = "Time Series")


#anatotations
#"6": <- 10, 25
#"8": <- 10,26
#"9": <- 10,25
#"12": <-  25,26
#"13"   <- 25

#ORTALAMADA DEGISIM NOKTASI ARAYAN ALGORITMALAR

tsdata <- ts(df$value, start = min(df$index), end = max(df$index), frequency = 1)
print(tsdata)

###Amoc
d.m_amoc <- cpt.mean(tsdata, method = "AMOC")
plot(d.m_amoc)
abline(v = cpts(d.m_amoc), col = "red", lty = 2,lwd=2)
cpts(d.m_amoc)

###oracle settings
o.m_amoc <- cpt.mean(tsdata, penalty = "BIC", method = "AMOC")
plot(o.m_amoc)
abline(v = cpts(o.m_amoc), col = "red", lty = 2,lwd=2)
cpts(o.m_amoc)

###BinSeg_1
#default
x<-cpt.mean(tsdata,method = "BinSeg")
plot(x)
abline(v = cpts(x), col = "red", lty = 2,lwd=2)
cpts(x)

#oracle
m_binseg <- cpt.mean(tsdata, penalty = "BIC", method = "BinSeg", Q = 2)
plot(m_binseg)
abline(v = cpts(m_binseg), col = "red", lty = 2,lwd=2)
cpts(m_binseg)

# en iyisi bu oldu
m_binseg1 <- cpt.mean(tsdata, penalty = "BIC", method = "BinSeg", Q = 3)
plot(m_binseg1)
abline(v = cpts(m_binseg1), col = "red", lty = 2,lwd=2)
cpts(m_binseg1)

###PELT
#default
m_pelt <- cpt.mean(tsdata, method = "PELT")
plot(m_pelt, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(m_pelt), col = "blue", lty = 2,lwd=2)
cpts(m_pelt)

###oracle
m_pelt1 <- cpt.mean(tsdata, penalty = "MBIC", method = "PELT", minseglen = 5)
plot(m_pelt1, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(m_pelt1), col = "blue", lty = 2,lwd=2)
cpts(m_pelt1) 

###
m_pelt2 <- cpt.mean(tsdata, penalty = "MBIC", method = "PELT", minseglen = 10, pen.value = 5)
plot(m_pelt2, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(m_pelt2), col = "blue", lty = 2,lwd=2)
cpts(m_pelt2) 

### en iyisi bu oldu
m_pelt3 <- cpt.mean(tsdata, penalty = "MBIC", method = "PELT", minseglen = 7, pen.value = 1)
plot(m_pelt3, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(m_pelt3), col = "blue", lty = 2,lwd=2)
cpts(m_pelt3) 

#VARYANSTA DEGISIM NOKTALARI ARAYAN ALGORITMA
v_pelt <- cpt.var(tsdata, method = "PELT")
plot(v_pelt, type = "l", cpt.col = "blue", xlab = "Index")
cpts(v_pelt)  

#HEM ORTALAMADA HEM DE VARYANSTA DEGISIM NOKTASI ARAMA
mv_pelt <- cpt.meanvar(tsdata, method = "PELT")
mv_pelt 
plot(mv_pelt)
cpts(mv_pelt)

###Segmented regression
#default
attach(df)
lm1<-lm(value~time)

seg_get<-segmented(lm1,seg.Z = ~ time)
seg_get
seg_get$psi[,2]
cp <- round(seg_get$psi[,2])
cp_closest <- sapply(cp, function(bp) {
  d<- abs( time - bp)
  indd <- which.min(d)
  e <- c(time[indd])
  c(index_pos=indd, closest_time=e)
})
cp_closest
detach(df)

#oracle
attach(df)
lm2<-lm(value~time)

seg_get2<-segmented(lm2,seg.Z = ~ time, npsi=2)
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
detach(df)

## PROPHET ##
#default
df_prophet <- data.frame(
  ds = as.Date(time_raw),
  y = as.numeric(series_raw)
)

model <- prophet(df_prophet)

changepoints <- model$changepoints
changepoints <- as.numeric(as.Date(changepoints))
print(changepoints)

indeksler <- which(time_raw %in% changepoints)
print(indeksler)

# Time series grafiDini C'izme model

changepoints_df <- data.frame(ds = as.Date(changepoints), y = max(df_prophet$y))

p <- ggplot(df_prophet, aes(x = ds, y = y)) +
  geom_line() +
  geom_vline(data = changepoints_df, aes(xintercept = as.numeric(ds)), color = "red", linetype = "dashed") +
  labs(title = "Time Series with Changepoints",
       x = "Date",
       y = "Value") +
  theme_minimal()

print(p)

## oracle bu iyi dene 

model2 <- prophet(df_prophet, n.changepoints = 2)
changepoints2 <- model2$changepoints
changepoints2 <- as.numeric(as.Date(changepoints2))
print(changepoints2)

indeksler2 <- which(time_raw %in% changepoints2)
print(indeksler2)

# Time series grafiDini C'izme model2

changepoints_df2 <- data.frame(ds = as.Date(changepoints2), y = max(df_prophet$y))

p2 <- ggplot(df_prophet, aes(x = ds, y = y)) +
  geom_line() +
  geom_vline(data = changepoints_df2, aes(xintercept = as.numeric(ds)), color = "red", linetype = "dashed") +
  labs(title = "Time Series with Changepoints",
       x = "Date",
       y = "Value") +
  theme_minimal()

print(p2)

## bu da iyi dene

model3 <- prophet(df_prophet,n.changepoints = 3)
changepoints3 <- model3$changepoints
changepoints3 <- as.numeric(as.Date(changepoints3))
print(changepoints3)

indeksler3 <- which(time_raw %in% changepoints3)
print(indeksler3)

# Time series grafiDini C'izme model3

changepoints_df3 <- data.frame(ds = as.Date(changepoints3), y = max(df_prophet$y))

p3 <- ggplot(df_prophet, aes(x = ds, y = y)) +
  geom_line() +
  geom_vline(data = changepoints_df3, aes(xintercept = as.numeric(ds)), color = "red", linetype = "dashed") +
  labs(title = "Time Series with Changepoints",
       x = "Date",
       y = "Value") +
  theme_minimal()

print(p3)

                            #### RATHER STOCK DATA ####

ratner_stock<-'{
  "name": "ratner_stock",
  "longname": "Ratner Group Stock Price",
  "n_obs": 600,
  "n_dim": 1,
  "time": {
    "type": "string",
    "format": "%Y-%m-%d",
    "index": [
      0,
      1,
      2,
      3,
      4,
      5,
      6,
      7,
      8,
      9,
      10,
      11,
      12,
      13,
      14,
      15,
      16,
      17,
      18,
      19,
      20,
      21,
      22,
      23,
      24,
      25,
      26,
      27,
      28,
      29,
      30,
      31,
      32,
      33,
      34,
      35,
      36,
      37,
      38,
      39,
      40,
      41,
      42,
      43,
      44,
      45,
      46,
      47,
      48,
      49,
      50,
      51,
      52,
      53,
      54,
      55,
      56,
      57,
      58,
      59,
      60,
      61,
      62,
      63,
      64,
      65,
      66,
      67,
      68,
      69,
      70,
      71,
      72,
      73,
      74,
      75,
      76,
      77,
      78,
      79,
      80,
      81,
      82,
      83,
      84,
      85,
      86,
      87,
      88,
      89,
      90,
      91,
      92,
      93,
      94,
      95,
      96,
      97,
      98,
      99,
      100,
      101,
      102,
      103,
      104,
      105,
      106,
      107,
      108,
      109,
      110,
      111,
      112,
      113,
      114,
      115,
      116,
      117,
      118,
      119,
      120,
      121,
      122,
      123,
      124,
      125,
      126,
      127,
      128,
      129,
      130,
      131,
      132,
      133,
      134,
      135,
      136,
      137,
      138,
      139,
      140,
      141,
      142,
      143,
      144,
      145,
      146,
      147,
      148,
      149,
      150,
      151,
      152,
      153,
      154,
      155,
      156,
      157,
      158,
      159,
      160,
      161,
      162,
      163,
      164,
      165,
      166,
      167,
      168,
      169,
      170,
      171,
      172,
      173,
      174,
      175,
      176,
      177,
      178,
      179,
      180,
      181,
      182,
      183,
      184,
      185,
      186,
      187,
      188,
      189,
      190,
      191,
      192,
      193,
      194,
      195,
      196,
      197,
      198,
      199,
      200,
      201,
      202,
      203,
      204,
      205,
      206,
      207,
      208,
      209,
      210,
      211,
      212,
      213,
      214,
      215,
      216,
      217,
      218,
      219,
      220,
      221,
      222,
      223,
      224,
      225,
      226,
      227,
      228,
      229,
      230,
      231,
      232,
      233,
      234,
      235,
      236,
      237,
      238,
      239,
      240,
      241,
      242,
      243,
      244,
      245,
      246,
      247,
      248,
      249,
      250,
      251,
      252,
      253,
      254,
      255,
      256,
      257,
      258,
      259,
      260,
      261,
      262,
      263,
      264,
      265,
      266,
      267,
      268,
      269,
      270,
      271,
      272,
      273,
      274,
      275,
      276,
      277,
      278,
      279,
      280,
      281,
      282,
      283,
      284,
      285,
      286,
      287,
      288,
      289,
      290,
      291,
      292,
      293,
      294,
      295,
      296,
      297,
      298,
      299,
      300,
      301,
      302,
      303,
      304,
      305,
      306,
      307,
      308,
      309,
      310,
      311,
      312,
      313,
      314,
      315,
      316,
      317,
      318,
      319,
      320,
      321,
      322,
      323,
      324,
      325,
      326,
      327,
      328,
      329,
      330,
      331,
      332,
      333,
      334,
      335,
      336,
      337,
      338,
      339,
      340,
      341,
      342,
      343,
      344,
      345,
      346,
      347,
      348,
      349,
      350,
      351,
      352,
      353,
      354,
      355,
      356,
      357,
      358,
      359,
      360,
      361,
      362,
      363,
      364,
      365,
      366,
      367,
      368,
      369,
      370,
      371,
      372,
      373,
      374,
      375,
      376,
      377,
      378,
      379,
      380,
      381,
      382,
      383,
      384,
      385,
      386,
      387,
      388,
      389,
      390,
      391,
      392,
      393,
      394,
      395,
      396,
      397,
      398,
      399,
      400,
      401,
      402,
      403,
      404,
      405,
      406,
      407,
      408,
      409,
      410,
      411,
      412,
      413,
      414,
      415,
      416,
      417,
      418,
      419,
      420,
      421,
      422,
      423,
      424,
      425,
      426,
      427,
      428,
      429,
      430,
      431,
      432,
      433,
      434,
      435,
      436,
      437,
      438,
      439,
      440,
      441,
      442,
      443,
      444,
      445,
      446,
      447,
      448,
      449,
      450,
      451,
      452,
      453,
      454,
      455,
      456,
      457,
      458,
      459,
      460,
      461,
      462,
      463,
      464,
      465,
      466,
      467,
      468,
      469,
      470,
      471,
      472,
      473,
      474,
      475,
      476,
      477,
      478,
      479,
      480,
      481,
      482,
      483,
      484,
      485,
      486,
      487,
      488,
      489,
      490,
      491,
      492,
      493,
      494,
      495,
      496,
      497,
      498,
      499,
      500,
      501,
      502,
      503,
      504,
      505,
      506,
      507,
      508,
      509,
      510,
      511,
      512,
      513,
      514,
      515,
      516,
      517,
      518,
      519,
      520,
      521,
      522,
      523,
      524,
      525,
      526,
      527,
      528,
      529,
      530,
      531,
      532,
      533,
      534,
      535,
      536,
      537,
      538,
      539,
      540,
      541,
      542,
      543,
      544,
      545,
      546,
      547,
      548,
      549,
      550,
      551,
      552,
      553,
      554,
      555,
      556,
      557,
      558,
      559,
      560,
      561,
      562,
      563,
      564,
      565,
      566,
      567,
      568,
      569,
      570,
      571,
      572,
      573,
      574,
      575,
      576,
      577,
      578,
      579,
      580,
      581,
      582,
      583,
      584,
      585,
      586,
      587,
      588,
      589,
      590,
      591,
      592,
      593,
      594,
      595,
      596,
      597,
      598,
      599
    ],
    "raw": [
      "1988-07-14",
      "1988-07-19",
      "1988-07-22",
      "1988-07-27",
      "1988-08-01",
      "1988-08-04",
      "1988-08-09",
      "1988-08-12",
      "1988-08-17",
      "1988-08-22",
      "1988-08-25",
      "1988-08-30",
      "1988-09-02",
      "1988-09-08",
      "1988-09-13",
      "1988-09-16",
      "1988-09-21",
      "1988-09-26",
      "1988-09-29",
      "1988-10-04",
      "1988-10-07",
      "1988-10-12",
      "1988-10-17",
      "1988-10-20",
      "1988-10-25",
      "1988-10-28",
      "1988-11-02",
      "1988-11-07",
      "1988-11-10",
      "1988-11-15",
      "1988-11-18",
      "1988-11-23",
      "1988-11-29",
      "1988-12-02",
      "1988-12-07",
      "1988-12-12",
      "1988-12-15",
      "1988-12-20",
      "1988-12-23",
      "1988-12-29",
      "1989-01-04",
      "1989-01-09",
      "1989-01-12",
      "1989-01-17",
      "1989-01-20",
      "1989-01-25",
      "1989-01-30",
      "1989-02-02",
      "1989-02-07",
      "1989-02-10",
      "1989-02-15",
      "1989-02-21",
      "1989-02-24",
      "1989-03-01",
      "1989-03-06",
      "1989-03-09",
      "1989-03-14",
      "1989-03-17",
      "1989-03-22",
      "1989-03-28",
      "1989-03-31",
      "1989-04-05",
      "1989-04-10",
      "1989-04-13",
      "1989-04-18",
      "1989-04-21",
      "1989-04-26",
      "1989-05-01",
      "1989-05-04",
      "1989-05-09",
      "1989-05-12",
      "1989-05-17",
      "1989-05-22",
      "1989-05-25",
      "1989-05-31",
      "1989-06-05",
      "1989-06-08",
      "1989-06-13",
      "1989-06-16",
      "1989-06-21",
      "1989-06-26",
      "1989-06-29",
      "1989-07-05",
      "1989-07-10",
      "1989-07-13",
      "1989-07-18",
      "1989-07-21",
      "1989-07-26",
      "1989-07-31",
      "1989-08-03",
      "1989-08-08",
      "1989-08-11",
      "1989-08-16",
      "1989-08-21",
      "1989-08-24",
      "1989-08-29",
      "1989-09-01",
      "1989-09-07",
      "1989-09-12",
      "1989-09-15",
      "1989-09-20",
      "1989-09-25",
      "1989-09-28",
      "1989-10-03",
      "1989-10-06",
      "1989-10-11",
      "1989-10-16",
      "1989-10-19",
      "1989-10-24",
      "1989-10-27",
      "1989-11-01",
      "1989-11-06",
      "1989-11-09",
      "1989-11-14",
      "1989-11-17",
      "1989-11-22",
      "1989-11-28",
      "1989-12-01",
      "1989-12-06",
      "1989-12-11",
      "1989-12-14",
      "1989-12-19",
      "1989-12-22",
      "1989-12-28",
      "1990-01-03",
      "1990-01-08",
      "1990-01-11",
      "1990-01-16",
      "1990-01-19",
      "1990-01-24",
      "1990-01-29",
      "1990-02-01",
      "1990-02-06",
      "1990-02-09",
      "1990-02-14",
      "1990-02-20",
      "1990-02-23",
      "1990-02-28",
      "1990-03-05",
      "1990-03-08",
      "1990-03-13",
      "1990-03-16",
      "1990-03-21",
      "1990-03-26",
      "1990-03-29",
      "1990-04-03",
      "1990-04-06",
      "1990-04-11",
      "1990-04-17",
      "1990-04-20",
      "1990-04-25",
      "1990-04-30",
      "1990-05-03",
      "1990-05-08",
      "1990-05-11",
      "1990-05-16",
      "1990-05-21",
      "1990-05-24",
      "1990-05-30",
      "1990-06-04",
      "1990-06-07",
      "1990-06-12",
      "1990-06-15",
      "1990-06-20",
      "1990-06-25",
      "1990-06-28",
      "1990-07-03",
      "1990-07-09",
      "1990-07-12",
      "1990-07-17",
      "1990-07-20",
      "1990-07-25",
      "1990-07-30",
      "1990-08-02",
      "1990-08-07",
      "1990-08-10",
      "1990-08-15",
      "1990-08-20",
      "1990-08-23",
      "1990-08-28",
      "1990-08-31",
      "1990-09-06",
      "1990-09-11",
      "1990-09-14",
      "1990-09-19",
      "1990-09-24",
      "1990-09-27",
      "1990-10-02",
      "1990-10-05",
      "1990-10-10",
      "1990-10-15",
      "1990-10-18",
      "1990-10-23",
      "1990-10-26",
      "1990-10-31",
      "1990-11-05",
      "1990-11-08",
      "1990-11-13",
      "1990-11-16",
      "1990-11-21",
      "1990-11-27",
      "1990-11-30",
      "1990-12-05",
      "1990-12-10",
      "1990-12-13",
      "1990-12-18",
      "1990-12-21",
      "1990-12-27",
      "1991-01-02",
      "1991-01-07",
      "1991-01-10",
      "1991-01-15",
      "1991-01-18",
      "1991-01-23",
      "1991-01-28",
      "1991-01-31",
      "1991-02-05",
      "1991-02-08",
      "1991-02-13",
      "1991-02-19",
      "1991-02-22",
      "1991-02-27",
      "1991-03-04",
      "1991-03-07",
      "1991-03-12",
      "1991-03-15",
      "1991-03-20",
      "1991-03-25",
      "1991-03-28",
      "1991-04-03",
      "1991-04-08",
      "1991-04-11",
      "1991-04-16",
      "1991-04-19",
      "1991-04-24",
      "1991-04-29",
      "1991-05-02",
      "1991-05-07",
      "1991-05-10",
      "1991-05-15",
      "1991-05-20",
      "1991-05-23",
      "1991-05-29",
      "1991-06-03",
      "1991-06-06",
      "1991-06-11",
      "1991-06-14",
      "1991-06-19",
      "1991-06-24",
      "1991-06-27",
      "1991-07-02",
      "1991-07-08",
      "1991-07-11",
      "1991-07-16",
      "1991-07-19",
      "1991-07-24",
      "1991-07-29",
      "1991-08-01",
      "1991-08-06",
      "1991-08-09",
      "1991-08-14",
      "1991-08-19",
      "1991-08-22",
      "1991-08-27",
      "1991-08-30",
      "1991-09-05",
      "1991-09-10",
      "1991-09-13",
      "1991-09-18",
      "1991-09-23",
      "1991-09-26",
      "1991-10-01",
      "1991-10-04",
      "1991-10-09",
      "1991-10-14",
      "1991-10-17",
      "1991-10-22",
      "1991-10-25",
      "1991-10-30",
      "1991-11-04",
      "1991-11-07",
      "1991-11-12",
      "1991-11-15",
      "1991-11-20",
      "1991-11-25",
      "1991-11-29",
      "1991-12-04",
      "1991-12-09",
      "1991-12-12",
      "1991-12-17",
      "1991-12-20",
      "1991-12-26",
      "1991-12-31",
      "1992-01-06",
      "1992-01-09",
      "1992-01-14",
      "1992-01-17",
      "1992-01-22",
      "1992-01-27",
      "1992-01-30",
      "1992-02-04",
      "1992-02-07",
      "1992-02-12",
      "1992-02-18",
      "1992-02-21",
      "1992-02-26",
      "1992-03-02",
      "1992-03-05",
      "1992-03-10",
      "1992-03-13",
      "1992-03-18",
      "1992-03-23",
      "1992-03-26",
      "1992-03-31",
      "1992-04-03",
      "1992-04-08",
      "1992-04-13",
      "1992-04-16",
      "1992-04-22",
      "1992-04-27",
      "1992-04-30",
      "1992-05-05",
      "1992-05-08",
      "1992-05-13",
      "1992-05-18",
      "1992-05-21",
      "1992-05-27",
      "1992-06-01",
      "1992-06-04",
      "1992-06-09",
      "1992-06-12",
      "1992-06-17",
      "1992-06-22",
      "1992-06-25",
      "1992-06-30",
      "1992-07-06",
      "1992-07-09",
      "1992-07-14",
      "1992-07-17",
      "1992-07-22",
      "1992-07-27",
      "1992-07-30",
      "1992-08-04",
      "1992-08-07",
      "1992-08-12",
      "1992-08-17",
      "1992-08-20",
      "1992-08-25",
      "1992-08-28",
      "1992-09-02",
      "1992-09-08",
      "1992-09-11",
      "1992-09-16",
      "1992-09-21",
      "1992-09-24",
      "1992-09-29",
      "1992-10-02",
      "1992-10-07",
      "1992-10-12",
      "1992-10-15",
      "1992-10-20",
      "1992-10-23",
      "1992-10-28",
      "1992-11-02",
      "1992-11-05",
      "1992-11-10",
      "1992-11-13",
      "1992-11-18",
      "1992-11-23",
      "1992-11-27",
      "1992-12-02",
      "1992-12-07",
      "1992-12-10",
      "1992-12-15",
      "1992-12-18",
      "1992-12-23",
      "1992-12-29",
      "1993-01-04",
      "1993-01-07",
      "1993-01-12",
      "1993-01-15",
      "1993-01-20",
      "1993-01-25",
      "1993-01-28",
      "1993-02-02",
      "1993-02-05",
      "1993-02-10",
      "1993-02-16",
      "1993-02-19",
      "1993-02-24",
      "1993-03-01",
      "1993-03-04",
      "1993-03-09",
      "1993-03-12",
      "1993-03-17",
      "1993-03-22",
      "1993-03-25",
      "1993-03-30",
      "1993-04-02",
      "1993-04-07",
      "1993-04-13",
      "1993-04-16",
      "1993-04-21",
      "1993-04-26",
      "1993-04-29",
      "1993-05-04",
      "1993-05-07",
      "1993-05-12",
      "1993-05-17",
      "1993-05-20",
      "1993-05-25",
      "1993-05-28",
      "1993-06-03",
      "1993-06-08",
      "1993-06-11",
      "1993-06-16",
      "1993-06-21",
      "1993-06-24",
      "1993-06-29",
      "1993-07-02",
      "1993-07-08",
      "1993-07-13",
      "1993-07-16",
      "1993-07-21",
      "1993-07-26",
      "1993-07-29",
      "1993-08-03",
      "1993-08-06",
      "1993-08-11",
      "1993-08-16",
      "1993-08-19",
      "1993-08-24",
      "1993-08-27",
      "1993-09-01",
      "1993-09-07",
      "1993-09-10",
      "1993-09-15",
      "1993-09-20",
      "1993-09-23",
      "1993-09-28",
      "1993-10-01",
      "1993-10-06",
      "1993-10-11",
      "1993-10-14",
      "1993-10-19",
      "1993-10-22",
      "1993-10-27",
      "1993-11-01",
      "1993-11-04",
      "1993-11-09",
      "1993-11-12",
      "1993-11-17",
      "1993-11-22",
      "1993-11-26",
      "1993-12-01",
      "1993-12-06",
      "1993-12-09",
      "1993-12-14",
      "1993-12-17",
      "1993-12-22",
      "1993-12-28",
      "1993-12-31",
      "1994-01-05",
      "1994-01-10",
      "1994-01-13",
      "1994-01-18",
      "1994-01-21",
      "1994-01-26",
      "1994-01-31",
      "1994-02-03",
      "1994-02-08",
      "1994-02-11",
      "1994-02-16",
      "1994-02-22",
      "1994-02-25",
      "1994-03-02",
      "1994-03-07",
      "1994-03-10",
      "1994-03-15",
      "1994-03-18",
      "1994-03-23",
      "1994-03-28",
      "1994-03-31",
      "1994-04-06",
      "1994-04-11",
      "1994-04-14",
      "1994-04-19",
      "1994-04-22",
      "1994-04-28",
      "1994-05-03",
      "1994-05-06",
      "1994-05-11",
      "1994-05-16",
      "1994-05-19",
      "1994-05-24",
      "1994-05-27",
      "1994-06-02",
      "1994-06-07",
      "1994-06-10",
      "1994-06-15",
      "1994-06-20",
      "1994-06-23",
      "1994-06-28",
      "1994-07-01",
      "1994-07-07",
      "1994-07-12",
      "1994-07-15",
      "1994-07-20",
      "1994-07-25",
      "1994-07-28",
      "1994-08-02",
      "1994-08-05",
      "1994-08-10",
      "1994-08-15",
      "1994-08-18",
      "1994-08-23",
      "1994-08-26",
      "1994-08-31",
      "1994-09-06",
      "1994-09-09",
      "1994-09-14",
      "1994-09-19",
      "1994-09-22",
      "1994-09-27",
      "1994-09-30",
      "1994-10-05",
      "1994-10-10",
      "1994-10-13",
      "1994-10-18",
      "1994-10-21",
      "1994-10-26",
      "1994-10-31",
      "1994-11-03",
      "1994-11-08",
      "1994-11-11",
      "1994-11-16",
      "1994-11-21",
      "1994-11-25",
      "1994-11-30",
      "1994-12-05",
      "1994-12-08",
      "1994-12-13",
      "1994-12-16",
      "1994-12-21",
      "1994-12-27",
      "1994-12-30",
      "1995-01-05",
      "1995-01-10",
      "1995-01-13",
      "1995-01-18",
      "1995-01-23",
      "1995-01-26",
      "1995-01-31",
      "1995-02-03",
      "1995-02-08",
      "1995-02-13",
      "1995-02-16",
      "1995-02-22",
      "1995-02-27",
      "1995-03-02",
      "1995-03-07",
      "1995-03-10",
      "1995-03-15",
      "1995-03-20",
      "1995-03-23",
      "1995-03-28",
      "1995-03-31",
      "1995-04-05",
      "1995-04-10",
      "1995-04-13",
      "1995-04-19",
      "1995-04-24",
      "1995-04-27",
      "1995-05-02",
      "1995-05-05",
      "1995-05-10",
      "1995-05-15",
      "1995-05-18",
      "1995-05-23",
      "1995-05-26",
      "1995-06-01",
      "1995-06-06",
      "1995-06-09",
      "1995-06-14",
      "1995-06-19",
      "1995-06-22",
      "1995-06-27",
      "1995-06-30",
      "1995-07-06",
      "1995-07-11",
      "1995-07-14",
      "1995-07-19",
      "1995-07-24",
      "1995-07-27",
      "1995-08-01",
      "1995-08-04",
      "1995-08-09",
      "1995-08-14",
      "1995-08-17",
      "1995-08-22"
    ]
  },
  "series": [
    {
      "label": "Close Price",
      "type": "float",
      "raw": [
        90.833336,
        86.666664,
        85.833336,
        85.833336,
        86.666664,
        86.666664,
        83.333336,
        81.666664,
        80.0,
        80.0,
        80.833336,
        73.333336,
        73.333336,
        73.333336,
        71.666664,
        74.166664,
        74.166664,
        71.666664,
        70.0,
        70.0,
        71.666664,
        67.5,
        68.333336,
        70.833336,
        70.0,
        69.166664,
        70.0,
        70.0,
        70.0,
        70.833336,
        73.333336,
        75.0,
        69.166664,
        68.333336,
        68.333336,
        63.333332,
        59.166668,
        59.166668,
        59.166668,
        61.666668,
        61.666668,
        61.666668,
        61.666668,
        61.666668,
        65.833336,
        67.5,
        67.5,
        74.166664,
        74.166664,
        74.166664,
        73.333336,
        71.666664,
        71.666664,
        71.666664,
        70.0,
        71.666664,
        71.666664,
        71.666664,
        70.0,
        70.0,
        70.0,
        75.0,
        71.666664,
        71.666664,
        72.5,
        74.166664,
        73.333336,
        73.333336,
        73.333336,
        78.333336,
        80.0,
        82.5,
        78.333336,
        75.833336,
        76.666664,
        77.5,
        78.333336,
        76.666664,
        74.166664,
        77.5,
        78.333336,
        78.333336,
        86.666664,
        92.5,
        90.833336,
        88.333336,
        86.666664,
        85.833336,
        87.5,
        87.5,
        85.833336,
        83.333336,
        81.666664,
        81.666664,
        81.666664,
        79.166664,
        79.166664,
        83.333336,
        83.333336,
        88.333336,
        89.166664,
        87.5,
        86.666664,
        85.0,
        80.833336,
        76.666664,
        79.166664,
        77.5,
        80.0,
        75.0,
        81.666664,
        81.666664,
        85.833336,
        85.833336,
        85.833336,
        85.833336,
        85.833336,
        85.833336,
        80.0,
        78.333336,
        76.666664,
        77.5,
        77.5,
        81.666664,
        84.166664,
        90.0,
        86.666664,
        84.166664,
        84.166664,
        81.666664,
        87.5,
        84.166664,
        85.833336,
        84.166664,
        80.0,
        79.166664,
        76.666664,
        77.5,
        79.166664,
        77.5,
        74.166664,
        77.5,
        74.166664,
        74.166664,
        76.666664,
        75.833336,
        77.5,
        78.333336,
        76.666664,
        75.0,
        75.833336,
        75.833336,
        73.333336,
        71.666664,
        73.333336,
        74.166664,
        80.0,
        86.666664,
        91.666664,
        93.333336,
        95.0,
        97.5,
        97.5,
        94.166664,
        97.5,
        97.5,
        79.166664,
        82.5,
        82.5,
        90.0,
        88.333336,
        90.0,
        94.166664,
        89.166664,
        86.666664,
        84.166664,
        85.0,
        80.833336,
        80.0,
        80.0,
        80.0,
        82.5,
        81.666664,
        85.0,
        83.333336,
        76.666664,
        78.333336,
        75.833336,
        85.833336,
        85.0,
        79.166664,
        81.666664,
        83.333336,
        76.666664,
        78.333336,
        75.0,
        71.666664,
        70.833336,
        71.666664,
        73.333336,
        80.0,
        76.666664,
        78.333336,
        78.333336,
        74.166664,
        66.666664,
        66.666664,
        65.833336,
        63.333332,
        63.333332,
        64.166664,
        60.0,
        63.333332,
        55.833332,
        45.833332,
        46.666668,
        50.833332,
        50.833332,
        53.333332,
        56.666668,
        58.333332,
        56.666668,
        58.333332,
        60.0,
        62.5,
        67.5,
        63.333332,
        64.166664,
        62.5,
        64.166664,
        63.333332,
        63.333332,
        60.833332,
        59.166668,
        65.833336,
        61.666668,
        56.666668,
        58.333332,
        58.333332,
        56.666668,
        53.333332,
        55.0,
        53.333332,
        54.166668,
        54.166668,
        52.5,
        50.0,
        45.0,
        46.666668,
        46.666668,
        45.833332,
        48.333332,
        45.833332,
        50.833332,
        50.0,
        51.666668,
        49.166668,
        49.166668,
        45.833332,
        44.166668,
        44.166668,
        40.833332,
        41.666668,
        42.5,
        41.666668,
        45.0,
        45.833332,
        49.166668,
        47.5,
        45.833332,
        45.833332,
        42.5,
        33.333332,
        28.333334,
        30.833334,
        26.666666,
        25.833334,
        26.666666,
        27.5,
        29.166666,
        28.333334,
        20.0,
        17.5,
        17.5,
        20.833334,
        19.166666,
        15.0,
        13.333333,
        14.166667,
        13.333333,
        10.833333,
        11.666667,
        10.833333,
        9.166667,
        9.583333,
        9.583333,
        10.833333,
        8.333333,
        8.333333,
        7.5,
        7.916667,
        7.916667,
        7.5,
        6.666667,
        7.5,
        6.666667,
        7.083333,
        6.666667,
        7.083333,
        6.666667,
        5.833333,
        5.833333,
        5.416667,
        5.833333,
        5.0,
        5.0,
        6.25,
        5.416667,
        5.833333,
        5.833333,
        7.916667,
        7.5,
        7.083333,
        5.833333,
        6.25,
        6.666667,
        6.25,
        6.25,
        6.25,
        5.0,
        5.416667,
        5.416667,
        4.166667,
        4.166667,
        5.0,
        4.166667,
        4.583333,
        4.166667,
        5.0,
        4.166667,
        4.583333,
        4.166667,
        3.75,
        4.166667,
        4.583333,
        5.416667,
        5.416667,
        4.583333,
        5.0,
        4.166667,
        4.166667,
        4.166667,
        3.75,
        3.333333,
        4.166667,
        3.75,
        4.166667,
        3.333333,
        2.916667,
        2.916667,
        2.916667,
        2.916667,
        3.333333,
        4.166667,
        5.0,
        4.583333,
        5.416667,
        6.25,
        6.25,
        5.0,
        5.0,
        5.0,
        5.0,
        5.0,
        4.166667,
        5.416667,
        4.583333,
        4.583333,
        4.166667,
        4.166667,
        4.583333,
        4.166667,
        5.416667,
        5.416667,
        4.166667,
        5.0,
        4.166667,
        4.166667,
        4.166667,
        4.166667,
        4.166667,
        5.416667,
        5.0,
        4.583333,
        4.583333,
        4.583333,
        5.0,
        5.0,
        5.0,
        4.583333,
        4.583333,
        4.583333,
        5.0,
        6.25,
        7.5,
        7.916667,
        9.166667,
        10.416667,
        10.416667,
        10.833333,
        10.208333,
        10.416667,
        10.0,
        9.583333,
        9.583333,
        9.166667,
        11.25,
        9.583333,
        9.166667,
        9.583333,
        10.0,
        8.75,
        8.333333,
        8.75,
        9.166667,
        8.75,
        10.0,
        10.0,
        10.0,
        10.833333,
        10.833333,
        11.25,
        10.833333,
        10.833333,
        10.0,
        10.0,
        8.333333,
        8.75,
        9.166667,
        8.333333,
        8.333333,
        8.333333,
        8.333333,
        9.166667,
        8.333333,
        8.333333,
        8.75,
        8.333333,
        8.75,
        7.916667,
        8.333333,
        8.75,
        7.916667,
        7.5,
        7.083333,
        6.25,
        5.0,
        5.625,
        6.25,
        7.083333,
        7.5,
        7.5,
        9.166667,
        10.0,
        8.333333,
        9.166667,
        11.25,
        9.583333,
        10.0,
        9.583333,
        10.0,
        10.416667,
        10.416667,
        10.0,
        10.833333,
        10.416667,
        10.833333,
        11.666667,
        12.083333,
        12.083333,
        11.25,
        10.833333,
        10.416667,
        10.0,
        12.083333,
        10.833333,
        10.833333,
        11.666667,
        12.5,
        11.666667,
        12.5,
        12.083333,
        12.083333,
        14.166667,
        12.083333,
        10.0,
        10.0,
        10.0,
        11.666667,
        10.833333,
        10.416667,
        11.666667,
        10.416667,
        10.0,
        11.25,
        11.666667,
        10.416667,
        10.0,
        9.166667,
        9.583333,
        10.0,
        9.166667,
        9.166667,
        8.333333,
        9.583333,
        8.333333,
        9.166667,
        8.75,
        8.75,
        9.166667,
        9.583333,
        10.0,
        10.416667,
        10.0,
        8.75,
        7.916667,
        7.916667,
        7.083333,
        7.5,
        7.5,
        7.083333,
        7.083333,
        7.5,
        6.666667,
        6.666667,
        5.416667,
        5.416667,
        6.666667,
        7.5,
        6.666667,
        7.5,
        6.25,
        6.25,
        6.666667,
        6.666667,
        7.083333,
        7.5,
        6.666667,
        6.666667,
        6.666667,
        6.25,
        5.833333,
        6.666667,
        5.833333,
        5.833333,
        5.833333,
        5.833333,
        5.416667,
        5.833333,
        5.833333,
        5.0,
        4.583333,
        4.583333,
        4.166667,
        6.25,
        5.833333,
        5.0,
        4.583333,
        4.166667,
        4.166667,
        5.0,
        5.208333,
        5.208333,
        4.583333,
        5.208333,
        4.166667,
        5.0,
        3.75,
        4.583333,
        4.583333,
        3.75,
        4.166667,
        4.166667,
        3.75,
        3.333333,
        3.333333,
        3.333333,
        3.333333,
        3.75,
        4.166667,
        3.541667,
        4.166667,
        4.166667,
        3.75,
        3.333333,
        3.333333,
        4.583333,
        3.333333,
        4.583333,
        3.333333
      ]
    }
  ]
}'

ratner_stock
parsed_data <- fromJSON(ratner_stock)

cat("Name:", parsed_data$name, "\n")
cat("Longname:", parsed_data$longname, "\n")
cat("Number of Observations:", parsed_data$n_obs, "\n")
cat("Number of Dimensions:", parsed_data$n_dim, "\n")

cat("Time Format:", parsed_data$time$format, "\n")
cat("First 5 Time Indices:", head(parsed_data$time$index), "\n")
cat("First 5 Time Values:", head(parsed_data$time$raw), "\n")
cat("Series Label:", parsed_data$series$label, "\n")
cat("First 5 Series Values:", head(parsed_data$series$raw[[1]], 5), "\n")



# ggplot2 kC<tC<phanesini yC<kle
library(ggplot2)
library(changepoint)
# Time series data
time_raw <- as.numeric(as.Date(parsed_data$time$raw))
series_raw <- as.numeric(parsed_data$series$raw[[1]])

# Create a data frame
df <- data.frame(
  index = 1:length(time_raw),
  time = as.integer(time_raw),
  value = series_raw
)

plot(df$index, df$value, type = "l", xlab = "Index", ylab = "Value", main = "Time Series")

tsdata <- ts(df$value, start = min(df$index), end = max(df$index), frequency = 1)

###ratner_stock
##6<-281
##7<-176,286
##8<-272
##10<-205,272
##12<-197,271

###Amoc
d.m_amoc <- cpt.mean(tsdata, method = "AMOC")
plot(d.m_amoc)
abline(v = cpts(d.m_amoc), col = "red", lty = 2,lwd=2)
cpts(d.m_amoc)

###oracle settings
o.m_amoc <- cpt.mean(tsdata, penalty = "MBIC", method = "AMOC")
plot(o.m_amoc)
abline(v = cpts(o.m_amoc), col = "red", lty = 2,lwd=2)
cpts(o.m_amoc)

###BinSeg
#default
x<-cpt.mean(tsdata,method = "BinSeg",Q=3)
plot(x)
abline(v = cpts(x), col = "red", lty = 2,lwd=2)
cpts(x)

#oracle
m_binseg <- cpt.mean(tsdata, penalty = "BIC", method = "BinSeg", Q = 2)
plot(m_binseg)
abline(v = cpts(m_binseg), col = "red", lty = 2,lwd=2)
cpts(m_binseg)

###

m_binseg1 <- cpt.mean(tsdata, penalty = "BIC", method = "BinSeg", Q = 5)
plot(m_binseg1)
abline(v = cpts(m_binseg1), col = "red", lty = 2,lwd=2)
cpts(m_binseg1)

###  en iyisi bu

m_binseg2 <- cpt.mean(tsdata, penalty = "BIC", method = "BinSeg", Q = 7)
plot(m_binseg2)
abline(v = cpts(m_binseg2), col = "red", lty = 2,lwd=2)
cpts(m_binseg2)

###PELT_1
#default
m_pelt <- cpt.mean(tsdata, method = "PELT",minseglen =50)
plot(m_pelt, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(m_pelt), col = "blue", lty = 2,lwd=2)
cpts(m_pelt) 

###oracle
m_pelt1 <- cpt.mean(tsdata, penalty = "MBIC", method = "PELT", minseglen =20)
plot(m_pelt1, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(m_pelt1), col = "blue", lty = 2,lwd=2)
cpts(m_pelt1) 

###
m_pelt2 <- cpt.mean(tsdata, penalty = "MBIC", method = "PELT", minseglen = 25, pen.value = 5)
plot(m_pelt2, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(m_pelt2), col = "blue", lty = 2,lwd=2)
cpts(m_pelt2) 

## en iyisi bu
m_pelt3 <- cpt.mean(tsdata, penalty = "MBIC", method = "PELT", minseglen = 80, pen.value = 1)
plot(m_pelt3, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(m_pelt3), col = "blue", lty = 2,lwd=2)
cpts(m_pelt3) 

#VARYANSTA DEGISIM NOKTALARI ARAYAN ALGORITMA
v_pelt <- cpt.var(tsdata, method = "PELT")
plot(v_pelt, type = "l", cpt.col = "blue", xlab = "Index")
cpts(v_pelt)  

#HEM ORTALAMADA HEM DE VARYANSTA DEGISIM NOKTASI ARAMA
mv_pelt <- cpt.meanvar(tsdata, method = "PELT")
mv_pelt 
plot(mv_pelt)
cpts(mv_pelt)

###Segmented regression

#default
attach(df)
lm1<-lm(value~time)

seg_get<-segmented(lm1,seg.Z = ~ time,npsi = 5)
seg_get
seg_get$psi[,2]
cp <- round(seg_get$psi[,2])
cp_closest <- sapply(cp, function(bp) {
  d<- abs( time - bp)
  indd <- which.min(d)
  e <- c(time[indd])
  c(index_pos=indd, closest_time=e)
})
cp_closest
detach(df)

#oracle 
attach(df)
lm2<-lm(value~time)
seg_get2<-segmented(lm2,seg.Z = ~ time, npsi=8)
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
detach(df)

## PROPHET ##
#default
df_prophet <- data.frame(
  ds = as.Date(time_raw),
  y = as.numeric(series_raw)
)

model <- prophet(df_prophet,n.changepoints = 5)

changepoints <- model$changepoints
changepoints <- as.numeric(as.Date(changepoints))
print(changepoints)

indeksler <- which(time_raw %in% changepoints)
print(indeksler)

# Time series grafiDini C'izme model

changepoints_df <- data.frame(ds = as.Date(changepoints), y = max(df_prophet$y))

p <- ggplot(df_prophet, aes(x = ds, y = y)) +
  geom_line() +
  geom_vline(data = changepoints_df, aes(xintercept = as.numeric(ds)), color = "red", linetype = "dashed") +
  labs(title = "Time Series with Changepoints",
       x = "Date",
       y = "Value") +
  theme_minimal()

print(p)

## oracle

model2 <- prophet(df_prophet, n.changepoints = 20)
changepoints2 <- model2$changepoints
changepoints2 <- as.numeric(as.Date(changepoints2))
print(changepoints2)

indeksler2 <- which(time_raw %in% changepoints2)
print(indeksler2)
# Time series grafiDini C'izme model2

changepoints_df2 <- data.frame(ds = as.Date(changepoints2), y = max(df_prophet$y))

p2 <- ggplot(df_prophet, aes(x = ds, y = y)) +
  geom_line() +
  geom_vline(data = changepoints_df2, aes(xintercept = as.numeric(ds)), color = "red", linetype = "dashed") +
  labs(title = "Time Series with Changepoints",
       x = "Date",
       y = "Value") +
  theme_minimal()

print(p2)

## en iyisi bu oldu

model3 <- prophet(df_prophet,n.changepoints = 25)
changepoints3 <- model3$changepoints
changepoints3 <- as.numeric(as.Date(changepoints3))
print(changepoints3)

indeksler3 <- which(time_raw %in% changepoints3)
print(indeksler3)

# Time series grafiDini C'izme model3

changepoints_df3 <- data.frame(ds = as.Date(changepoints3), y = max(df_prophet$y))

p3 <- ggplot(df_prophet, aes(x = ds, y = y)) +
  geom_line() +
  geom_vline(data = changepoints_df3, aes(xintercept = as.numeric(ds)), color = "red", linetype = "dashed") +
  labs(title = "Time Series with Changepoints",
       x = "Date",
       y = "Value") +
  theme_minimal()

print(p3)

                              #### SCANLINE DATA ####
scanline_42049<- '{
  "name": "scanline_42049",
  "longname": "Scanline 42049",
  "n_obs": 481,
  "n_dim": 1,
  "time": {
    "index": [
      0,
      1,
      2,
      3,
      4,
      5,
      6,
      7,
      8,
      9,
      10,
      11,
      12,
      13,
      14,
      15,
      16,
      17,
      18,
      19,
      20,
      21,
      22,
      23,
      24,
      25,
      26,
      27,
      28,
      29,
      30,
      31,
      32,
      33,
      34,
      35,
      36,
      37,
      38,
      39,
      40,
      41,
      42,
      43,
      44,
      45,
      46,
      47,
      48,
      49,
      50,
      51,
      52,
      53,
      54,
      55,
      56,
      57,
      58,
      59,
      60,
      61,
      62,
      63,
      64,
      65,
      66,
      67,
      68,
      69,
      70,
      71,
      72,
      73,
      74,
      75,
      76,
      77,
      78,
      79,
      80,
      81,
      82,
      83,
      84,
      85,
      86,
      87,
      88,
      89,
      90,
      91,
      92,
      93,
      94,
      95,
      96,
      97,
      98,
      99,
      100,
      101,
      102,
      103,
      104,
      105,
      106,
      107,
      108,
      109,
      110,
      111,
      112,
      113,
      114,
      115,
      116,
      117,
      118,
      119,
      120,
      121,
      122,
      123,
      124,
      125,
      126,
      127,
      128,
      129,
      130,
      131,
      132,
      133,
      134,
      135,
      136,
      137,
      138,
      139,
      140,
      141,
      142,
      143,
      144,
      145,
      146,
      147,
      148,
      149,
      150,
      151,
      152,
      153,
      154,
      155,
      156,
      157,
      158,
      159,
      160,
      161,
      162,
      163,
      164,
      165,
      166,
      167,
      168,
      169,
      170,
      171,
      172,
      173,
      174,
      175,
      176,
      177,
      178,
      179,
      180,
      181,
      182,
      183,
      184,
      185,
      186,
      187,
      188,
      189,
      190,
      191,
      192,
      193,
      194,
      195,
      196,
      197,
      198,
      199,
      200,
      201,
      202,
      203,
      204,
      205,
      206,
      207,
      208,
      209,
      210,
      211,
      212,
      213,
      214,
      215,
      216,
      217,
      218,
      219,
      220,
      221,
      222,
      223,
      224,
      225,
      226,
      227,
      228,
      229,
      230,
      231,
      232,
      233,
      234,
      235,
      236,
      237,
      238,
      239,
      240,
      241,
      242,
      243,
      244,
      245,
      246,
      247,
      248,
      249,
      250,
      251,
      252,
      253,
      254,
      255,
      256,
      257,
      258,
      259,
      260,
      261,
      262,
      263,
      264,
      265,
      266,
      267,
      268,
      269,
      270,
      271,
      272,
      273,
      274,
      275,
      276,
      277,
      278,
      279,
      280,
      281,
      282,
      283,
      284,
      285,
      286,
      287,
      288,
      289,
      290,
      291,
      292,
      293,
      294,
      295,
      296,
      297,
      298,
      299,
      300,
      301,
      302,
      303,
      304,
      305,
      306,
      307,
      308,
      309,
      310,
      311,
      312,
      313,
      314,
      315,
      316,
      317,
      318,
      319,
      320,
      321,
      322,
      323,
      324,
      325,
      326,
      327,
      328,
      329,
      330,
      331,
      332,
      333,
      334,
      335,
      336,
      337,
      338,
      339,
      340,
      341,
      342,
      343,
      344,
      345,
      346,
      347,
      348,
      349,
      350,
      351,
      352,
      353,
      354,
      355,
      356,
      357,
      358,
      359,
      360,
      361,
      362,
      363,
      364,
      365,
      366,
      367,
      368,
      369,
      370,
      371,
      372,
      373,
      374,
      375,
      376,
      377,
      378,
      379,
      380,
      381,
      382,
      383,
      384,
      385,
      386,
      387,
      388,
      389,
      390,
      391,
      392,
      393,
      394,
      395,
      396,
      397,
      398,
      399,
      400,
      401,
      402,
      403,
      404,
      405,
      406,
      407,
      408,
      409,
      410,
      411,
      412,
      413,
      414,
      415,
      416,
      417,
      418,
      419,
      420,
      421,
      422,
      423,
      424,
      425,
      426,
      427,
      428,
      429,
      430,
      431,
      432,
      433,
      434,
      435,
      436,
      437,
      438,
      439,
      440,
      441,
      442,
      443,
      444,
      445,
      446,
      447,
      448,
      449,
      450,
      451,
      452,
      453,
      454,
      455,
      456,
      457,
      458,
      459,
      460,
      461,
      462,
      463,
      464,
      465,
      466,
      467,
      468,
      469,
      470,
      471,
      472,
      473,
      474,
      475,
      476,
      477,
      478,
      479,
      480
    ]
  },
  "series": [
    {
      "label": "Line 170",
      "type": "int",
      "raw": [
        162,
        164,
        165,
        165,
        165,
        166,
        169,
        171,
        170,
        171,
        172,
        173,
        174,
        175,
        176,
        177,
        178,
        178,
        178,
        179,
        180,
        181,
        181,
        182,
        182,
        183,
        183,
        184,
        186,
        187,
        187,
        188,
        188,
        188,
        188,
        189,
        189,
        190,
        190,
        190,
        191,
        191,
        191,
        191,
        191,
        191,
        191,
        191,
        189,
        193,
        190,
        194,
        194,
        201,
        185,
        120,
        138,
        140,
        128,
        152,
        130,
        110,
        103,
        109,
        89,
        83,
        76,
        67,
        54,
        41,
        37,
        39,
        39,
        39,
        45,
        32,
        94,
        208,
        199,
        194,
        195,
        196,
        196,
        197,
        197,
        196,
        196,
        195,
        196,
        196,
        196,
        196,
        196,
        196,
        196,
        196,
        196,
        196,
        196,
        197,
        197,
        198,
        198,
        198,
        197,
        197,
        197,
        197,
        197,
        197,
        197,
        197,
        198,
        198,
        198,
        198,
        198,
        198,
        198,
        198,
        199,
        199,
        199,
        199,
        199,
        199,
        199,
        199,
        198,
        198,
        198,
        199,
        199,
        200,
        200,
        200,
        201,
        201,
        201,
        201,
        201,
        201,
        201,
        201,
        202,
        202,
        201,
        201,
        201,
        201,
        200,
        200,
        201,
        201,
        200,
        200,
        200,
        201,
        202,
        202,
        202,
        202,
        202,
        202,
        202,
        202,
        202,
        202,
        204,
        204,
        204,
        204,
        204,
        204,
        204,
        204,
        204,
        208,
        204,
        203,
        207,
        203,
        204,
        218,
        161,
        63,
        68,
        63,
        57,
        62,
        64,
        47,
        42,
        28,
        13,
        22,
        41,
        35,
        17,
        15,
        15,
        15,
        15,
        15,
        16,
        16,
        16,
        16,
        23,
        25,
        12,
        43,
        80,
        67,
        74,
        56,
        50,
        41,
        39,
        31,
        41,
        51,
        49,
        70,
        78,
        45,
        71,
        44,
        50,
        41,
        56,
        74,
        81,
        69,
        29,
        32,
        19,
        24,
        29,
        32,
        25,
        32,
        50,
        75,
        69,
        63,
        60,
        24,
        23,
        52,
        39,
        33,
        40,
        34,
        33,
        28,
        34,
        44,
        44,
        16,
        18,
        26,
        37,
        26,
        23,
        23,
        23,
        25,
        27,
        30,
        33,
        35,
        46,
        8,
        151,
        232,
        205,
        217,
        210,
        209,
        210,
        210,
        210,
        210,
        210,
        209,
        209,
        209,
        209,
        209,
        209,
        209,
        209,
        209,
        209,
        209,
        209,
        209,
        209,
        208,
        208,
        207,
        207,
        207,
        207,
        207,
        207,
        207,
        207,
        207,
        207,
        207,
        206,
        206,
        206,
        206,
        206,
        206,
        206,
        206,
        205,
        205,
        205,
        205,
        205,
        205,
        205,
        205,
        204,
        204,
        204,
        204,
        204,
        204,
        204,
        204,
        203,
        204,
        204,
        204,
        203,
        203,
        202,
        201,
        204,
        198,
        201,
        206,
        194,
        167,
        150,
        148,
        166,
        127,
        99,
        72,
        64,
        70,
        48,
        28,
        28,
        87,
        175,
        212,
        218,
        213,
        199,
        213,
        205,
        202,
        200,
        200,
        201,
        202,
        201,
        200,
        200,
        200,
        200,
        200,
        200,
        200,
        200,
        200,
        202,
        202,
        201,
        201,
        201,
        200,
        200,
        200,
        198,
        198,
        198,
        198,
        198,
        198,
        198,
        198,
        198,
        198,
        198,
        198,
        198,
        198,
        198,
        198,
        197,
        197,
        197,
        196,
        196,
        195,
        195,
        195,
        195,
        195,
        195,
        195,
        195,
        195,
        195,
        195,
        195,
        195,
        195,
        195,
        195,
        195,
        195,
        195,
        194,
        194,
        194,
        194,
        194,
        194,
        194,
        194,
        194,
        194,
        194,
        193,
        193,
        192,
        192,
        192,
        192,
        192,
        192,
        192,
        192,
        192,
        192,
        192,
        190,
        193,
        193,
        191,
        192,
        193,
        190,
        185,
        196,
        189,
        155,
        104,
        76,
        75,
        68,
        52,
        40,
        32,
        96,
        124,
        93,
        145,
        210,
        181,
        178
      ]
    }
  ]
}'
scanline_42049
parsed_data <- fromJSON(scanline_42049)

cat("Name:", parsed_data$name, "\n")
cat("Longname:", parsed_data$longname, "\n")
cat("Number of Observations:", parsed_data$n_obs, "\n")
cat("Number of Dimensions:", parsed_data$n_dim, "\n")

cat("Time Format:", parsed_data$time$format, "\n")
cat("First 5 Time Indices:", head(parsed_data$time$index), "\n")
cat("First 5 Time Values:", head(parsed_data$time$raw), "\n")
cat("Series Label:", parsed_data$series$label, "\n")
cat("First 5 Series Values:", head(parsed_data$series$raw[[1]], 5), "\n")



# ggplot2 kC<tC<phanesini yC<kle
library(ggplot2)
library(changepoint)
# Time series data
time_raw <- as.numeric(parsed_data$time$index)
series_raw <- as.numeric(parsed_data$series$raw[[1]])

# Create a data frame
df <- data.frame(
  index = 1:length(time_raw),
  time = as.integer(time_raw),
  value = series_raw
)

plot(df$index, df$value, type = "l", xlab = "Index", ylab = "Value", main = "Time Series")




#ORTALAMADA DEGISIM NOKTASI ARAYAN ALGORITMALAR

tsdata <- ts(df$value, start = min(df$index), end = max(df$index), frequency = 1)
print(tsdata)

##

#scanline_42049
#	6 <- 54,76,77,183,185,273,275,348,360,363
# 7 <- 55,77,184,275,349,466
# 8 <- 185,275
# 9 <- 54,76,184,274,349,363,466
# 13 <- 52,78,183,277,346,366,464

# AMOC #
# default settings
d.m_amoc <- cpt.mean(tsdata, method = "AMOC")
plot(d.m_amoc)
abline(v = cpts(d.m_amoc), col = "red", lty = 2,lwd=2)
cpts(d.m_amoc)

# oracle settings
o.m_amoc <- cpt.mean(tsdata, penalty = "BIC", method = "AMOC", pen.value = 10)
plot(o.m_amoc)
abline(v = cpts(o.m_amoc), col = "red", lty = 2,lwd=2)
cpts(o.m_amoc)

# BINSEG #
# default settings
d.m_binseg <-cpt.mean(tsdata,method = "BinSeg")
plot(d.m_binseg)
abline(v = cpts(d.m_binseg), col = "red", lty = 2,lwd=2)
cpts(d.m_binseg)

# oracle settings
o.m_binseg <- cpt.mean(tsdata, penalty = "BIC",pen.value = 10, method = "BinSeg", Q = 2)
plot(o.m_binseg)
abline(v = cpts(o.m_binseg), col = "red", lty = 2,lwd=2)
cpts(o.m_binseg)

#
o.m_binseg1 <- cpt.mean(tsdata, penalty = "BIC",pen.value = 10, method = "BinSeg", Q = 7)
plot(o.m_binseg1)
abline(v = cpts(o.m_binseg1), col = "red", lty = 2,lwd=2)
cpts(o.m_binseg1)

# en iyisi bu
o.m_binseg2 <- cpt.mean(tsdata, penalty = "BIC",pen.value = 10, method = "BinSeg", Q = 10)
plot(o.m_binseg2)
abline(v = cpts(o.m_binseg2), col = "red", lty = 2,lwd=2)
cpts(o.m_binseg2)

# PELT #
# default settings
d.m_pelt <- cpt.mean(tsdata, method = "PELT",minseglen = 28)
plot(d.m_pelt)
abline(v = cpts(d.m_pelt), col = "red", lty = 2,lwd=1)
cpts(d.m_pelt)

# oracle settings
o.m_pelt <- cpt.mean(tsdata, penalty = "BIC", pen.value = 10, method = "PELT", minseglen = 25)
plot(o.m_pelt, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(o.m_pelt), col = "blue", lty = 2,lwd=1)
cpts(o.m_pelt)   

# en iyisi bu
o.m_pelt1 <- cpt.mean(tsdata, penalty = "BIC", pen.value = 10, method = "PELT", minseglen = 20)
plot(o.m_pelt1, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(o.m_pelt1), col = "blue", lty = 2,lwd=1)
cpts(o.m_pelt1)  

# denecekk hangisi yuksekse o konulacak
o.m_pelt2 <- cpt.meanvar(tsdata, penalty = "BIC", pen.value = 10, method = "PELT", minseglen = 20)
plot(o.m_pelt2, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(o.m_pelt2), col = "blue", lty = 2,lwd=1)
cpts(o.m_pelt2)  
#VARYANSTA DEGiSiM NOKTALARI ARAYAN ALGORiTMA
v_pelt <- cpt.var(tsdata, method = "PELT")
plot(v_pelt, type = "l", cpt.col = "blue", xlab = "Index")
cpts(v_pelt)  

#HEM ORTALAMADA HEM DE VARYANSTA DEGiSiM NOKTASI ARAMA
mv_pelt <- cpt.meanvar(tsdata, method = "PELT")
mv_pelt 
plot(mv_pelt)
cpts(mv_pelt)   

# SEGMENTED REGRESSION #
#default
attach(df)
lm1<-lm(value~time)
seg_get<-segmented(lm1,seg.Z = ~ time , npsi=7)
seg_get
seg_get$psi[,2]
cp <- round(seg_get$psi[,2])
cp_closest <- sapply(cp, function(bp) {
  d<- abs( time - bp)
  indd <- which.min(d)
  e <- c(time[indd])
  c(index_pos=indd, closest_time=e)
})
cp_closest
detach(df)

#oracle
attach(df)
lm2<-lm(value~time)
seg_get2<-segmented(lm2,seg.Z = ~ time, npsi=10)
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
detach(df)

## PROPHET ##
#default
df_prophet <- data.frame(
  ds = as.Date(time_raw), # Tarih sC<tunu
  y = as.numeric(series_raw) # DeDer sC<tunu
)

model <- prophet(df_prophet, n.changepoints = 20)

changepoints <- model$changepoints
changepoints <- as.numeric(as.Date(changepoints))
print(changepoints)

indeksler <- which(time_raw %in% changepoints)
print(indeksler)

# DeDiEim noktalarD1nD1 iC'eren bir veri C'erC'evesi oluEturma
changepoints_df <- data.frame(ds = as.Date(changepoints), y = max(df_prophet$y))

# Time series grafiDini C'izme
p <- ggplot(df_prophet, aes(x = ds, y = y)) +
  geom_line() +
  geom_vline(data = changepoints_df, aes(xintercept = as.numeric(ds)), color = "red", linetype = "dashed") +
  labs(title = "Time Series with Changepoints",
       x = "Date",
       y = "Value") +
  theme_minimal()

print(p)


# Time series grafiDini C'izme model1

changepoints_df1 <- data.frame(ds = as.Date(changepoints1), y = max(df_prophet$y))

p1 <- ggplot(df_prophet, aes(x = ds, y = y)) +
  geom_line() +
  geom_vline(data = changepoints_df1, aes(xintercept = as.numeric(ds)), color = "red", linetype = "dashed") +
  labs(title = "Time Series with Changepoints",
       x = "Date",
       y = "Value") +
  theme_minimal()

print(p1)

# en iyisi bu

model2 <- prophet(df_prophet,n.changepoints = 21)
changepoints2 <- model2$changepoints
changepoints2 <- as.numeric(as.Date(changepoints2))
print(changepoints2)

indeksler2 <- which(time_raw %in% changepoints2)
print(indeksler2)


# Time series grafiDini C'izme model2

changepoints_df2 <- data.frame(ds = as.Date(changepoints2), y = max(df_prophet$y))

p2 <- ggplot(df_prophet, aes(x = ds, y = y)) +
  geom_line() +
  geom_vline(data = changepoints_df2, aes(xintercept = as.numeric(ds)), color = "red", linetype = "dashed") +
  labs(title = "Time Series with Changepoints",
       x = "Date",
       y = "Value") +
  theme_minimal()

print(p2)

                             #### SHANGHAI LICENSE DATA ####

shanghai_license<- '{
  "name": "shanghai_license",
  "longname": "Shanghai License",
  "n_obs": 205,
  "n_dim": 1,
  "time": {
    "type": "string",
    "format": "%Y-%m",
    "index": [
      0,
      1,
      2,
      3,
      4,
      5,
      6,
      7,
      8,
      9,
      10,
      11,
      12,
      13,
      14,
      15,
      16,
      17,
      18,
      19,
      20,
      21,
      22,
      23,
      24,
      25,
      26,
      27,
      28,
      29,
      30,
      31,
      32,
      33,
      34,
      35,
      36,
      37,
      38,
      39,
      40,
      41,
      42,
      43,
      44,
      45,
      46,
      47,
      48,
      49,
      50,
      51,
      52,
      53,
      54,
      55,
      56,
      57,
      58,
      59,
      60,
      61,
      62,
      63,
      64,
      65,
      66,
      67,
      68,
      69,
      70,
      71,
      72,
      73,
      74,
      75,
      76,
      77,
      78,
      79,
      80,
      81,
      82,
      83,
      84,
      85,
      86,
      87,
      88,
      89,
      90,
      91,
      92,
      93,
      94,
      95,
      96,
      97,
      98,
      99,
      100,
      101,
      102,
      103,
      104,
      105,
      106,
      107,
      108,
      109,
      110,
      111,
      112,
      113,
      114,
      115,
      116,
      117,
      118,
      119,
      120,
      121,
      122,
      123,
      124,
      125,
      126,
      127,
      128,
      129,
      130,
      131,
      132,
      133,
      134,
      135,
      136,
      137,
      138,
      139,
      140,
      141,
      142,
      143,
      144,
      145,
      146,
      147,
      148,
      149,
      150,
      151,
      152,
      153,
      154,
      155,
      156,
      157,
      158,
      159,
      160,
      161,
      162,
      163,
      164,
      165,
      166,
      167,
      168,
      169,
      170,
      171,
      172,
      173,
      174,
      175,
      176,
      177,
      178,
      179,
      180,
      181,
      182,
      183,
      184,
      185,
      186,
      187,
      188,
      189,
      190,
      191,
      192,
      193,
      194,
      195,
      196,
      197,
      198,
      199,
      200,
      201,
      202,
      203,
      204
    ],
    "raw": [
      "2002-01",
      "2002-02",
      "2002-03",
      "2002-04",
      "2002-05",
      "2002-06",
      "2002-07",
      "2002-08",
      "2002-09",
      "2002-10",
      "2002-11",
      "2002-12",
      "2003-01",
      "2003-02",
      "2003-03",
      "2003-04",
      "2003-05",
      "2003-06",
      "2003-07",
      "2003-08",
      "2003-09",
      "2003-10",
      "2003-11",
      "2003-12",
      "2004-01",
      "2004-02",
      "2004-03",
      "2004-04",
      "2004-05",
      "2004-06",
      "2004-07",
      "2004-08",
      "2004-09",
      "2004-10",
      "2004-11",
      "2004-12",
      "2005-01",
      "2005-02",
      "2005-03",
      "2005-04",
      "2005-05",
      "2005-06",
      "2005-07",
      "2005-08",
      "2005-09",
      "2005-10",
      "2005-11",
      "2005-12",
      "2006-01",
      "2006-02",
      "2006-03",
      "2006-04",
      "2006-05",
      "2006-06",
      "2006-07",
      "2006-08",
      "2006-09",
      "2006-10",
      "2006-11",
      "2006-12",
      "2007-01",
      "2007-02",
      "2007-03",
      "2007-04",
      "2007-05",
      "2007-06",
      "2007-07",
      "2007-08",
      "2007-09",
      "2007-10",
      "2007-11",
      "2007-12",
      "2008-01",
      "2008-02",
      "2008-03",
      "2008-04",
      "2008-05",
      "2008-06",
      "2008-07",
      "2008-08",
      "2008-09",
      "2008-10",
      "2008-11",
      "2008-12",
      "2009-01",
      "2009-02",
      "2009-03",
      "2009-04",
      "2009-05",
      "2009-06",
      "2009-07",
      "2009-08",
      "2009-09",
      "2009-10",
      "2009-11",
      "2009-12",
      "2010-01",
      "2010-02",
      "2010-03",
      "2010-04",
      "2010-05",
      "2010-06",
      "2010-07",
      "2010-08",
      "2010-09",
      "2010-10",
      "2010-11",
      "2010-12",
      "2011-01",
      "2011-02",
      "2011-03",
      "2011-04",
      "2011-05",
      "2011-06",
      "2011-07",
      "2011-08",
      "2011-09",
      "2011-10",
      "2011-11",
      "2011-12",
      "2012-01",
      "2012-02",
      "2012-03",
      "2012-04",
      "2012-05",
      "2012-06",
      "2012-07",
      "2012-08",
      "2012-09",
      "2012-10",
      "2012-11",
      "2012-12",
      "2013-01",
      "2013-02",
      "2013-03",
      "2013-04",
      "2013-05",
      "2013-06",
      "2013-07",
      "2013-08",
      "2013-09",
      "2013-10",
      "2013-11",
      "2013-12",
      "2014-01",
      "2014-02",
      "2014-03",
      "2014-04",
      "2014-05",
      "2014-06",
      "2014-07",
      "2014-08",
      "2014-09",
      "2014-10",
      "2014-11",
      "2014-12",
      "2015-01",
      "2015-02",
      "2015-03",
      "2015-04",
      "2015-05",
      "2015-06",
      "2015-07",
      "2015-08",
      "2015-09",
      "2015-10",
      "2015-11",
      "2015-12",
      "2016-01",
      "2016-02",
      "2016-03",
      "2016-04",
      "2016-05",
      "2016-06",
      "2016-07",
      "2016-08",
      "2016-09",
      "2016-10",
      "2016-11",
      "2016-12",
      "2017-01",
      "2017-02",
      "2017-03",
      "2017-04",
      "2017-05",
      "2017-06",
      "2017-07",
      "2017-08",
      "2017-09",
      "2017-10",
      "2017-11",
      "2017-12",
      "2018-01",
      "2018-02",
      "2018-03",
      "2018-04",
      "2018-05",
      "2018-06",
      "2018-07",
      "2018-08",
      "2018-09",
      "2018-10",
      "2018-11",
      "2018-12",
      "2019-01"
    ]
  },
  "series": [
    {
      "label": "No. of Applicants",
      "type": "int",
      "raw": [
        3718,
        4590,
        5190,
        4806,
        4665,
        4502,
        3774,
        4640,
        4393,
        4661,
        4021,
        3525,
        9442,
        12030,
        11219,
        8794,
        14634,
        15507,
        11929,
        9315,
        8532,
        9383,
        9849,
        10491,
        8663,
        10156,
        9950,
        8150,
        8114,
        19233,
        14464,
        15506,
        10634,
        9519,
        9188,
        9005,
        6208,
        8949,
        9117,
        8113,
        9673,
        8409,
        8777,
        7520,
        10972,
        11167,
        13633,
        8351,
        5907,
        12367,
        8904,
        7888,
        8301,
        8478,
        8966,
        9190,
        7064,
        11237,
        110234,
        9477,
        6587,
        5056,
        10168,
        10523,
        10273,
        11478,
        10327,
        12943,
        10561,
        10715,
        10596,
        10356,
        10269.5,
        10269.5,
        63534,
        37072,
        26341,
        21208,
        16783,
        13451,
        11002,
        11882,
        10170,
        16801,
        16544,
        16848,
        18575,
        17654,
        16471,
        17433,
        17220,
        18750,
        14906,
        22006,
        21902,
        18577,
        18975,
        18810,
        17704,
        17313,
        16324,
        16252,
        13389,
        16855,
        15198,
        14941,
        13429,
        11224,
        30675,
        25104,
        25014,
        22326,
        25708,
        22474,
        21852,
        21544,
        22268,
        19415,
        20050,
        26531,
        24354,
        23391,
        24897,
        22706,
        24230,
        24774,
        26526,
        21425,
        19114,
        19921,
        19120,
        18244,
        20857,
        24651,
        23589,
        26174,
        22224,
        21482,
        21811,
        22650,
        35154,
        28887,
        38220,
        39625,
        41946,
        45758,
        61853,
        94241,
        114121,
        135677,
        136098,
        121550,
        122219,
        105532,
        95595,
        96972,
        98203,
        103224,
        132690,
        152298,
        156007,
        172205,
        166302,
        166939,
        165765,
        170995,
        169159,
        179133,
        187533,
        196470,
        221109,
        256897,
        277889,
        275438,
        240750,
        251188,
        229544,
        213212,
        215424,
        219882,
        232101,
        251717,
        262010,
        252273,
        270197,
        244349,
        269189,
        256083,
        250566,
        244868,
        226911,
        228148,
        226316,
        220831,
        217056,
        204980,
        198627,
        209672,
        202337,
        192755,
        189142,
        181861,
        177355,
        165442,
        168614
      ]
    }
  ]
}'

shanghai_license
parsed_data <- fromJSON(shanghai_license)

cat("Name:", parsed_data$name, "\n")
cat("Longname:", parsed_data$longname, "\n")
cat("Number of Observations:", parsed_data$n_obs, "\n")
cat("Number of Dimensions:", parsed_data$n_dim, "\n")

cat("Time Format:", parsed_data$time$format, "\n")
cat("First 5 Time Indices:", head(parsed_data$time$index), "\n")
cat("First 5 Time Values:", head(parsed_data$time$raw), "\n")
cat("Series Label:", parsed_data$series$label, "\n")
cat("First 5 Series Values:", head(parsed_data$series$raw[[1]], 5), "\n")

# Check for non-numeric values
shanghai_license_list <- fromJSON(shanghai_license)

date_raw <- as.numeric(as.Date(paste0(shanghai_license_list$time$raw,"-15")))
series_raw <- shanghai_license_list$series$raw[[1]]


shanghai_license_df <- data.frame(
  index = 1:length(date_raw),
  time = as.numeric(date_raw),
  value = as.numeric(series_raw)
  
)

# Print the data frame
print(shanghai_license_df)
plot(shanghai_license_df$index, shanghai_license_df$value, type = "l", xlab = "Index", ylab = "Value", main = "Time Series")

tsdata <- ts(shanghai_license_df$value, start = min(shanghai_license_df$index), end = max(shanghai_license_df$index), frequency = 1)
print(tsdata)

### shanghai_license annotations
# 6 -> 140
# 8 -> 146
# 10 -> 146
# 12 -> 145
# 13 -> 108,140

#ORTALAMADA DEGISIM NOKTASI ARAYAN ALGORITMALAR

## AMOC ##

# default settings
d.m_amoc <- cpt.mean(tsdata, method = "AMOC")
plot(d.m_amoc)
abline(v = cpts(d.m_amoc), col = "red", lty = 2,lwd=2)
cpts(d.m_amoc)

# oracle settings
#1
o.m_amoc1 <- cpt.mean(tsdata, method = "AMOC", penalty = "BIC", pen.value = 10)
plot(o.m_amoc1)
abline(v = cpts(o.m_amoc1), col = "red", lty = 2,lwd=2)
cpts(o.m_amoc1)

#2
o.m_amoc2 <- cpt.mean(tsdata, method = "AMOC", penalty = "SIC" )
plot(o.m_amoc2)
abline(v = cpts(o.m_amoc2), col = "red", lty = 2,lwd=2)
cpts(o.m_amoc2)

#3
o.m_amoc3 <- cpt.mean(tsdata, method = "AMOC", penalty = "AIC" )
plot(o.m_amoc3)
abline(v = cpts(o.m_amoc3), col = "red", lty = 2,lwd=2)
cpts(o.m_amoc3)

#4
o.m_amoc4 <- cpt.mean(tsdata, method = "AMOC", penalty = "None" )
plot(o.m_amoc4)
abline(v = cpts(o.m_amoc4), col = "red", lty = 2,lwd=2)
cpts(o.m_amoc4)

#5
o.m_amoc5 <- cpt.mean(tsdata, method = "AMOC", penalty = "Manual" )
plot(o.m_amoc5)
abline(v = cpts(o.m_amoc5), col = "red", lty = 2,lwd=2)
cpts(o.m_amoc5)

#6
o.m_amoc6 <- cpt.mean(tsdata, method = "AMOC", penalty = "Hannan-Quinn")
plot(o.m_amoc6)
abline(v = cpts(o.m_amoc6), col = "red", lty = 2,lwd=2)
cpts(o.m_amoc6)

# BINSEG #
# default settings
d.m_binseg <-cpt.mean(tsdata,method = "BinSeg",Q=5)
plot(d.m_binseg)
abline(v = cpts(d.m_binseg), col = "red", lty = 2,lwd=2)
cpts(d.m_binseg)

# oracle settings

#1 en iyiiii
o.m_binseg1 <- cpt.mean(tsdata,method = "BinSeg", penalty = "BIC", pen.value = 0.05, Q = 18)
plot(o.m_binseg1)
abline(v = cpts(o.m_binseg1), col = "red", lty = 2,lwd=2)
cpts(o.m_binseg1)

#2
o.m_binseg2 <- cpt.mean(tsdata,method = "BinSeg", penalty = "AIC", pen.value = 100, Q = 3)
plot(o.m_binseg2)
abline(v = cpts(o.m_binseg2), col = "red", lty = 2,lwd=2)
cpts(o.m_binseg2)

#3 
o.m_binseg3 <- cpt.mean(tsdata,method = "BinSeg", penalty = "BIC", pen.value = 0.05, Q = 4)
plot(o.m_binseg3)
abline(v = cpts(o.m_binseg3), col = "red", lty = 2,lwd=2)
cpts(o.m_binseg3)

#4 
o.m_binseg4 <- cpt.mean(tsdata,method = "BinSeg", penalty = "SIC", pen.value = 100, Q = 6)
plot(o.m_binseg4)
abline(v = cpts(o.m_binseg4), col = "red", lty = 2,lwd=2)
cpts(o.m_binseg4)

# PELT #
# default settings
d.m_pelt <- cpt.mean(tsdata, method = "PELT",minseglen = 20)
plot(d.m_pelt)
abline(v = cpts(d.m_pelt), col = "red", lty = 2,lwd=1)
cpts(d.m_pelt)

# oracle settings
#1
o.m_pelt1 <- cpt.mean(tsdata, method = "PELT", penalty = "BIC", pen.value = 0.05, minseglen = 25)
plot(o.m_pelt1, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(o.m_pelt1), col = "blue", lty = 2,lwd=1)
cpts(o.m_pelt1)   

#2
o.m_pelt2 <- cpt.mean(tsdata, method = "PELT", penalty = "AIC", pen.value = 0.05, minseglen = 30)
plot(o.m_pelt2, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(o.m_pelt2), col = "blue", lty = 2,lwd=1)
cpts(o.m_pelt2)

#3 en iyiiiiiii :D
o.m_pelt3 <- cpt.mean(tsdata, method = "PELT", penalty = "AIC", pen.value = 0.05, minseglen = 27)
plot(o.m_pelt3, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(o.m_pelt3), col = "blue", lty = 2,lwd=1)
cpts(o.m_pelt3)

#4 denenecekkkk iyiyse bu alinacak
o.m_pelt4 <- cpt.meanvar(tsdata, method = "PELT", penalty = "AIC", pen.value = 0.05, minseglen = 27)
plot(o.m_pelt4, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(o.m_pelt4), col = "blue", lty = 2,lwd=1)
cpts(o.m_pelt4)

#VARYANSTA DEGiSiM NOKTALARI ARAYAN ALGORiTMA
v_pelt <- cpt.var(tsdata, method = "PELT")
plot(v_pelt, type = "l", cpt.col = "blue", xlab = "Index")
cpts(v_pelt)  

#HEM ORTALAMADA HEM DE VARYANSTA DEGiSiM NOKTASI ARAMA
mv_pelt <- cpt.meanvar(tsdata, method = "PELT")
mv_pelt 
plot(mv_pelt)
cpts(mv_pelt)   

# SEGMENTED REGRESSION #
#default
attach(shanghai_license_df)
lm1<-lm(value~time)

seg_get<-segmented(lm1,seg.Z = ~ time,npsi = 3)
seg_get
seg_get$psi[,2]
cp <- round(seg_get$psi[,2])
cp_closest <- sapply(cp, function(bp) {
  d<- abs( time - bp)
  indd <- which.min(d)
  e <- c(time[indd])
  c(index_pos=indd, closest_time=e)
})
cp_closest
detach(shanghai_license_df)

#1 en iyiii :D
attach(shanghai_license_df)
lm2<-lm(value~time)

seg_get2<-segmented(lm2,seg.Z = ~ time, npsi=2)
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
detach(shanghai_license_df)

#2
attach(shanghai_license_df)
lm3<-lm(value~time)

seg_get3<-segmented(lm3,seg.Z = ~ time, npsi=4)
seg_get3
seg_get3$psi[,2]
cp <- round(seg_get3$psi[,2])
cp_closest3 <- sapply(cp, function(bp) {
  d<- abs( time - bp)
  indd <- which.min(d)
  e <- c(time[indd])
  c(index_pos=indd, closest_time=e)
})
cp_closest3
detach(shanghai_license_df)
## PROPHET ##

df_prophet <- data.frame(
  ds = as.Date(date_raw), # Tarih sC<tunu
  y = as.numeric(series_raw) # DeDer sC<tunu
)

model <- prophet(df_prophet,n.changepoints = 7)
changepoints <- model$changepoints
changepoints <- as.numeric(as.Date(changepoints))
print(changepoints)

indeksler <- which(date_raw %in% changepoints)
print(indeksler)

# Time series grafiDini C'izme model 
changepoints_df <- data.frame(ds = as.Date(changepoints), y = max(df_prophet$y))


p <- ggplot(df_prophet, aes(x = ds, y = y)) +
  geom_line() +
  geom_vline(data = changepoints_df, aes(xintercept = as.numeric(ds)), color = "red", linetype = "dashed") +
  labs(title = "Time Series with Changepoints",
       x = "Date",
       y = "Value") +
  theme_minimal()

print(p)

## en iyi olarak bunu ve alttakini dener misinnn

model2 <- prophet(df_prophet, n.changepoints = 14)
changepoints2 <- model2$changepoints
changepoints2 <- as.numeric(as.Date(changepoints2))
print(changepoints2)

indeksler2 <- which(date_raw %in% changepoints2)
print(indeksler2)

# Time series grafiDini C'izme model2

changepoints_df2 <- data.frame(ds = as.Date(changepoints2), y = max(df_prophet$y))

p2 <- ggplot(df_prophet, aes(x = ds, y = y)) +
  geom_line() +
  geom_vline(data = changepoints_df2, aes(xintercept = as.numeric(ds)), color = "red", linetype = "dashed") +
  labs(title = "Time Series with Changepoints",
       x = "Date",
       y = "Value") +
  theme_minimal()

print(p2)

## en iyiii

model3 <- prophet(df_prophet,n.changepoints = 8)
changepoints3 <- model3$changepoints
changepoints3 <- as.numeric(as.Date(changepoints3))
print(changepoints3)

indeksler3 <- which(date_raw %in% changepoints3)
print(indeksler3)


# Time series grafiDini C'izme model3

changepoints_df3 <- data.frame(ds = as.Date(changepoints3), y = max(df_prophet$y))

p3 <- ggplot(df_prophet, aes(x = ds, y = y)) +
  geom_line() +
  geom_vline(data = changepoints_df3, aes(xintercept = as.numeric(ds)), color = "red", linetype = "dashed") +
  labs(title = "Time Series with Changepoints",
       x = "Date",
       y = "Value") +
  theme_minimal()

print(p3)

                               #### USD ISK DATA ####

usd_isk<- '{
  "name": "usd_isk",
  "longname": "USD-ISK exhange rate",
  "n_obs": 247,
  "n_dim": 1,
  "time": {
    "format": "%Y-%m",
    "index": [
      0,
      1,
      2,
      3,
      4,
      5,
      6,
      7,
      8,
      9,
      10,
      11,
      12,
      13,
      14,
      15,
      16,
      17,
      18,
      19,
      20,
      21,
      22,
      23,
      24,
      25,
      26,
      27,
      28,
      29,
      30,
      31,
      32,
      33,
      34,
      35,
      36,
      37,
      38,
      39,
      40,
      41,
      42,
      43,
      44,
      45,
      46,
      47,
      48,
      49,
      50,
      51,
      52,
      53,
      54,
      55,
      56,
      57,
      58,
      59,
      60,
      61,
      62,
      63,
      64,
      65,
      66,
      67,
      68,
      69,
      70,
      71,
      72,
      73,
      74,
      75,
      76,
      77,
      78,
      79,
      80,
      81,
      82,
      83,
      84,
      85,
      86,
      87,
      88,
      89,
      90,
      91,
      92,
      93,
      94,
      95,
      96,
      97,
      98,
      99,
      100,
      101,
      102,
      103,
      104,
      105,
      106,
      107,
      108,
      109,
      110,
      111,
      112,
      113,
      114,
      115,
      116,
      117,
      118,
      119,
      120,
      121,
      122,
      123,
      124,
      125,
      126,
      127,
      128,
      129,
      130,
      131,
      132,
      133,
      134,
      135,
      136,
      137,
      138,
      139,
      140,
      141,
      142,
      143,
      144,
      145,
      146,
      147,
      148,
      149,
      150,
      151,
      152,
      153,
      154,
      155,
      156,
      157,
      158,
      159,
      160,
      161,
      162,
      163,
      164,
      165,
      166,
      167,
      168,
      169,
      170,
      171,
      172,
      173,
      174,
      175,
      176,
      177,
      178,
      179,
      180,
      181,
      182,
      183,
      184,
      185,
      186,
      187,
      188,
      189,
      190,
      191,
      192,
      193,
      194,
      195,
      196,
      197,
      198,
      199,
      200,
      201,
      202,
      203,
      204,
      205,
      206,
      207,
      208,
      209,
      210,
      211,
      212,
      213,
      214,
      215,
      216,
      217,
      218,
      219,
      220,
      221,
      222,
      223,
      224,
      225,
      226,
      227,
      228,
      229,
      230,
      231,
      232,
      233,
      234,
      235,
      236,
      237,
      238,
      239,
      240,
      241,
      242,
      243,
      244,
      245,
      246
    ],
    "raw": [
      "1999-01",
      "1999-02",
      "1999-03",
      "1999-04",
      "1999-05",
      "1999-06",
      "1999-07",
      "1999-08",
      "1999-09",
      "1999-10",
      "1999-11",
      "1999-12",
      "2000-01",
      "2000-02",
      "2000-03",
      "2000-04",
      "2000-05",
      "2000-06",
      "2000-07",
      "2000-08",
      "2000-09",
      "2000-10",
      "2000-11",
      "2000-12",
      "2001-01",
      "2001-02",
      "2001-03",
      "2001-04",
      "2001-05",
      "2001-06",
      "2001-07",
      "2001-08",
      "2001-09",
      "2001-10",
      "2001-11",
      "2001-12",
      "2002-01",
      "2002-02",
      "2002-03",
      "2002-04",
      "2002-05",
      "2002-06",
      "2002-07",
      "2002-08",
      "2002-09",
      "2002-10",
      "2002-11",
      "2002-12",
      "2003-01",
      "2003-02",
      "2003-03",
      "2003-04",
      "2003-05",
      "2003-06",
      "2003-07",
      "2003-08",
      "2003-09",
      "2003-10",
      "2003-11",
      "2003-12",
      "2004-01",
      "2004-02",
      "2004-03",
      "2004-04",
      "2004-05",
      "2004-06",
      "2004-07",
      "2004-08",
      "2004-09",
      "2004-10",
      "2004-11",
      "2004-12",
      "2005-01",
      "2005-02",
      "2005-03",
      "2005-04",
      "2005-05",
      "2005-06",
      "2005-07",
      "2005-08",
      "2005-09",
      "2005-10",
      "2005-11",
      "2005-12",
      "2006-01",
      "2006-02",
      "2006-03",
      "2006-04",
      "2006-05",
      "2006-06",
      "2006-07",
      "2006-08",
      "2006-09",
      "2006-10",
      "2006-11",
      "2006-12",
      "2007-01",
      "2007-02",
      "2007-03",
      "2007-04",
      "2007-05",
      "2007-06",
      "2007-07",
      "2007-08",
      "2007-09",
      "2007-10",
      "2007-11",
      "2007-12",
      "2008-01",
      "2008-02",
      "2008-03",
      "2008-04",
      "2008-05",
      "2008-06",
      "2008-07",
      "2008-08",
      "2008-09",
      "2008-10",
      "2008-11",
      "2008-12",
      "2009-01",
      "2009-02",
      "2009-03",
      "2009-04",
      "2009-05",
      "2009-06",
      "2009-07",
      "2009-08",
      "2009-09",
      "2009-10",
      "2009-11",
      "2009-12",
      "2010-01",
      "2010-02",
      "2010-03",
      "2010-04",
      "2010-05",
      "2010-06",
      "2010-07",
      "2010-08",
      "2010-09",
      "2010-10",
      "2010-11",
      "2010-12",
      "2011-01",
      "2011-02",
      "2011-03",
      "2011-04",
      "2011-05",
      "2011-06",
      "2011-07",
      "2011-08",
      "2011-09",
      "2011-10",
      "2011-11",
      "2011-12",
      "2012-01",
      "2012-02",
      "2012-03",
      "2012-04",
      "2012-05",
      "2012-06",
      "2012-07",
      "2012-08",
      "2012-09",
      "2012-10",
      "2012-11",
      "2012-12",
      "2013-01",
      "2013-02",
      "2013-03",
      "2013-04",
      "2013-05",
      "2013-06",
      "2013-07",
      "2013-08",
      "2013-09",
      "2013-10",
      "2013-11",
      "2013-12",
      "2014-01",
      "2014-02",
      "2014-03",
      "2014-04",
      "2014-05",
      "2014-06",
      "2014-07",
      "2014-08",
      "2014-09",
      "2014-10",
      "2014-11",
      "2014-12",
      "2015-01",
      "2015-02",
      "2015-03",
      "2015-04",
      "2015-05",
      "2015-06",
      "2015-07",
      "2015-08",
      "2015-09",
      "2015-10",
      "2015-11",
      "2015-12",
      "2016-01",
      "2016-02",
      "2016-03",
      "2016-04",
      "2016-05",
      "2016-06",
      "2016-07",
      "2016-08",
      "2016-09",
      "2016-10",
      "2016-11",
      "2016-12",
      "2017-01",
      "2017-02",
      "2017-03",
      "2017-04",
      "2017-05",
      "2017-06",
      "2017-07",
      "2017-08",
      "2017-09",
      "2017-10",
      "2017-11",
      "2017-12",
      "2018-01",
      "2018-02",
      "2018-03",
      "2018-04",
      "2018-05",
      "2018-06",
      "2018-07",
      "2018-08",
      "2018-09",
      "2018-10",
      "2018-11",
      "2018-12",
      "2019-01",
      "2019-02",
      "2019-03",
      "2019-04",
      "2019-05",
      "2019-06",
      "2019-07"
    ]
  },
  "series": [
    {
      "label": "Exchange rate",
      "type": "float",
      "raw": [
        0.014398412304639048,
        0.014082171127026009,
        0.013851342751686396,
        0.013680981595092026,
        0.013550937141399972,
        0.013449974079834112,
        0.01342279268767017,
        0.013721532091097308,
        0.01377360965372508,
        0.014120284885254552,
        0.013917609046849758,
        0.013794514940646744,
        0.013812508516146614,
        0.013669724770642202,
        0.013575953822328594,
        0.01352663905156406,
        0.013062283737024222,
        0.013135898145585386,
        0.01279199564388783,
        0.01248584449661649,
        0.01204225352112676,
        0.011742413840450364,
        0.011435438643343572,
        0.011608020698576972,
        0.01174931129476584,
        0.011627349564778604,
        0.011408680381334671,
        0.01070956897586745,
        0.009920562868815251,
        0.009547896150402864,
        0.009792922971896689,
        0.010152198421645997,
        0.009994515138218517,
        0.009757647565704438,
        0.009319135543432649,
        0.009557673771018528,
        0.009748372144354927,
        0.009871780324520595,
        0.009955666704558372,
        0.010302395906024657,
        0.010882981248516495,
        0.011203095684803002,
        0.011689443920829407,
        0.011610068867252434,
        0.01142857142857143,
        0.01139753717472119,
        0.011619865398004177,
        0.011991285916156382,
        0.012577856719952634,
        0.012870967741935482,
        0.012818171035464357,
        0.013010314224034541,
        0.013716248223590714,
        0.013522318840579708,
        0.01297284964636094,
        0.012545331681495662,
        0.01263596441842135,
        0.013112033195020746,
        0.013207674943566592,
        0.013699821587867972,
        0.014383624130459575,
        0.014584246338369277,
        0.014057090450533072,
        0.01368306884347528,
        0.013648971240195523,
        0.013864077669902913,
        0.01398472238057234,
        0.013982544786403308,
        0.013939532230462064,
        0.014261246860013703,
        0.014906483075157772,
        0.015963805214906538,
        0.015975401850949828,
        0.016118404756006937,
        0.016678458622867973,
        0.016030231693718252,
        0.015412821758135018,
        0.01534047919293821,
        0.01535331632653061,
        0.015684573178512184,
        0.016094550229809587,
        0.016393778141629144,
        0.016149630035626198,
        0.01573248407643312,
        0.016228211316706893,
        0.015590962517957425,
        0.014353952710771436,
        0.013346747879051556,
        0.013927363943723415,
        0.013403263403263402,
        0.013446411533976466,
        0.014221802841918294,
        0.014250363901018921,
        0.014614671456715727,
        0.014426027550677567,
        0.014426247406922152,
        0.01428147659854977,
        0.014856818181818181,
        0.014930657345811253,
        0.015296514259846083,
        0.015872885338345865,
        0.015925706147638264,
        0.016493506493506494,
        0.015399050418268145,
        0.015685743311886217,
        0.016485515643105447,
        0.01643608685918961,
        0.016042721867430082,
        0.015574603174603175,
        0.015039771568427495,
        0.01385349750178444,
        0.013502786112301756,
        0.01324450876894262,
        0.0126159961064244,
        0.012757867486449316,
        0.012267551404931598,
        0.01094114063808726,
        0.004850713661520537,
        0.0052405844824037874,
        0.004637586206896552,
        0.00806469298245614,
        0.008777289578470411,
        0.00871743486973948,
        0.007890643694663795,
        0.007918552036199095,
        0.007893669745438162,
        0.00785240510562399,
        0.007868960953011251,
        0.008025351336456325,
        0.008065762970221569,
        0.008077339688041595,
        0.007989284933304178,
        0.007938591611970186,
        0.007795181409124566,
        0.007836557897776495,
        0.007838390925568614,
        0.007708116066498988,
        0.00777891048104492,
        0.00810022201078338,
        0.008368380062305296,
        0.008561226495446505,
        0.008950859792619307,
        0.008885203252032521,
        0.008634315198223498,
        0.008560261421157175,
        0.008577263872305662,
        0.008674556946337836,
        0.008855233306763136,
        0.008732882965126894,
        0.008688930490971677,
        0.00861092665258074,
        0.008735611182167001,
        0.008563432835820894,
        0.008621752531924263,
        0.008530614813416399,
        0.008266842303349644,
        0.008079258749139173,
        0.008101948290650656,
        0.007913794137042143,
        0.007880021553014429,
        0.007860479409956976,
        0.007837076894200087,
        0.007936446425111411,
        0.00831823975313611,
        0.008139284583729029,
        0.008057384175878772,
        0.00785836804704729,
        0.00791732045866023,
        0.007770760233918129,
        0.007828762306610408,
        0.00797392053143068,
        0.008418535513475087,
        0.0082561689137624,
        0.008212840151939722,
        0.00815868263473054,
        0.008354255586241527,
        0.008261946026244119,
        0.008278688524590164,
        0.008221924319054292,
        0.00852132819301082,
        0.008627028397565923,
        0.008762430230320138,
        0.008858625993335043,
        0.008900631524681015,
        0.008869082219208162,
        0.00879399585921325,
        0.008748949919224557,
        0.008623235332210854,
        0.008398541761604062,
        0.008278677815521296,
        0.008089771031977689,
        0.007998832381940841,
        0.0076003924133420525,
        0.0075676756900920124,
        0.007314077473343231,
        0.007344644317252658,
        0.007543978349120432,
        0.007561024949426837,
        0.007456937474569374,
        0.007587357809413527,
        0.007799402238131647,
        0.007911971830985916,
        0.0076255415867604245,
        0.007697806086341117,
        0.007673827020915772,
        0.007794406970207982,
        0.007875133025895709,
        0.008077936881099949,
        0.008093738819320215,
        0.008104655359076144,
        0.00819683056872038,
        0.00848558238098842,
        0.008710379117464263,
        0.00875983157225709,
        0.008899052327976928,
        0.008878315789473684,
        0.008740118577075098,
        0.008967812605325243,
        0.009123121584699453,
        0.009055822987923318,
        0.009722173377879374,
        0.009863843991567112,
        0.00956380857427717,
        0.009429004951285737,
        0.009404104183109708,
        0.009475296203755944,
        0.00959222031543679,
        0.009543621996452185,
        0.009728091858703453,
        0.009902959339161119,
        0.010043966780654617,
        0.01003761242845462,
        0.009616543189774486,
        0.009360371914075023,
        0.009393135600032153,
        0.00929946050406635,
        0.009045697881914812,
        0.008540194838997546,
        0.008132064673057663,
        0.00826244737988097,
        0.008353578223327968,
        0.008340191036002939,
        0.008342190729258932,
        0.008303531845721884,
        0.008126271432723048,
        0.00801945746342849,
        0.008020304568527918
      ]
    }
  ]
}'

usd_isk
parsed_data <- fromJSON(usd_isk)

cat("Name:", parsed_data$name, "\n")
cat("Longname:", parsed_data$longname, "\n")
cat("Number of Observations:", parsed_data$n_obs, "\n")
cat("Number of Dimensions:", parsed_data$n_dim, "\n")

cat("Time Format:", parsed_data$time$format, "\n")
cat("First 5 Time Indices:", head(parsed_data$time$index), "\n")
cat("First 5 Time Values:", head(parsed_data$time$raw), "\n")
cat("Series Label:", parsed_data$series$label, "\n")
cat("First 5 Series Values:", head(parsed_data$series$raw[[1]], 5), "\n")


# Check for non-numeric values
usd_isk_list <- fromJSON(usd_isk)

date_raw <- as.numeric(as.Date(paste0(usd_isk_list$time$raw,"-15")))
series_raw <- usd_isk_list$series$raw[[1]]

# Creating a data frame

usd_isk_df <- data.frame(
  index = 1:length(date_raw),
  time = as.numeric(date_raw),
  value = as.numeric(series_raw)
  
)

# Print the data frame
print(usd_isk_df)
plot(usd_isk_df$index, usd_isk_df$value, type = "l", xlab = "Index", ylab = "Value", main = "Time Series")


tsdata <- ts(usd_isk_df$value, start = min(usd_isk_df$index), end = max(usd_isk_df$index), frequency = 1)
print(tsdata)

###usd_isk annotations
# 6 <- 115,120
# 7 <- 35,86,106,120
# 8 <- 120
# 9 <- 30,40,120
# 13 <- 117,123

#ORTALAMADA DEGISIM NOKTASI ARAYAN ALGORITMALAR

## AMOC ##

# default settings
d.m_amoc <- cpt.mean(tsdata, method = "AMOC")
plot(d.m_amoc)
abline(v = cpts(d.m_amoc), col = "red", lty = 2,lwd=2)
cpts(d.m_amoc)

# oracle settings
#1
o.m_amoc1 <- cpt.mean(tsdata, penalty = "BIC", method = "AMOC", pen.value = 10)
plot(o.m_amoc1)
abline(v = cpts(o.m_amoc1), col = "red", lty = 2,lwd=2)
cpts(o.m_amoc1)

#2
o.m_amoc2 <- cpt.mean(tsdata, penalty = "AIC", method = "AMOC", pen.value = 10)
plot(o.m_amoc2)
abline(v = cpts(o.m_amoc2), col = "red", lty = 2,lwd=2)
cpts(o.m_amoc2)

#3
o.m_amoc3 <- cpt.mean(tsdata, penalty = "SIC", method = "AMOC", pen.value = 10)
plot(o.m_amoc3)
abline(v = cpts(o.m_amoc3), col = "red", lty = 2,lwd=2)
cpts(o.m_amoc3)

#4 en iyii
o.m_amoc4 <- cpt.mean(tsdata, penalty = "None", method = "AMOC", pen.value = 0.05)
plot(o.m_amoc4)
abline(v = cpts(o.m_amoc4), col = "red", lty = 2,lwd=2)
cpts(o.m_amoc4)

#5
o.m_amoc5 <- cpt.mean(tsdata, penalty = "Manual", method = "AMOC", pen.value = 0.05)
plot(o.m_amoc5)
abline(v = cpts(o.m_amoc5), col = "red", lty = 2,lwd=2)
cpts(o.m_amoc5)

#6
o.m_amoc6 <- cpt.mean(tsdata, penalty = " Hannan-Quinn", method = "AMOC", pen.value = 10)
plot(o.m_amoc6)
abline(v = cpts(o.m_amoc6), col = "red", lty = 2,lwd=2)
cpts(o.m_amoc6)

# BINSEG #
# default settings
d.m_binseg <-cpt.mean(tsdata,method = "BinSeg",Q=5)
plot(d.m_binseg)
abline(v = cpts(d.m_binseg), col = "red", lty = 2,lwd=2)
cpts(d.m_binseg)

# oracle settings
#1
o.m_binseg1 <- cpt.mean(tsdata, penalty = "None",pen.value = 10, method = "BinSeg", Q = 2)
plot(o.m_binseg1)
abline(v = cpts(o.m_binseg1), col = "red", lty = 2,lwd=2)
cpts(o.m_binseg1)

#2 bu iyi gibi
o.m_binseg2 <- cpt.mean(tsdata, penalty = "None",pen.value = 10, method = "BinSeg", Q = 3)
plot(o.m_binseg2)
abline(v = cpts(o.m_binseg2), col = "red", lty = 2,lwd=2)
cpts(o.m_binseg2)


# PELT #
# default settings
d.m_pelt <- cpt.mean(tsdata, method = "PELT",minseglen = 20)
plot(d.m_pelt)
abline(v = cpts(d.m_pelt), col = "red", lty = 2,lwd=1)
cpts(d.m_pelt)

# oracle settings
#1
o.m_pelt1 <- cpt.mean(tsdata, penalty = "None", pen.value = 10, method = "PELT", minseglen = 40)
plot(o.m_pelt1, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(o.m_pelt1), col = "blue", lty = 2,lwd=1)
cpts(o.m_pelt1)   

#2
o.m_pelt2 <- cpt.mean(tsdata, penalty = "None", pen.value = 10, method = "PELT", minseglen = 45)
plot(o.m_pelt2, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(o.m_pelt2), col = "blue", lty = 2,lwd=1)
cpts(o.m_pelt2)   

#3 bu iyi
o.m_pelt3 <- cpt.mean(tsdata, penalty = "None", pen.value = 10, method = "PELT", minseglen = 60)
plot(o.m_pelt3, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(o.m_pelt3), col = "blue", lty = 2,lwd=1)
cpts(o.m_pelt3)  

#4 denenecekk ama daha kotu oldu gibi
o.m_pelt4 <- cpt.meanvar(tsdata, penalty = "None", pen.value = 10, method = "PELT", minseglen = 60)
plot(o.m_pelt4, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(o.m_pelt4), col = "blue", lty = 2,lwd=1)
cpts(o.m_pelt4)  

#VARYANSTA DEGiSiM NOKTALARI ARAYAN ALGORiTMA
v_pelt <- cpt.var(tsdata, method = "PELT")
plot(v_pelt, type = "l", cpt.col = "blue", xlab = "Index")
cpts(v_pelt)  

#HEM ORTALAMADA HEM DE VARYANSTA DEGiSiM NOKTASI ARAMA
mv_pelt <- cpt.meanvar(tsdata, method = "PELT")
mv_pelt 
plot(mv_pelt)
cpts(mv_pelt)   

# SEGMENTED REGRESSION #

#default
attach(usd_isk_df)
lm1<-lm(value~time)

seg_get1<-segmented(lm1,seg.Z = ~ time,npsi=5)
seg_get1
seg_get1$psi[,2]
cp <- round(seg_get1$psi[,2])
cp_closest1 <- sapply(cp, function(bp) {
  d<- abs( time - bp)
  indd <- which.min(d)
  e <- c(time[indd])
  c(index_pos=indd, closest_time=e)
})
cp_closest1
detach(usd_isk_df)

# oracle 1
attach(usd_isk_df)
lm2<-lm(value~time)

seg_get2<-segmented(lm2,seg.Z = ~ time, npsi=2)
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
detach(usd_isk_df)

#2
attach(usd_isk_df)
lm3<-lm(value~time)

seg_get3<-segmented(lm3,seg.Z = ~ time, npsi=3)
seg_get3
seg_get2$psi[,2]
cp <- round(seg_get3$psi[,2])
cp_closest3 <- sapply(cp, function(bp) {
  d<- abs( time - bp)
  indd <- which.min(d)
  e <- c(time[indd])
  c(index_pos=indd, closest_time=e)
})
cp_closest3
detach(usd_isk_df)

#3 en iyi
attach(usd_isk_df)
lm4<-lm(value~time)

seg_get4<-segmented(lm4,seg.Z = ~ time, npsi=4)
seg_get4
seg_get4$psi[,2]
cp <- round(seg_get4$psi[,2])
cp_closest4 <- sapply(cp, function(bp) {
  d<- abs( time - bp)
  indd <- which.min(d)
  e <- c(time[indd])
  c(index_pos=indd, closest_time=e)
})
cp_closest4
detach(usd_isk_df)

## PROPHET ##

df_prophet <- data.frame(
  ds = as.Date(date_raw), # Tarih sC<tunu
  y = as.numeric(series_raw) # DeDer sC<tunu
)

model <- prophet(df_prophet,n.changepoints = 4)

changepoints <- model$changepoints
print(changepoints)
changepoints <- as.numeric(as.Date(changepoints))
indeksler <- which(date_raw %in% changepoints)
print(indeksler)

# Time series grafiDini C'izme model

changepoints_df <- data.frame(ds = as.Date(changepoints), y = max(df_prophet$y))

p <- ggplot(df_prophet, aes(x = ds, y = y)) +
  geom_line() +
  geom_vline(data = changepoints_df, aes(xintercept = as.numeric(ds)), color = "red", linetype = "dashed") +
  labs(title = "Time Series with Changepoints",
       x = "Date",
       y = "Value") +
  theme_minimal()

print(p)

## 

model2 <- prophet(df_prophet, n.changepoints = 2)
changepoints2 <- model2$changepoints
print(changepoints2)
changepoints2 <- as.numeric(as.Date(changepoints2))
indeksler2 <- which(date_raw %in% changepoints2)
print(indeksler2)


# Time series grafiDini C'izme model2

changepoints_df2 <- data.frame(ds = as.Date(changepoints2), y = max(df_prophet$y))

p2 <- ggplot(df_prophet, aes(x = ds, y = y)) +
  geom_line() +
  geom_vline(data = changepoints_df2, aes(xintercept = as.numeric(ds)), color = "red", linetype = "dashed") +
  labs(title = "Time Series with Changepoints",
       x = "Date",
       y = "Value") +
  theme_minimal()

print(p2)

##

model3 <- prophet(df_prophet,n.changepoints = 3)
changepoints3 <- model3$changepoints
print(changepoints3)
changepoints3 <- as.numeric(as.Date(changepoints3))
indeksler3 <- which(date_raw %in% changepoints3)
print(indeksler3)


# Time series grafiDini C'izme model3

changepoints_df3 <- data.frame(ds = as.Date(changepoints3), y = max(df_prophet$y))

p3 <- ggplot(df_prophet, aes(x = ds, y = y)) +
  geom_line() +
  geom_vline(data = changepoints_df3, aes(xintercept = as.numeric(ds)), color = "red", linetype = "dashed") +
  labs(title = "Time Series with Changepoints",
       x = "Date",
       y = "Value") +
  theme_minimal()

print(p3)

## bu iyiii

model4 <- prophet(df_prophet,n.changepoints = 5)
changepoints4 <- model4$changepoints
print(changepoints4)
changepoints4 <- as.numeric(as.Date(changepoints4))
indeksler4 <- which(date_raw %in% changepoints4)
print(indeksler4)



# Time series grafiDini C'izme model3

changepoints_df4 <- data.frame(ds = as.Date(changepoints4), y = max(df_prophet$y))

p4 <- ggplot(df_prophet, aes(x = ds, y = y)) +
  geom_line() +
  geom_vline(data = changepoints_df4, aes(xintercept = as.numeric(ds)), color = "red", linetype = "dashed") +
  labs(title = "Time Series with Changepoints",
       x = "Date",
       y = "Value") +
  theme_minimal()

print(p4)

                             #### WELL LOG DATA ####

well_log<-'{
  "name": "well_log",
  "longname": "Well Log",
  "n_obs": 675,
  "n_dim": 1,
  "time": {
    "index": [
      0,
      1,
      2,
      3,
      4,
      5,
      6,
      7,
      8,
      9,
      10,
      11,
      12,
      13,
      14,
      15,
      16,
      17,
      18,
      19,
      20,
      21,
      22,
      23,
      24,
      25,
      26,
      27,
      28,
      29,
      30,
      31,
      32,
      33,
      34,
      35,
      36,
      37,
      38,
      39,
      40,
      41,
      42,
      43,
      44,
      45,
      46,
      47,
      48,
      49,
      50,
      51,
      52,
      53,
      54,
      55,
      56,
      57,
      58,
      59,
      60,
      61,
      62,
      63,
      64,
      65,
      66,
      67,
      68,
      69,
      70,
      71,
      72,
      73,
      74,
      75,
      76,
      77,
      78,
      79,
      80,
      81,
      82,
      83,
      84,
      85,
      86,
      87,
      88,
      89,
      90,
      91,
      92,
      93,
      94,
      95,
      96,
      97,
      98,
      99,
      100,
      101,
      102,
      103,
      104,
      105,
      106,
      107,
      108,
      109,
      110,
      111,
      112,
      113,
      114,
      115,
      116,
      117,
      118,
      119,
      120,
      121,
      122,
      123,
      124,
      125,
      126,
      127,
      128,
      129,
      130,
      131,
      132,
      133,
      134,
      135,
      136,
      137,
      138,
      139,
      140,
      141,
      142,
      143,
      144,
      145,
      146,
      147,
      148,
      149,
      150,
      151,
      152,
      153,
      154,
      155,
      156,
      157,
      158,
      159,
      160,
      161,
      162,
      163,
      164,
      165,
      166,
      167,
      168,
      169,
      170,
      171,
      172,
      173,
      174,
      175,
      176,
      177,
      178,
      179,
      180,
      181,
      182,
      183,
      184,
      185,
      186,
      187,
      188,
      189,
      190,
      191,
      192,
      193,
      194,
      195,
      196,
      197,
      198,
      199,
      200,
      201,
      202,
      203,
      204,
      205,
      206,
      207,
      208,
      209,
      210,
      211,
      212,
      213,
      214,
      215,
      216,
      217,
      218,
      219,
      220,
      221,
      222,
      223,
      224,
      225,
      226,
      227,
      228,
      229,
      230,
      231,
      232,
      233,
      234,
      235,
      236,
      237,
      238,
      239,
      240,
      241,
      242,
      243,
      244,
      245,
      246,
      247,
      248,
      249,
      250,
      251,
      252,
      253,
      254,
      255,
      256,
      257,
      258,
      259,
      260,
      261,
      262,
      263,
      264,
      265,
      266,
      267,
      268,
      269,
      270,
      271,
      272,
      273,
      274,
      275,
      276,
      277,
      278,
      279,
      280,
      281,
      282,
      283,
      284,
      285,
      286,
      287,
      288,
      289,
      290,
      291,
      292,
      293,
      294,
      295,
      296,
      297,
      298,
      299,
      300,
      301,
      302,
      303,
      304,
      305,
      306,
      307,
      308,
      309,
      310,
      311,
      312,
      313,
      314,
      315,
      316,
      317,
      318,
      319,
      320,
      321,
      322,
      323,
      324,
      325,
      326,
      327,
      328,
      329,
      330,
      331,
      332,
      333,
      334,
      335,
      336,
      337,
      338,
      339,
      340,
      341,
      342,
      343,
      344,
      345,
      346,
      347,
      348,
      349,
      350,
      351,
      352,
      353,
      354,
      355,
      356,
      357,
      358,
      359,
      360,
      361,
      362,
      363,
      364,
      365,
      366,
      367,
      368,
      369,
      370,
      371,
      372,
      373,
      374,
      375,
      376,
      377,
      378,
      379,
      380,
      381,
      382,
      383,
      384,
      385,
      386,
      387,
      388,
      389,
      390,
      391,
      392,
      393,
      394,
      395,
      396,
      397,
      398,
      399,
      400,
      401,
      402,
      403,
      404,
      405,
      406,
      407,
      408,
      409,
      410,
      411,
      412,
      413,
      414,
      415,
      416,
      417,
      418,
      419,
      420,
      421,
      422,
      423,
      424,
      425,
      426,
      427,
      428,
      429,
      430,
      431,
      432,
      433,
      434,
      435,
      436,
      437,
      438,
      439,
      440,
      441,
      442,
      443,
      444,
      445,
      446,
      447,
      448,
      449,
      450,
      451,
      452,
      453,
      454,
      455,
      456,
      457,
      458,
      459,
      460,
      461,
      462,
      463,
      464,
      465,
      466,
      467,
      468,
      469,
      470,
      471,
      472,
      473,
      474,
      475,
      476,
      477,
      478,
      479,
      480,
      481,
      482,
      483,
      484,
      485,
      486,
      487,
      488,
      489,
      490,
      491,
      492,
      493,
      494,
      495,
      496,
      497,
      498,
      499,
      500,
      501,
      502,
      503,
      504,
      505,
      506,
      507,
      508,
      509,
      510,
      511,
      512,
      513,
      514,
      515,
      516,
      517,
      518,
      519,
      520,
      521,
      522,
      523,
      524,
      525,
      526,
      527,
      528,
      529,
      530,
      531,
      532,
      533,
      534,
      535,
      536,
      537,
      538,
      539,
      540,
      541,
      542,
      543,
      544,
      545,
      546,
      547,
      548,
      549,
      550,
      551,
      552,
      553,
      554,
      555,
      556,
      557,
      558,
      559,
      560,
      561,
      562,
      563,
      564,
      565,
      566,
      567,
      568,
      569,
      570,
      571,
      572,
      573,
      574,
      575,
      576,
      577,
      578,
      579,
      580,
      581,
      582,
      583,
      584,
      585,
      586,
      587,
      588,
      589,
      590,
      591,
      592,
      593,
      594,
      595,
      596,
      597,
      598,
      599,
      600,
      601,
      602,
      603,
      604,
      605,
      606,
      607,
      608,
      609,
      610,
      611,
      612,
      613,
      614,
      615,
      616,
      617,
      618,
      619,
      620,
      621,
      622,
      623,
      624,
      625,
      626,
      627,
      628,
      629,
      630,
      631,
      632,
      633,
      634,
      635,
      636,
      637,
      638,
      639,
      640,
      641,
      642,
      643,
      644,
      645,
      646,
      647,
      648,
      649,
      650,
      651,
      652,
      653,
      654,
      655,
      656,
      657,
      658,
      659,
      660,
      661,
      662,
      663,
      664,
      665,
      666,
      667,
      668,
      669,
      670,
      671,
      672,
      673,
      674
    ]
  },
  "series": [
    {
      "label": "V1",
      "type": "float",
      "raw": [
        133530.6,
        121415.7,
        99749.55,
        102195.2,
        110237.3,
        112865.1,
        109105.7,
        113924.7,
        108863.5,
        111484.0,
        109094.0,
        109901.9,
        113883.1,
        108888.4,
        115038.8,
        110599.4,
        111085.6,
        111144.4,
        111452.6,
        111403.5,
        116044.4,
        112870.5,
        112939.9,
        111222.6,
        110730.3,
        115567.7,
        112578.5,
        112287.6,
        114286.4,
        109835.9,
        113874.4,
        111091.8,
        112729.2,
        113090.6,
        116651.8,
        112286.8,
        109256.2,
        111292.4,
        113110.6,
        111750.0,
        114852.8,
        115527.2,
        114005.1,
        110319.8,
        115906.3,
        114136.3,
        108217.0,
        113623.3,
        109375.5,
        108955.2,
        111543.6,
        106985.0,
        112035.3,
        111763.8,
        111231.6,
        113001.3,
        111084.7,
        111743.8,
        114086.8,
        109420.7,
        110649.7,
        107673.7,
        110970.8,
        109565.5,
        111754.2,
        107962.5,
        111990.0,
        112608.0,
        111111.0,
        112367.6,
        108693.4,
        111071.3,
        109565.5,
        111975.7,
        106123.5,
        113704.8,
        111654.4,
        114257.7,
        110075.7,
        113317.5,
        112442.6,
        114809.1,
        112856.4,
        109403.5,
        112488.1,
        111072.0,
        105201.7,
        113484.8,
        108528.5,
        114559.3,
        113212.2,
        111140.4,
        113493.7,
        109775.3,
        112554.5,
        107234.5,
        112005.8,
        109311.9,
        112689.0,
        115299.9,
        112730.4,
        113217.7,
        113727.4,
        107535.9,
        113294.4,
        112519.6,
        109506.4,
        112508.0,
        113623.9,
        112710.3,
        114569.5,
        110875.4,
        115205.1,
        117231.5,
        116483.5,
        113756.3,
        110879.7,
        112925.9,
        113135.2,
        113133.4,
        112366.0,
        110439.7,
        108413.9,
        115856.8,
        109716.1,
        110731.8,
        110871.7,
        113192.1,
        110871.7,
        108246.7,
        112116.4,
        109338.8,
        115396.5,
        113574.0,
        113374.0,
        111684.2,
        113565.1,
        115458.6,
        114701.9,
        111684.0,
        114068.3,
        114944.8,
        112057.4,
        116171.5,
        116559.2,
        113122.5,
        113301.0,
        110497.4,
        115409.0,
        118280.9,
        110243.2,
        114676.0,
        114676.0,
        111041.0,
        114782.7,
        114380.0,
        113877.7,
        111290.6,
        118370.5,
        110527.9,
        112519.6,
        112779.8,
        116042.6,
        108632.2,
        110215.5,
        114217.4,
        114422.8,
        112839.8,
        108786.8,
        113026.7,
        113873.1,
        108227.1,
        110788.4,
        106759.2,
        105148.3,
        103986.6,
        106738.8,
        102410.8,
        109164.3,
        125406.3,
        131636.1,
        128111.7,
        131241.2,
        128905.9,
        125881.8,
        126784.4,
        128542.3,
        124188.2,
        130857.8,
        123989.0,
        127843.5,
        127552.7,
        125366.4,
        128273.6,
        128019.5,
        130421.7,
        129362.6,
        123335.7,
        125364.5,
        122354.6,
        127849.7,
        127400.1,
        90702.05,
        84798.66,
        129261.4,
        127893.9,
        130990.6,
        125308.0,
        126856.6,
        128558.1,
        132020.1,
        125792.8,
        128034.5,
        126946.4,
        130183.5,
        125221.2,
        127936.9,
        131090.9,
        132045.0,
        126164.1,
        127696.9,
        128799.0,
        127327.4,
        129451.0,
        129125.1,
        127567.4,
        126632.7,
        125259.5,
        125643.8,
        126295.0,
        124446.2,
        127823.4,
        124761.1,
        126881.2,
        124332.4,
        124088.9,
        121660.5,
        126318.1,
        86079.19,
        123023.6,
        127240.8,
        125736.2,
        123709.5,
        127006.6,
        123191.6,
        130923.3,
        125320.9,
        128493.1,
        128646.3,
        125184.6,
        127159.3,
        126509.3,
        128776.6,
        121142.4,
        125841.6,
        136284.4,
        136391.6,
        133729.3,
        133288.7,
        138664.6,
        137905.3,
        135262.3,
        132531.5,
        133551.1,
        134225.1,
        128222.4,
        136084.1,
        135415.6,
        135744.6,
        136250.4,
        133644.3,
        135149.0,
        134702.4,
        137905.3,
        135830.5,
        132356.1,
        137078.3,
        133805.1,
        134243.2,
        136295.3,
        136073.3,
        120658.8,
        110069.3,
        110323.1,
        114690.0,
        116286.1,
        114809.1,
        118353.3,
        111573.3,
        115755.5,
        116785.4,
        118690.3,
        114214.5,
        118581.7,
        112891.7,
        113920.6,
        118243.4,
        114420.1,
        113074.4,
        117012.5,
        112728.1,
        117012.5,
        117129.4,
        115399.7,
        119073.1,
        113615.5,
        118119.2,
        115290.8,
        116953.0,
        113604.3,
        114875.3,
        122709.1,
        129022.0,
        131367.8,
        131903.9,
        126696.7,
        125753.3,
        126895.9,
        128265.5,
        126657.8,
        130009.1,
        129191.2,
        130659.1,
        129986.2,
        129544.0,
        128070.8,
        131819.4,
        131314.7,
        129435.2,
        129921.2,
        132022.6,
        128957.1,
        127484.9,
        132964.9,
        126984.2,
        128645.4,
        130311.7,
        130622.4,
        122005.1,
        126793.5,
        129790.9,
        124681.9,
        125246.3,
        115856.2,
        116054.0,
        121879.5,
        119255.2,
        120866.7,
        121093.7,
        119558.7,
        119522.7,
        118239.9,
        118655.2,
        122286.6,
        119527.8,
        113522.3,
        122357.0,
        120150.9,
        121462.7,
        120773.2,
        118294.2,
        119486.3,
        121205.1,
        121369.2,
        118702.7,
        121538.6,
        122296.7,
        118727.4,
        121828.8,
        121007.6,
        122900.9,
        117693.2,
        119699.3,
        119622.0,
        119647.7,
        117405.8,
        120517.8,
        116868.7,
        118851.4,
        120486.3,
        119611.8,
        120836.2,
        119871.2,
        120154.3,
        115919.1,
        116985.3,
        118190.2,
        118024.2,
        116171.5,
        113464.7,
        119424.1,
        116148.3,
        116332.5,
        119121.8,
        120685.0,
        118705.1,
        116875.1,
        120995.4,
        116379.6,
        122207.1,
        115926.6,
        114353.5,
        133621.0,
        137957.1,
        130508.4,
        136489.5,
        137816.8,
        136749.5,
        135903.0,
        137080.0,
        134576.1,
        138404.8,
        123406.9,
        118621.9,
        118350.3,
        119851.3,
        116926.9,
        122174.2,
        118905.0,
        118309.0,
        117839.4,
        118497.0,
        130342.2,
        130268.3,
        129262.3,
        128776.2,
        126527.9,
        127340.3,
        129609.0,
        132753.0,
        129367.5,
        129755.9,
        113969.1,
        113412.1,
        114895.5,
        115685.7,
        118499.7,
        113885.8,
        117454.6,
        111995.4,
        114205.9,
        114646.4,
        113684.7,
        117811.1,
        115542.6,
        111644.4,
        116514.2,
        115037.9,
        118125.4,
        112366.6,
        116841.3,
        116710.9,
        114951.7,
        121538.4,
        115034.0,
        117131.0,
        122691.6,
        116772.0,
        115693.4,
        117324.2,
        118270.0,
        114121.4,
        86441.49,
        81135.48,
        107827.2,
        110698.5,
        106343.7,
        105257.5,
        109621.9,
        111779.5,
        115979.9,
        113283.3,
        110409.1,
        113184.5,
        114966.1,
        111874.8,
        110760.7,
        111694.2,
        112518.5,
        112337.2,
        110687.8,
        109689.7,
        115114.9,
        109641.6,
        110326.7,
        106796.3,
        109864.2,
        110974.6,
        110490.6,
        115409.0,
        109895.1,
        112850.5,
        106927.3,
        108100.8,
        107635.1,
        110922.2,
        110785.6,
        110740.9,
        111785.0,
        110912.1,
        109517.8,
        107688.0,
        108836.1,
        108211.3,
        111263.1,
        107564.1,
        112147.6,
        110459.5,
        110649.7,
        107746.5,
        109599.3,
        110448.7,
        108666.0,
        106906.8,
        110776.5,
        108185.2,
        106757.4,
        110237.3,
        110730.3,
        111945.9,
        112379.0,
        114501.0,
        119043.6,
        101972.0,
        110610.1,
        106082.9,
        111622.9,
        112815.0,
        112045.7,
        110678.6,
        112126.5,
        110517.2,
        112538.6,
        109280.8,
        110818.8,
        111664.1,
        116155.6,
        110769.9,
        114386.4,
        110240.8,
        113063.0,
        114931.6,
        113516.9,
        113225.1,
        112267.6,
        114705.8,
        113103.2,
        111373.9,
        110950.9,
        109105.0,
        113252.3,
        112667.9,
        114542.2,
        113805.3,
        112165.7,
        110271.6,
        111634.6,
        112039.9,
        109500.0,
        109500.0,
        114205.9,
        113001.3,
        109895.1,
        112568.0,
        112810.4,
        104370.9,
        110097.7,
        112266.9,
        109868.3,
        117039.8,
        113644.5,
        113392.1,
        110854.2,
        112818.5,
        109395.3,
        109633.1,
        107564.1,
        109835.9,
        108068.2,
        109868.3,
        111734.0,
        111308.1,
        110713.9,
        114492.7,
        111753.9,
        113756.3,
        112701.5,
        110154.9,
        112035.6,
        110326.7,
        110990.6,
        113392.1,
        109403.5,
        105107.8,
        113510.7,
        109876.8,
        113778.6,
        105758.6,
        107353.9,
        107367.9,
        111514.1,
        110140.9,
        111884.9,
        109319.8,
        107534.4,
        110370.2,
        108697.6,
        109176.3,
        111528.4,
        108173.0,
        113272.3,
        112460.8,
        98821.82,
        111553.8,
        111728.4,
        111593.2,
        115007.9,
        116717.3,
        111563.2,
        112628.9,
        108943.5,
        113039.8,
        108638.8,
        109017.9,
        104991.2,
        106448.9,
        110029.9,
        111073.1,
        108401.6,
        109446.6,
        107483.9,
        107112.7,
        108027.6,
        108642.1,
        106455.6,
        113780.5,
        108584.6,
        106502.3,
        108442.0,
        108330.8,
        105500.2,
        105776.6,
        107889.7,
        111024.3,
        114835.1,
        114701.9,
        113524.9,
        116090.3,
        109315.2,
        112025.5,
        111518.3,
        108535.2,
        112717.7,
        108134.4,
        107688.0,
        111920.3,
        112698.7,
        101178.5,
        74658.69,
        67629.86,
        69435.12,
        110645.4,
        107200.8,
        105555.7,
        110764.9,
        111432.7,
        108239.2,
        117362.1,
        113811.7,
        113723.7,
        111724.2,
        111714.0,
        108851.8,
        103871.4,
        101699.6
      ]
    }
  ]
}'

well_log
parsed_data <- fromJSON(well_log)

cat("Name:", parsed_data$name, "\n")
cat("Longname:", parsed_data$longname, "\n")
cat("Number of Observations:", parsed_data$n_obs, "\n")
cat("Number of Dimensions:", parsed_data$n_dim, "\n")

cat("Time Format:", parsed_data$time$format, "\n")
cat("First 5 Time Indices:", head(parsed_data$time$index), "\n")
cat("First 5 Time Values:", head(parsed_data$time$raw), "\n")
cat("Series Label:", parsed_data$series$label, "\n")
cat("First 5 Series Values:", head(parsed_data$series$raw[[1]], 5), "\n")

well_log_list <- fromJSON(well_log)

index <- as.numeric(well_log_list$time$index)
series_raw <- well_log_list$series$raw[[1]]

# Creating a data frame

well_log_df <- data.frame(
  index = 1:length(index),
  time = as.numeric(index),
  value = as.numeric(series_raw)
  
)

# Print the data frame
print(well_log_df)
plot(well_log_df$index, well_log_df$value, type = "l", xlab = "Index", ylab = "Value", main = "Time Series")


tsdata <- ts(well_log_df$value, start = min(well_log_df$index), end = max(well_log_df$index), frequency = 1)
print(tsdata)

###well_log annotations
# 6 -> 179,255,281,311,343,402,413,422,432,462,464
# 7 -> 179,255,281,312,343,402,412,422,432
# 8 -> 179,255,282,312,343,402,413,422,432
# 12 -> 177,467
# 13 -> 4,179,255,281,311,344,402,412,422,432,462,464,521,526,620,643,661

#ORTALAMADA DEGISIM NOKTASI ARAYAN ALGORITMALAR

## AMOC ##

# default settings
d.m_amoc <- cpt.mean(tsdata, method = "AMOC")
plot(d.m_amoc)
abline(v = cpts(d.m_amoc), col = "red", lty = 2,lwd=2)
cpts(d.m_amoc)

# oracle settings
#1
o.m_amoc1 <- cpt.mean(tsdata, method = "AMOC", penalty = "BIC", pen.value = 10)
plot(o.m_amoc1)
abline(v = cpts(o.m_amoc1), col = "red", lty = 2,lwd=2)
cpts(o.m_amoc1)

#2
o.m_amoc2 <- cpt.mean(tsdata, method = "AMOC", penalty = "SIC" )
plot(o.m_amoc2)
abline(v = cpts(o.m_amoc2), col = "red", lty = 2,lwd=2)
cpts(o.m_amoc2)

#3
o.m_amoc3 <- cpt.mean(tsdata, method = "AMOC", penalty = "AIC" )
plot(o.m_amoc3)
abline(v = cpts(o.m_amoc3), col = "red", lty = 2,lwd=2)
cpts(o.m_amoc3)

#4
o.m_amoc4 <- cpt.mean(tsdata, method = "AMOC", penalty = "None" )
plot(o.m_amoc4)
abline(v = cpts(o.m_amoc4), col = "red", lty = 2,lwd=2)
cpts(o.m_amoc4)

#5
o.m_amoc5 <- cpt.mean(tsdata, method = "AMOC", penalty = "Manual" )
plot(o.m_amoc5)
abline(v = cpts(o.m_amoc5), col = "red", lty = 2,lwd=2)
cpts(o.m_amoc5)

#6
o.m_amoc6 <- cpt.mean(tsdata, method = "AMOC", penalty = "Hannan-Quinn" )
plot(o.m_amoc6)
abline(v = cpts(o.m_amoc6), col = "red", lty = 2,lwd=2)
cpts(o.m_amoc6)

# BINSEG #
# default settings
d.m_binseg <-cpt.mean(tsdata,method = "BinSeg",Q=5)
plot(d.m_binseg)
abline(v = cpts(d.m_binseg), col = "red", lty = 2,lwd=2)
cpts(d.m_binseg)

# oracle settings
#1
o.m_binseg1 <- cpt.mean(tsdata, penalty = "BIC",pen.value = 10, method = "BinSeg", Q = 2)
plot(o.m_binseg1)
abline(v = cpts(o.m_binseg1), col = "red", lty = 2,lwd=2)
cpts(o.m_binseg1)

#2
o.m_binseg2 <- cpt.mean(tsdata,method = "BinSeg", penalty = "BIC", pen.value = 100, Q = 9)
plot(o.m_binseg2)
abline(v = cpts(o.m_binseg2), col = "red", lty = 2,lwd=2)
cpts(o.m_binseg2)

#3 bu iyi
o.m_binseg3 <- cpt.mean(tsdata,method = "BinSeg", penalty = "BIC", pen.value = 0.05, Q = 11)
plot(o.m_binseg3)
abline(v = cpts(o.m_binseg3), col = "red", lty = 2,lwd=2)
cpts(o.m_binseg3)

#4
o.m_binseg4 <- cpt.mean(tsdata,method = "BinSeg", penalty = "SIC", pen.value = 100, Q = 17)
plot(o.m_binseg4)
abline(v = cpts(o.m_binseg4), col = "red", lty = 2,lwd=2)
cpts(o.m_binseg4)

# PELT #
# default settings
d.m_pelt <- cpt.mean(tsdata, method = "PELT",minseglen = 60)
plot(d.m_pelt)
abline(v = cpts(d.m_pelt), col = "red", lty = 2,lwd=1)
cpts(d.m_pelt)

# oracle settings
#1
o.m_pelt1 <- cpt.mean(tsdata, method = "PELT", penalty = "BIC", pen.value = 0.05, minseglen = 135)
plot(o.m_pelt1, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(o.m_pelt1), col = "blue", lty = 2,lwd=1)
cpts(o.m_pelt1)   

#2
o.m_pelt2 <- cpt.mean(tsdata, method = "PELT", penalty = "AIC", pen.value = 0.05, minseglen = 55)
plot(o.m_pelt2, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(o.m_pelt2), col = "blue", lty = 2,lwd=1)
cpts(o.m_pelt2)

#3
o.m_pelt3 <- cpt.mean(tsdata, method = "PELT", penalty = "AIC", pen.value = 0.05, minseglen = 50)
plot(o.m_pelt3, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(o.m_pelt3), col = "blue", lty = 2,lwd=1)
cpts(o.m_pelt3)  

#4 en iyi
o.m_pelt4 <- cpt.mean(tsdata, method = "PELT", penalty = "AIC", pen.value = 0.05, minseglen = 32)
plot(o.m_pelt4, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(o.m_pelt4), col = "blue", lty = 2,lwd=1)
cpts(o.m_pelt4) 

#5 denenecekkk hangisi yuksekse o alinacak
o.m_pelt5 <- cpt.meanvar(tsdata, method = "PELT", penalty = "AIC", pen.value = 0.05, minseglen = 32)
plot(o.m_pelt5, type = "l", cpt.col = "blue", xlab = "Index")
abline(v = cpts(o.m_pelt5), col = "blue", lty = 2,lwd=1)
cpts(o.m_pelt5) 

#VARYANSTA DEGiSiM NOKTALARI ARAYAN ALGORiTMA
v_pelt <- cpt.var(tsdata, method = "PELT")
plot(v_pelt, type = "l", cpt.col = "blue", xlab = "Index")
cpts(v_pelt)  

#HEM ORTALAMADA HEM DE VARYANSTA DEGiSiM NOKTASI ARAMA
mv_pelt <- cpt.meanvar(tsdata, method = "PELT")
mv_pelt 
plot(mv_pelt)
cpts(mv_pelt)   

# SEGMENTED REGRESSION #
#default
attach(well_log_df)
lm1<-lm(value~time)

seg_get<-segmented(lm1,seg.Z = ~ time,npsi = 3)
seg_get
seg_get$psi[,2]
cp <- round(seg_get$psi[,2])
cp_closest <- sapply(cp, function(bp) {
  d<- abs( time - bp)
  indd <- which.min(d)
  e <- c(time[indd])
  c(index_pos=indd, closest_time=e)
})
cp_closest
detach(well_log_df)

#oracle 1
attach(well_log_df)
lm2<-lm(value~time)

seg_get2<-segmented(lm2,seg.Z = ~ time, npsi=2)
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
detach(well_log_df)

#
attach(well_log_df)
lm3<-lm(value~time)

seg_get3<-segmented(lm3,seg.Z = ~ time, npsi=9)
seg_get3
seg_get3$psi[,2]
cp <- round(seg_get3$psi[,2])
cp_closest3 <- sapply(cp, function(bp) {
  d<- abs( time - bp)
  indd <- which.min(d)
  e <- c(time[indd])
  c(index_pos=indd, closest_time=e)
})
cp_closest3
detach(well_log_df)

#
attach(well_log_df)
lm4<-lm(value~time)

seg_get4<-segmented(lm4,seg.Z = ~ time, npsi=11)
seg_get4
seg_get4$psi[,2]
cp <- round(seg_get4$psi[,2])
cp_closest4 <- sapply(cp, function(bp) {
  d<- abs( time - bp)
  indd <- which.min(d)
  e <- c(time[indd])
  c(index_pos=indd, closest_time=e)
})
cp_closest4
detach(well_log_df)

# en iyi
attach(well_log_df)
lm5<-lm(value~time)
seg_get5<-segmented(lm5,seg.Z = ~ time, npsi=20)
seg_get5
seg_get5$psi[,2]
cp <- round(seg_get5$psi[,2])
cp_closest5 <- sapply(cp, function(bp) {
  d<- abs( time - bp)
  indd <- which.min(d)
  e <- c(time[indd])
  c(index_pos=indd, closest_time=e)
})
cp_closest5
detach(well_log_df)
## PROPHET ##

df_prophet <- data.frame(
  ds = as.Date(index), # Tarih sC<tunu
  y = as.numeric(series_raw) # DeDer sC<tunu
)

model <- prophet(df_prophet,n.changepoints = 5)

changepoints <- model$changepoints
changepoints <- as.numeric(as.Date(changepoints))
print(changepoints)



# Time series grafiDini C'izme model 
changepoints_df <- data.frame(ds = as.Date(changepoints), y = max(df_prophet$y))

p <- ggplot(df_prophet, aes(x = ds, y = y)) +
  geom_line() +
  geom_vline(data = changepoints_df, aes(xintercept = as.numeric(ds)), color = "red", linetype = "dashed") +
  labs(title = "Time Series with Changepoints",
       x = "Date",
       y = "Value") +
  theme_minimal()

print(p)

##

model2 <- prophet(df_prophet, n.changepoints = 2)
changepoints2 <- model2$changepoints
changepoints2 <- as.numeric(as.Date(changepoints2))
print(changepoints2)




# Time series grafiDini C'izme model2

changepoints_df2 <- data.frame(ds = as.Date(changepoints2), y = max(df_prophet$y))

p2 <- ggplot(df_prophet, aes(x = ds, y = y)) +
  geom_line() +
  geom_vline(data = changepoints_df2, aes(xintercept = as.numeric(ds)), color = "red", linetype = "dashed") +
  labs(title = "Time Series with Changepoints",
       x = "Date",
       y = "Value") +
  theme_minimal()

print(p2)

##

model3 <- prophet(df_prophet,n.changepoints = 9)
changepoints3 <- model3$changepoints
changepoints3 <- as.numeric(as.Date(changepoints3))
print(changepoints3)




# Time series grafiDini C'izme model3

changepoints_df3 <- data.frame(ds = as.Date(changepoints3), y = max(df_prophet$y))

p3 <- ggplot(df_prophet, aes(x = ds, y = y)) +
  geom_line() +
  geom_vline(data = changepoints_df3, aes(xintercept = as.numeric(ds)), color = "red", linetype = "dashed") +
  labs(title = "Time Series with Changepoints",
       x = "Date",
       y = "Value") +
  theme_minimal()

print(p3)

##

model4 <- prophet(df_prophet,n.changepoints = 11)
changepoints4 <- model4$changepoints
changepoints4 <- as.numeric(as.Date(changepoints4))
print(changepoints4)



# Time series grafiDini C'izme model3

changepoints_df4 <- data.frame(ds = as.Date(changepoints4), y = max(df_prophet$y))

p4 <- ggplot(df_prophet, aes(x = ds, y = y)) +
  geom_line() +
  geom_vline(data = changepoints_df4, aes(xintercept = as.numeric(ds)), color = "red", linetype = "dashed") +
  labs(title = "Time Series with Changepoints",
       x = "Date",
       y = "Value") +
  theme_minimal()

print(p4)

## en iyi

model5 <- prophet(df_prophet,n.changepoints = 17)
changepoints5 <- model5$changepoints
changepoints5 <- as.numeric(as.Date(changepoints5))
print(changepoints5)



# Time series grafiDini C'izme model3

changepoints_df5 <- data.frame(ds = as.Date(changepoints5), y = max(df_prophet$y))

p5 <- ggplot(df_prophet, aes(x = ds, y = y)) +
  geom_line() +
  geom_vline(data = changepoints_df5, aes(xintercept = as.numeric(ds)), color = "red", linetype = "dashed") +
  labs(title = "Time Series with Changepoints",
       x = "Date",
       y = "Value") +
  theme_minimal()

print(p5)

                        #### FRIEDMAN-NEMENYI TEST ####

# REAL DATA COVER ORACLE #

if(!require('PMCMRplus')) {
  install.packages('PMCMRplus')
  library('PMCMRplus')
}
real <- matrix(c(0.7640, 0.4251, 0.7938, 0.5264, 0.5844, 0.7665, 0.3870, 0.4305, 0.9105,
                 0.8577, 0.4527, 0.4352, 0.4858, 0.7625, 0.7001, 0.8136, 0.7731, 0.3941,
                 0.8570, 0.7961, 0.7549, 0.6371, 0.2182, 0.3257, 0.7635, 0.7494, 0.7042,
                 0.5113, 0.4840, 0.4157, 0.3955, 0.5609, 0.4873, 0.3833, 0.5273, 0.7062,
                 0.6622, 0.8217, 0.6964, 0.4488, 0.7944, 0.8058, 0.7819, 0.5511, 0.2045,
                 0.3206, 0.3485, 0.4366, 0.6919, 0.6222, 0.3470, 0.4097, 0.3625, 0.4211,
                 0.4615),nrow=11, ncol=5, 
               dimnames=list(c("Bitcoin", "Brent-spot", "childeren-per women", "co2- canada", "debt -Ireland", "rail-lines", "rather-stock", "scanline-42049", "shangai-license", "usd-isk", "well-log"),
                             c("AMOC","BinSeg","PELT","ParC'alD1 Reg","Prophet")))
print(real)
result_real <-friedman.test(real)
result_real
frdAllPairsNemenyiTest(real, result_real)
