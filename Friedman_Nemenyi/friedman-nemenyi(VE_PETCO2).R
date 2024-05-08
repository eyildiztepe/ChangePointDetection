
# VE_PETCO2 COVER ORACLE #

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

#Friedman Test Hipotezi 
#H0:Algoritmalar arasında istatistiksel olarak anlamlı bir fark yoktur.
#H1:Algoritmalar arasında en az birinde istatistiksel olarak anlamlı bir fark vardır.
#p-value 0.05(alpha)'den büyüktür. Bu nedenle H0 hipotezi reddedilememiştir, algoritmalar arasında istatistiksel olarak anlamlı bir fark yoktur.

