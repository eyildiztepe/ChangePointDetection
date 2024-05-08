
# REAL DATA COVER ORACLE #

if(!require('PMCMRplus')) {
  install.packages('PMCMRplus')
  library('PMCMRplus')
}

# Define the data matrix
real <- matrix(c(0.7640, 0.4251, 0.7938, 0.5264, 0.5844, 0.7665, 0.3870, 0.4305, 0.9105,
                 0.8577, 0.4527, 0.4352, 0.4858, 0.7625, 0.7001, 0.8136, 0.7731, 0.3941,
                 0.8570, 0.7961, 0.7549, 0.6371, 0.2182, 0.3257, 0.7635, 0.7494, 0.7042,
                 0.5113, 0.4840, 0.4157, 0.3955, 0.5609, 0.4873, 0.3833, 0.5273, 0.7062,
                 0.6622, 0.8217, 0.6964, 0.4488, 0.7944, 0.8058, 0.7819, 0.5511, 0.2045,
                 0.3206, 0.3485, 0.4366, 0.6919, 0.6222, 0.3470, 0.4097, 0.3625, 0.4211,
                 0.4615),nrow=11, ncol=5, 
               dimnames=list(c("Bitcoin", "Brent-spot", "childeren-per women", "co2- canada", "debt -Ireland", "rail-lines", "rather-stock", "scanline-42049", "shangai-license", "usd-isk", "well-log"),
                             c("AMOC","BinSeg","PELT","Parçalı Reg","Prophet")))

# Perform Friedman test
result_real <- friedman.test(real)
print(result_real)
#Friedman Test Hipotezi 
#H0:Algoritmalar arasında istatistiksel olarak anlamlı bir fark yoktur.
#H1:Algoritmalar arasında en az birinde istatistiksel olarak anlamlı bir fark vardır.
#p-value 0.05(alpha)'den küçüktür. Bu nedenle H0 hipotezi reddedilmiştir, algoritmalar arasında en az bir tanesi farklıdır.



# Perform post-hoc Nemenyi test
nemenyi_result <- frdAllPairsNemenyiTest(real, result_real)
print(nemenyi_result)
#Prophet ile AMOC, BinSeg ve parçalı regresyon algoritmaları arasında istatistiksel olarak anlamlı bir fark vardır.

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
            dimnames=list(1:20,c("AMOC","BinSeg","Parçalı Reg","Prophet","PELT")))

result_sim<-friedman.test(sim)
result_sim

#Friedman Test Hipotezi 
#H0:Algoritmalar arasında istatistiksel olarak anlamlı bir fark yoktur.
#H1:Algoritmalar arasında en az birinde istatistiksel olarak anlamlı bir fark vardır.
#p-value 0.05(alpha)'den küçüktür. Bu nedenle H0 hipotezi reddedilmiştir, algoritmalar arasında en az bir tanesi farklıdır.

frdAllPairsNemenyiTest(sim, result_sim)
#AMOC ve BinSeg algoritmaları arasında istatistiksel olarak anlamlı fark vardır.
#Parçalı Regresyon ve AMOC algoritmaları arasında istatistiksel olarak anlamlı fark vardır.
#Prophet ile BinSeg ve Parçalı Regresyon algoritmaları arasında istatistiksel olarak anlamlı fark vardır. 
#PELT ile BinSeg ve Parçalı Regresyon algoritmaları arasında istatistiksel olarak anlamlı fark vardır.






