#Kaplan-Meier

## Libraries 
#library(HSAUR)
#library(HSAUR2)
library(stats4)
library(tidyverse)
#library(stringr)
#library(bbmle)
#library(orderstats)
library(NADA)
#(EstimationTools)
library(ggplot2) 
library(directlabels)
library(data.table)
library(reshape2)

## working directories
setwd("C:/Users/Martin/sciebo/Masterarbeit") ## Laptop # Broken
setwd("C:/Users/klaus/Desktop/Martin Studium Arbeitsordner/Masterarbeit") ## Laptop Eltern
setwd("C:/Users/marti/sciebo/Hennig Masterarbeit Projektordner") ## Desktop


###########################
# specify which variables should have missing data and prc of missing data
c_names = c("NaCl","NOx", "O2")
prc_missing = 0.10
set.seed(42)

iterat <- vector("list", 1000)
iterat10 <- vector("list", 1000)
iterat20 <- vector("list", 1000)
iterat30 <- vector("list", 1000)
iterat40 <- vector("list", 1000)
iterat50 <- vector("list", 1000)
iterat60 <- vector("list", 1000)
iterat70 <- vector("list", 1000)
iterat80 <- vector("list", 1000)
iterat90 <- vector("list", 1000)


#censored$p10 <- ifelse(xd$ranks>0.1, xd$y, NA)

for(i in seq(1, 1000, by = 1)){                                                                                           # i = 1000 iteration for list of dataframes
  N = 1000
  df = data.frame(NaCl = rnorm(N, mean = 65, sd = 10), NOx = rnorm(N,22:120), O2 = rnorm(N, mean = 4, sd = 0.2))
  df_ranks = data.frame(rankNaCl = percent_rank(df$NaCl), rankNOx = percent_rank(df$NOx), rankO2 = percent_rank(df$O2))
  df_combined = cbind(df, df_ranks)
  df_NaCl_cen = data.frame(matrix(ncol= 9, nrow = 1000))
  colnames(df_NaCl_cen) <- c("NaCl10", "NaCl20", "NaCl30", "NaCl40", "NaCl50", "NaCl60", "NaCl70", "NaCl80", "NaCl90")
  df_NOx_cen = data.frame(matrix(ncol= 9, nrow = 1000))
  colnames(df_NOx_cen) <- c("NOx10", "NOx20", "NOx30", "NOx40", "NOx50", "NOx60", "NOx70", "NOx80", "NOx90")
  df_O2_cen = data.frame(matrix(ncol= 9, nrow = 1000))
  colnames(df_O2_cen) <- c("O210", "O220", "O230", "O240", "O250", "O260", "O270", "O280", "O290")
  for(j in seq(1,3, by = 1)){                                                                                             # j for columns in df
    for(k in seq(1, 9, by =1)){                                                                                           # k for censormentlevel
      if(j == 1){
      df_NaCl_cen[k] = as.numeric(ifelse(df_combined$rankNaCl > (k/10), df_combined$NaCl, NA))
      }else if(j == 2){
        df_NOx_cen[k] = as.numeric(ifelse(df_combined$rankNOx > (k/10), df_combined$NOx, NA))
      }else if(j == 3){
        df_O2_cen[k] = as.numeric(ifelse(df_combined$rankO2 > (k/10), df_combined$O2, NA))
      }
      
      if(k == 1){
      iterat10[[i]] = cbind(df_NaCl_cen[k], df_NOx_cen[k], df_O2_cen[k])
      } else if (k == 2){
        iterat20[[i]] = cbind(df_NaCl_cen[k], df_NOx_cen[k], df_O2_cen[k])
      } else if (k == 3){
        iterat30[[i]] = cbind(df_NaCl_cen[k], df_NOx_cen[k], df_O2_cen[k])
      } else if (k == 4){
        iterat40[[i]] = cbind(df_NaCl_cen[k], df_NOx_cen[k], df_O2_cen[k])
      } else if (k == 5){
        iterat50[[i]] = cbind(df_NaCl_cen[k], df_NOx_cen[k], df_O2_cen[k])
      } else if (k == 6){
        iterat60[[i]] = cbind(df_NaCl_cen[k], df_NOx_cen[k], df_O2_cen[k])
      } else if (k == 7){
        iterat70[[i]] = cbind(df_NaCl_cen[k], df_NOx_cen[k], df_O2_cen[k])
      } else if (k == 8){
        iterat80[[i]] = cbind(df_NaCl_cen[k], df_NOx_cen[k], df_O2_cen[k])
      } else if (k == 9){
        iterat90[[i]] = cbind(df_NaCl_cen[k], df_NOx_cen[k], df_O2_cen[k])  
    }
  }
  iterat[[i]] = df                                                                                                              
}}

######################################### 
means

means <- data.frame(matrix(nrow = 1000, ncol = 3))
colnames(means) <- c("meanNaCl", "meanNOx", "meanO2")

for(i in seq(1, 1000, by = 1)){
  for(j in seq(1,3, by = 1)){
    if(j == 1){
      means$meanNaCl[i] <- apply(iterat[[i]][j], MARGIN = 2, FUN = mean)
    } else if (j == 2){
      means$meanNOx[i] <- apply(iterat[[i]][j], MARGIN = 2, FUN = mean)
    } else if (j == 3){
      means$meanO2[i] <- apply(iterat[[i]][j], MARGIN = 2, FUN = mean)
    }
  }
}

means10 <-data.frame(matrix(nrow = 1000, ncol = 3))
colnames(means10) <- c("meanNaCl", "meanNOx", "meanO2")

for(i in seq(1, 1000, by = 1)){
  for(j in seq(1,3, by = 1)){
    if(j == 1){
      means10$meanNaCl[i] <- apply(iterat10[[i]][j], MARGIN = 2, FUN = mean,  na.rm = TRUE)
    } else if (j == 2){
      means10$meanNOx[i] <- apply(iterat10[[i]][j], MARGIN = 2, FUN = mean,  na.rm = TRUE)
    } else if (j == 3){
      means10$meanO2[i] <- apply(iterat10[[i]][j], MARGIN = 2, FUN = mean,  na.rm = TRUE)
    }
  }
}

means20 <-data.frame(matrix(nrow = 1000, ncol = 3))
colnames(means20) <- c("meanNaCl", "meanNOx", "meanO2")

for(i in seq(1, 1000, by = 1)){
  for(j in seq(1,3, by = 1)){
    if(j == 1){
      means20$meanNaCl[i] <- apply(iterat20[[i]][j], MARGIN = 2, FUN = mean,  na.rm = TRUE)
    } else if (j == 2){
      means20$meanNOx[i] <- apply(iterat20[[i]][j], MARGIN = 2, FUN = mean,  na.rm = TRUE)
    } else if (j == 3){
      means20$meanO2[i] <- apply(iterat20[[i]][j], MARGIN = 2, FUN = mean,  na.rm = TRUE)
    }
  }
}

means30 <-data.frame(matrix(nrow = 1000, ncol = 3))
colnames(means30) <- c("meanNaCl", "meanNOx", "meanO2")

for(i in seq(1, 1000, by = 1)){
  for(j in seq(1,3, by = 1)){
    if(j == 1){
      means30$meanNaCl[i] <- apply(iterat30[[i]][j], MARGIN = 2, FUN = mean,  na.rm = TRUE)
    } else if (j == 2){
      means30$meanNOx[i] <- apply(iterat30[[i]][j], MARGIN = 2, FUN = mean,  na.rm = TRUE)
    } else if (j == 3){
      means30$meanO2[i] <- apply(iterat30[[i]][j], MARGIN = 2, FUN = mean,  na.rm = TRUE)
    }
  }
}

means40 <-data.frame(matrix(nrow = 1000, ncol = 3))
colnames(means40) <- c("meanNaCl", "meanNOx", "meanO2")

for(i in seq(1, 1000, by = 1)){
  for(j in seq(1,3, by = 1)){
    if(j == 1){
      means40$meanNaCl[i] <- apply(iterat40[[i]][j], MARGIN = 2, FUN = mean,  na.rm = TRUE)
    } else if (j == 2){
      means40$meanNOx[i] <- apply(iterat40[[i]][j], MARGIN = 2, FUN = mean,  na.rm = TRUE)
    } else if (j == 3){
      means40$meanO2[i] <- apply(iterat40[[i]][j], MARGIN = 2, FUN = mean,  na.rm = TRUE)
    }
  }
}

means50 <-data.frame(matrix(nrow = 1000, ncol = 3))
colnames(means50) <- c("meanNaCl", "meanNOx", "meanO2")

for(i in seq(1, 1000, by = 1)){
  for(j in seq(1,3, by = 1)){
    if(j == 1){
      means50$meanNaCl[i] <- apply(iterat50[[i]][j], MARGIN = 2, FUN = mean,  na.rm = TRUE)
    } else if (j == 2){
      means50$meanNOx[i] <- apply(iterat50[[i]][j], MARGIN = 2, FUN = mean,  na.rm = TRUE)
    } else if (j == 3){
      means50$meanO2[i] <- apply(iterat50[[i]][j], MARGIN = 2, FUN = mean,  na.rm = TRUE)
    }
  }
}

means60 <-data.frame(matrix(nrow = 1000, ncol = 3))
colnames(means60) <- c("meanNaCl", "meanNOx", "meanO2")

for(i in seq(1, 1000, by = 1)){
  for(j in seq(1,3, by = 1)){
    if(j == 1){
      means60$meanNaCl[i] <- apply(iterat60[[i]][j], MARGIN = 2, FUN = mean,  na.rm = TRUE)
    } else if (j == 2){
      means60$meanNOx[i] <- apply(iterat60[[i]][j], MARGIN = 2, FUN = mean,  na.rm = TRUE)
    } else if (j == 3){
      means60$meanO2[i] <- apply(iterat60[[i]][j], MARGIN = 2, FUN = mean,  na.rm = TRUE)
    }
  }
}

means70 <-data.frame(matrix(nrow = 1000, ncol = 3))
colnames(means70) <- c("meanNaCl", "meanNOx", "meanO2")

for(i in seq(1, 1000, by = 1)){
  for(j in seq(1,3, by = 1)){
    if(j == 1){
      means70$meanNaCl[i] <- apply(iterat70[[i]][j], MARGIN = 2, FUN = mean,  na.rm = TRUE)
    } else if (j == 2){
      means70$meanNOx[i] <- apply(iterat70[[i]][j], MARGIN = 2, FUN = mean,  na.rm = TRUE)
    } else if (j == 3){
      means70$meanO2[i] <- apply(iterat70[[i]][j], MARGIN = 2, FUN = mean,  na.rm = TRUE)
    }
  }
}

means80 <-data.frame(matrix(nrow = 1000, ncol = 3))
colnames(means80) <- c("meanNaCl", "meanNOx", "meanO2")

for(i in seq(1, 1000, by = 1)){
  for(j in seq(1,3, by = 1)){
    if(j == 1){
      means80$meanNaCl[i] <- apply(iterat80[[i]][j], MARGIN = 2, FUN = mean,  na.rm = TRUE)
    } else if (j == 2){
      means80$meanNOx[i] <- apply(iterat80[[i]][j], MARGIN = 2, FUN = mean,  na.rm = TRUE)
    } else if (j == 3){
      means80$meanO2[i] <- apply(iterat80[[i]][j], MARGIN = 2, FUN = mean,  na.rm = TRUE)
    }
  }
}

means90 <-data.frame(matrix(nrow = 1000, ncol = 3))
colnames(means90) <- c("meanNaCl", "meanNOx", "meanO2")

for(i in seq(1, 1000, by = 1)){
  for(j in seq(1,3, by = 1)){
    if(j == 1){
      means90$meanNaCl[i] <- apply(iterat90[[i]][j], MARGIN = 2, FUN = mean,  na.rm = TRUE)
    } else if (j == 2){
      means90$meanNOx[i] <- apply(iterat90[[i]][j], MARGIN = 2, FUN = mean,  na.rm = TRUE)
    } else if (j == 3){
      means90$meanO2[i] <- apply(iterat90[[i]][j], MARGIN = 2, FUN = mean,  na.rm = TRUE)
    }
  }
}

#############################################
#means sum

means_sum <- data.frame(matrix(nrow = 10, ncol = 3))
colnames(means_sum) <- c("meanNaCl", "meanNOx", "meanO2")
rownames(means_sum) <- c("0%","10%","20%","30%","40%","50%","60%","70%","80%","90%")
means_seq = NULL
means_seq <- list(means, means10, means20, means30, means40, means50, means60, means70, means80, means90)
for(i in seq(1,10, by = 1)){
  for(j in seq(1,3, by = 1)){
    if(j == 1){
      means_sum$meanNaCl[i] <- apply(means_seq[[i]][j], MARGIN = 2, FUN = mean, na.rm= TRUE)
    } else if (j == 2){
      means_sum$meanNOx[i] <- apply(means_seq[[i]][j], MARGIN = 2, FUN = mean, na.rm= TRUE)
    } else if (j == 3){
      means_sum$meanO2[i] <- apply(means_seq[[i]][j], MARGIN = 2, FUN = mean, na.rm= TRUE)
    }
  }
}

##################################################################
# create dummies

iterat_seq = NULL
iterat_seq <- list(iterat, iterat10, iterat20, iterat30, iterat40, iterat50, iterat60, iterat70, iterat80, iterat90)

iterat_dummies <- vector("list", 1000)
iterat_dummies10<- vector("list", 1000)
iterat_dummies20 <- vector("list", 1000)
iterat_dummies30 <- vector("list", 1000)
iterat_dummies40 <- vector("list", 1000)
iterat_dummies50 <- vector("list", 1000)
iterat_dummies60 <- vector("list", 1000)
iterat_dummies70 <- vector("list", 1000)
iterat_dummies80 <- vector("list", 1000)
iterat_dummies90 <- vector("list", 1000)

iterat_dummies_seq <- list(iterat_dummies, iterat_dummies10, iterat_dummies20, iterat_dummies30, iterat_dummies40, iterat_dummies50, iterat_dummies60, iterat_dummies70, iterat_dummies80, iterat_dummies90)

for(i in seq(1,10, by = 1)){                                                                          # i for dataset selection
  for(j in seq(1,1000, by = 1)){                                                                      # j for data in list selection
      tmp <- as.data.frame(iterat_seq[[i]][j]) ## i, j
      
      tmp2 <- as.data.frame(apply(tmp, MARGIN = 2, FUN = function(x){is.na(x)}))
      
      if(i == 1){
        iterat_dummies[[j]] = tmp2
      } else if (i == 2){
        iterat_dummies10[[j]] = tmp2
      } else if (i == 3){
        iterat_dummies20[[j]] = tmp2
      } else if (i == 4){
        iterat_dummies30[[j]] = tmp2
      } else if (i == 5){
        iterat_dummies40[[j]] = tmp2
      } else if (i == 6){
        iterat_dummies50[[j]] = tmp2
      } else if (i == 7){
        iterat_dummies60[[j]] = tmp2
      } else if (i == 8){
        iterat_dummies70[[j]] = tmp2
      } else if (i == 9){
        iterat_dummies80[[j]] = tmp2
      } else if (i == 10){
        iterat_dummies90[[j]] = tmp2
      }
  }
}

####################################
# doing replacement analysis 
# WORK IN PROGESS

testdf <- (iterat[[1]][1])
head(testdf)
testcen <- (iterat_dummies10[[1]][1])
head(testcen)
str(testcen)

censt<- censtats(obs = testdf$NaCl, censored = testcen$NaCl10)
censt

###########################
#KM ROS MLE 0

list10 <- NULL
list10 <-  vector("list", 1000)
list20 <-  vector("list", 1000)
list30 <-  vector("list", 1000)
list40 <-  vector("list", 1000)
list50 <-  vector("list", 1000)
list60 <-  vector("list", 1000)
list70 <-  vector("list", 1000)
list80 <-  vector("list", 1000)
list90 <-  vector("list", 1000)


df <- data.frame(matrix(nrow = 3, ncol = 3), row.names = c("K-M", "ROS", "MLE"))
colnames(df) = c("NaCl", "NOx", "O2")                                                           # dataframe skeleton for summary of iteration
df


for(i in seq(1, 1000, by = 1)){                                                                   # i for iteration
  data1 <- iterat[[i]][1]                                                                         # setting data for censtats with 1 = NaCl
  data2 <- iterat[[i]][2]                                                                         # 2 = NOx
  data3 <- iterat[[i]][3]                                                                         # 3 = O2
  cen1 <- iterat_dummies10[[i]][1]                                                                # same for dummies             
  cen2 <- iterat_dummies10[[i]][2]
  cen3 <- iterat_dummies10[[i]][3]
  censtNaCl <- censtats(obs = data1$NaCl, censored = cen1$NaCl10)                                 # doing the KM ROS MLE analysis
  censtNOx <- censtats(obs = data2$NOx, censored = cen2$NOx10)
  censtO2 <- censtats(obs = data3$O2, censored = cen3$O210)
  df[1,1] <- censtNaCl[1,2]                                                                       # putting the output of analysis (mean) into target df
  df[2,1] <- censtNaCl[2,2]
  df[3,1] <- censtNaCl[3,2]
  df[1,2] <- censtNOx[1,2]
  df[2,2] <- censtNOx[2,2]
  df[3,2] <- censtNOx[3,2]
  df[1,3] <- censtO2[1,2]
  df[2,3] <- censtO2[2,2]
  df[3,3] <- censtO2[3,2]
  list10[[i]] <- df
  }

for(i in seq(1, 1000, by = 1)){                                                                   # i for iteration
  data1 <- iterat[[i]][1]                                                                         # setting data for censtats with 1 = NaCl
  data2 <- iterat[[i]][2]                                                                         # 2 = NOx
  data3 <- iterat[[i]][3]                                                                         # 3 = O2
  cen1 <- iterat_dummies20[[i]][1]                                                                # same for dummies             
  cen2 <- iterat_dummies20[[i]][2]
  cen3 <- iterat_dummies20[[i]][3]
  censtNaCl <- censtats(obs = data1$NaCl, censored = cen1$NaCl20)                                 # doing the KM ROS MLE analysis
  censtNOx <- censtats(obs = data2$NOx, censored = cen2$NOx20)
  censtO2 <- censtats(obs = data3$O2, censored = cen3$O220)
  df[1,1] <- censtNaCl[1,2]                                                                       # putting the output of analysis (mean) into target df
  df[2,1] <- censtNaCl[2,2]
  df[3,1] <- censtNaCl[3,2]
  df[1,2] <- censtNOx[1,2]
  df[2,2] <- censtNOx[2,2]
  df[3,2] <- censtNOx[3,2]
  df[1,3] <- censtO2[1,2]
  df[2,3] <- censtO2[2,2]
  df[3,3] <- censtO2[3,2]
  list20[[i]] <- df
}

for(i in seq(1, 1000, by = 1)){                                                                   # i for iteration
  data1 <- iterat[[i]][1]                                                                         # setting data for censtats with 1 = NaCl
  data2 <- iterat[[i]][2]                                                                         # 2 = NOx
  data3 <- iterat[[i]][3]                                                                         # 3 = O2
  cen1 <- iterat_dummies30[[i]][1]                                                                # same for dummies             
  cen2 <- iterat_dummies30[[i]][2]
  cen3 <- iterat_dummies30[[i]][3]
  censtNaCl <- censtats(obs = data1$NaCl, censored = cen1$NaCl30)                                 # doing the KM ROS MLE analysis
  censtNOx <- censtats(obs = data2$NOx, censored = cen2$NOx30)
  censtO2 <- censtats(obs = data3$O2, censored = cen3$O230)
  df[1,1] <- censtNaCl[1,2]                                                                       # putting the output of analysis (mean) into target df
  df[2,1] <- censtNaCl[2,2]
  df[3,1] <- censtNaCl[3,2]
  df[1,2] <- censtNOx[1,2]
  df[2,2] <- censtNOx[2,2]
  df[3,2] <- censtNOx[3,2]
  df[1,3] <- censtO2[1,2]
  df[2,3] <- censtO2[2,2]
  df[3,3] <- censtO2[3,2]
  list30[[i]] <- df
}

for(i in seq(1, 1000, by = 1)){                                                                   # i for iteration
  data1 <- iterat[[i]][1]                                                                         # setting data for censtats with 1 = NaCl
  data2 <- iterat[[i]][2]                                                                         # 2 = NOx
  data3 <- iterat[[i]][3]                                                                         # 3 = O2
  cen1 <- iterat_dummies40[[i]][1]                                                                # same for dummies             
  cen2 <- iterat_dummies40[[i]][2]
  cen3 <- iterat_dummies40[[i]][3]
  censtNaCl <- censtats(obs = data1$NaCl, censored = cen1$NaCl40)                                 # doing the KM ROS MLE analysis
  censtNOx <- censtats(obs = data2$NOx, censored = cen2$NOx40)
  censtO2 <- censtats(obs = data3$O2, censored = cen3$O240)
  df[1,1] <- censtNaCl[1,2]                                                                       # putting the output of analysis (mean) into target df
  df[2,1] <- censtNaCl[2,2]
  df[3,1] <- censtNaCl[3,2]
  df[1,2] <- censtNOx[1,2]
  df[2,2] <- censtNOx[2,2]
  df[3,2] <- censtNOx[3,2]
  df[1,3] <- censtO2[1,2]
  df[2,3] <- censtO2[2,2]
  df[3,3] <- censtO2[3,2]
  list40[[i]] <- df
}

for(i in seq(1, 1000, by = 1)){                                                                   # i for iteration
  data1 <- iterat[[i]][1]                                                                         # setting data for censtats with 1 = NaCl
  data2 <- iterat[[i]][2]                                                                         # 2 = NOx
  data3 <- iterat[[i]][3]                                                                         # 3 = O2
  cen1 <- iterat_dummies50[[i]][1]                                                                # same for dummies             
  cen2 <- iterat_dummies50[[i]][2]
  cen3 <- iterat_dummies50[[i]][3]
  censtNaCl <- censtats(obs = data1$NaCl, censored = cen1$NaCl50)                                 # doing the KM ROS MLE analysis
  censtNOx <- censtats(obs = data2$NOx, censored = cen2$NOx50)
  censtO2 <- censtats(obs = data3$O2, censored = cen3$O250)
  df[1,1] <- censtNaCl[1,2]                                                                       # putting the output of analysis (mean) into target df
  df[2,1] <- censtNaCl[2,2]
  df[3,1] <- censtNaCl[3,2]
  df[1,2] <- censtNOx[1,2]
  df[2,2] <- censtNOx[2,2]
  df[3,2] <- censtNOx[3,2]
  df[1,3] <- censtO2[1,2]
  df[2,3] <- censtO2[2,2]
  df[3,3] <- censtO2[3,2]
  list50[[i]] <- df
}

for(i in seq(1, 1000, by = 1)){                                                                   # i for iteration
  data1 <- iterat[[i]][1]                                                                         # setting data for censtats with 1 = NaCl
  data2 <- iterat[[i]][2]                                                                         # 2 = NOx
  data3 <- iterat[[i]][3]                                                                         # 3 = O2
  cen1 <- iterat_dummies60[[i]][1]                                                                # same for dummies             
  cen2 <- iterat_dummies60[[i]][2]
  cen3 <- iterat_dummies60[[i]][3]
  censtNaCl <- censtats(obs = data1$NaCl, censored = cen1$NaCl60)                                 # doing the KM ROS MLE analysis
  censtNOx <- censtats(obs = data2$NOx, censored = cen2$NOx60)
  censtO2 <- censtats(obs = data3$O2, censored = cen3$O260)
  df[1,1] <- censtNaCl[1,2]                                                                       # putting the output of analysis (mean) into target df
  df[2,1] <- censtNaCl[2,2]
  df[3,1] <- censtNaCl[3,2]
  df[1,2] <- censtNOx[1,2]
  df[2,2] <- censtNOx[2,2]
  df[3,2] <- censtNOx[3,2]
  df[1,3] <- censtO2[1,2]
  df[2,3] <- censtO2[2,2]
  df[3,3] <- censtO2[3,2]
  list60[[i]] <- df
}

for(i in seq(1, 1000, by = 1)){                                                                   # i for iteration
  data1 <- iterat[[i]][1]                                                                         # setting data for censtats with 1 = NaCl
  data2 <- iterat[[i]][2]                                                                         # 2 = NOx
  data3 <- iterat[[i]][3]                                                                         # 3 = O2
  cen1 <- iterat_dummies70[[i]][1]                                                                # same for dummies             
  cen2 <- iterat_dummies70[[i]][2]
  cen3 <- iterat_dummies70[[i]][3]
  censtNaCl <- censtats(obs = data1$NaCl, censored = cen1$NaCl70)                                 # doing the KM ROS MLE analysis
  censtNOx <- censtats(obs = data2$NOx, censored = cen2$NOx70)
  censtO2 <- censtats(obs = data3$O2, censored = cen3$O270)
  df[1,1] <- censtNaCl[1,2]                                                                       # putting the output of analysis (mean) into target df
  df[2,1] <- censtNaCl[2,2]
  df[3,1] <- censtNaCl[3,2]
  df[1,2] <- censtNOx[1,2]
  df[2,2] <- censtNOx[2,2]
  df[3,2] <- censtNOx[3,2]
  df[1,3] <- censtO2[1,2]
  df[2,3] <- censtO2[2,2]
  df[3,3] <- censtO2[3,2]
  list70[[i]] <- df
}

for(i in seq(1, 1000, by = 1)){                                                                   # i for iteration
  data1 <- iterat[[i]][1]                                                                         # setting data for censtats with 1 = NaCl
  data2 <- iterat[[i]][2]                                                                         # 2 = NOx
  data3 <- iterat[[i]][3]                                                                         # 3 = O2
  cen1 <- iterat_dummies80[[i]][1]                                                                # same for dummies             
  cen2 <- iterat_dummies80[[i]][2]
  cen3 <- iterat_dummies80[[i]][3]
  censtNaCl <- censtats(obs = data1$NaCl, censored = cen1$NaCl80)                                 # doing the KM ROS MLE analysis
  censtNOx <- censtats(obs = data2$NOx, censored = cen2$NOx80)
  censtO2 <- censtats(obs = data3$O2, censored = cen3$O280)
  df[1,1] <- censtNaCl[1,2]                                                                       # putting the output of analysis (mean) into target df
  df[2,1] <- censtNaCl[2,2]
  df[3,1] <- censtNaCl[3,2]
  df[1,2] <- censtNOx[1,2]
  df[2,2] <- censtNOx[2,2]
  df[3,2] <- censtNOx[3,2]
  df[1,3] <- censtO2[1,2]
  df[2,3] <- censtO2[2,2]
  df[3,3] <- censtO2[3,2]
  list80[[i]] <- df
}

for(i in seq(1, 1000, by = 1)){                                                                   # i for iteration
  data1 <- iterat[[i]][1]                                                                         # setting data for censtats with 1 = NaCl
  data2 <- iterat[[i]][2]                                                                         # 2 = NOx
  data3 <- iterat[[i]][3]                                                                         # 3 = O2
  cen1 <- iterat_dummies90[[i]][1]                                                                # same for dummies             
  cen2 <- iterat_dummies90[[i]][2]
  cen3 <- iterat_dummies90[[i]][3]
  censtNaCl <- censtats(obs = data1$NaCl, censored = cen1$NaCl90)                                 # doing the KM ROS MLE analysis
  censtNOx <- censtats(obs = data2$NOx, censored = cen2$NOx90)
  censtO2 <- censtats(obs = data3$O2, censored = cen3$O290)
  df[1,1] <- censtNaCl[1,2]                                                                       # putting the output of analysis (mean) into target df
  df[2,1] <- censtNaCl[2,2]
  df[3,1] <- censtNaCl[3,2]
  df[1,2] <- censtNOx[1,2]
  df[2,2] <- censtNOx[2,2]
  df[3,2] <- censtNOx[3,2]
  df[1,3] <- censtO2[1,2]
  df[2,3] <- censtO2[2,2]
  df[3,3] <- censtO2[3,2]
  list90[[i]] <- df
}

###########
# means of km ros mle

#mean10 
means10_ad = NULL
means10_ad <- data.frame(matrix(nrow = 1000, ncol = 9))
colnames(means10_ad) <- c("K_M_NaCl", "ROS_NaCl", "MLE_NaCl", "K_M_NOx", "ROS_NOx", "MLE_NOx", "K_M_O2", "ROS_O2", "MLE_O2")

for(i in seq(1, 1000, by = 1)){
  data = list10
  NaCl <- data.frame(matrix(nrow = 1000, ncol = 3))
  
  means10_ad$K_M_NaCl[i] <- data[[i]][1,1]
  means10_ad$ROS_NaCl[i] <- data[[i]][2,1]
  means10_ad$MLE_NaCl[i] <- data[[i]][3,1]
  
  means10_ad$K_M_NOx[i] <- data[[i]][1,2]
  means10_ad$ROS_NOx[i] <- data[[i]][2,2]
  means10_ad$MLE_NOx[i] <- data[[i]][3,2]  

  means10_ad$K_M_O2[i] <- data[[i]][1,3]
  means10_ad$ROS_O2[i] <- data[[i]][2,3]
  means10_ad$MLE_O2[i] <- data[[i]][3,3]

}

#means20
means20_ad = NULL
means20_ad <- data.frame(matrix(nrow = 1000, ncol = 9))
colnames(means20_ad) <- c("K_M_NaCl", "ROS_NaCl", "MLE_NaCl", "K_M_NOx", "ROS_NOx", "MLE_NOx", "K_M_O2", "ROS_O2", "MLE_O2")

for(i in seq(1, 1000, by = 1)){
  data = list20
  NaCl <- data.frame(matrix(nrow = 1000, ncol = 3))
  
  means20_ad$K_M_NaCl[i] <- data[[i]][1,1]
  means20_ad$ROS_NaCl[i] <- data[[i]][2,1]
  means20_ad$MLE_NaCl[i] <- data[[i]][3,1]
  
  means20_ad$K_M_NOx[i] <- data[[i]][1,2]
  means20_ad$ROS_NOx[i] <- data[[i]][2,2]
  means20_ad$MLE_NOx[i] <- data[[i]][3,2]  
  
  means20_ad$K_M_O2[i] <- data[[i]][1,3]
  means20_ad$ROS_O2[i] <- data[[i]][2,3]
  means20_ad$MLE_O2[i] <- data[[i]][3,3]
  
}

#means30
means30_ad = NULL
means30_ad <- data.frame(matrix(nrow = 1000, ncol = 9))
colnames(means30_ad) <- c("K_M_NaCl", "ROS_NaCl", "MLE_NaCl", "K_M_NOx", "ROS_NOx", "MLE_NOx", "K_M_O2", "ROS_O2", "MLE_O2")

for(i in seq(1, 1000, by = 1)){
  data = list30
  NaCl <- data.frame(matrix(nrow = 1000, ncol = 3))
  
  means30_ad$K_M_NaCl[i] <- data[[i]][1,1]
  means30_ad$ROS_NaCl[i] <- data[[i]][2,1]
  means30_ad$MLE_NaCl[i] <- data[[i]][3,1]
  
  means30_ad$K_M_NOx[i] <- data[[i]][1,2]
  means30_ad$ROS_NOx[i] <- data[[i]][2,2]
  means30_ad$MLE_NOx[i] <- data[[i]][3,2]  
  
  means30_ad$K_M_O2[i] <- data[[i]][1,3]
  means30_ad$ROS_O2[i] <- data[[i]][2,3]
  means30_ad$MLE_O2[i] <- data[[i]][3,3]
  
}

#means40
means40_ad = NULL
means40_ad <- data.frame(matrix(nrow = 1000, ncol = 9))
colnames(means40_ad) <- c("K_M_NaCl", "ROS_NaCl", "MLE_NaCl", "K_M_NOx", "ROS_NOx", "MLE_NOx", "K_M_O2", "ROS_O2", "MLE_O2")

for(i in seq(1, 1000, by = 1)){
  data = list40
  NaCl <- data.frame(matrix(nrow = 1000, ncol = 3))
  
  means40_ad$K_M_NaCl[i] <- data[[i]][1,1]
  means40_ad$ROS_NaCl[i] <- data[[i]][2,1]
  means40_ad$MLE_NaCl[i] <- data[[i]][3,1]
  
  means40_ad$K_M_NOx[i] <- data[[i]][1,2]
  means40_ad$ROS_NOx[i] <- data[[i]][2,2]
  means40_ad$MLE_NOx[i] <- data[[i]][3,2]  
  
  means40_ad$K_M_O2[i] <- data[[i]][1,3]
  means40_ad$ROS_O2[i] <- data[[i]][2,3]
  means40_ad$MLE_O2[i] <- data[[i]][3,3]
  
}

#means50
means50_ad = NULL
means50_ad <- data.frame(matrix(nrow = 1000, ncol = 9))
colnames(means50_ad) <- c("K_M_NaCl", "ROS_NaCl", "MLE_NaCl", "K_M_NOx", "ROS_NOx", "MLE_NOx", "K_M_O2", "ROS_O2", "MLE_O2")

for(i in seq(1, 1000, by = 1)){
  data = list50
  NaCl <- data.frame(matrix(nrow = 1000, ncol = 3))
  
  means50_ad$K_M_NaCl[i] <- data[[i]][1,1]
  means50_ad$ROS_NaCl[i] <- data[[i]][2,1]
  means50_ad$MLE_NaCl[i] <- data[[i]][3,1]
  
  means50_ad$K_M_NOx[i] <- data[[i]][1,2]
  means50_ad$ROS_NOx[i] <- data[[i]][2,2]
  means50_ad$MLE_NOx[i] <- data[[i]][3,2]  
  
  means50_ad$K_M_O2[i] <- data[[i]][1,3]
  means50_ad$ROS_O2[i] <- data[[i]][2,3]
  means50_ad$MLE_O2[i] <- data[[i]][3,3]
  
}

#means60
means60_ad = NULL
means60_ad <- data.frame(matrix(nrow = 1000, ncol = 9))
colnames(means60_ad) <- c("K_M_NaCl", "ROS_NaCl", "MLE_NaCl", "K_M_NOx", "ROS_NOx", "MLE_NOx", "K_M_O2", "ROS_O2", "MLE_O2")

for(i in seq(1, 1000, by = 1)){
  data = list60
  NaCl <- data.frame(matrix(nrow = 1000, ncol = 3))
  
  means60_ad$K_M_NaCl[i] <- data[[i]][1,1]
  means60_ad$ROS_NaCl[i] <- data[[i]][2,1]
  means60_ad$MLE_NaCl[i] <- data[[i]][3,1]
  
  means60_ad$K_M_NOx[i] <- data[[i]][1,2]
  means60_ad$ROS_NOx[i] <- data[[i]][2,2]
  means60_ad$MLE_NOx[i] <- data[[i]][3,2]  
  
  means60_ad$K_M_O2[i] <- data[[i]][1,3]
  means60_ad$ROS_O2[i] <- data[[i]][2,3]
  means60_ad$MLE_O2[i] <- data[[i]][3,3]
  
}

#means70
means70_ad = NULL
means70_ad <- data.frame(matrix(nrow = 1000, ncol = 9))
colnames(means70_ad) <- c("K_M_NaCl", "ROS_NaCl", "MLE_NaCl", "K_M_NOx", "ROS_NOx", "MLE_NOx", "K_M_O2", "ROS_O2", "MLE_O2")

for(i in seq(1, 1000, by = 1)){
  data = list70
  NaCl <- data.frame(matrix(nrow = 1000, ncol = 3))
  
  means70_ad$K_M_NaCl[i] <- data[[i]][1,1]
  means70_ad$ROS_NaCl[i] <- data[[i]][2,1]
  means70_ad$MLE_NaCl[i] <- data[[i]][3,1]
  
  means70_ad$K_M_NOx[i] <- data[[i]][1,2]
  means70_ad$ROS_NOx[i] <- data[[i]][2,2]
  means70_ad$MLE_NOx[i] <- data[[i]][3,2]  
  
  means70_ad$K_M_O2[i] <- data[[i]][1,3]
  means70_ad$ROS_O2[i] <- data[[i]][2,3]
  means70_ad$MLE_O2[i] <- data[[i]][3,3]
  
}

#means80
means80_ad = NULL
means80_ad <- data.frame(matrix(nrow = 1000, ncol = 9))
colnames(means80_ad) <- c("K_M_NaCl", "ROS_NaCl", "MLE_NaCl", "K_M_NOx", "ROS_NOx", "MLE_NOx", "K_M_O2", "ROS_O2", "MLE_O2")

for(i in seq(1, 1000, by = 1)){
  data = list80
  NaCl <- data.frame(matrix(nrow = 1000, ncol = 3))
  
  means80_ad$K_M_NaCl[i] <- data[[i]][1,1]
  means80_ad$ROS_NaCl[i] <- data[[i]][2,1]
  means80_ad$MLE_NaCl[i] <- data[[i]][3,1]
  
  means80_ad$K_M_NOx[i] <- data[[i]][1,2]
  means80_ad$ROS_NOx[i] <- data[[i]][2,2]
  means80_ad$MLE_NOx[i] <- data[[i]][3,2]  
  
  means80_ad$K_M_O2[i] <- data[[i]][1,3]
  means80_ad$ROS_O2[i] <- data[[i]][2,3]
  means80_ad$MLE_O2[i] <- data[[i]][3,3]
  
}

#means90
means90_ad = NULL
means90_ad <- data.frame(matrix(nrow = 1000, ncol = 9))
colnames(means90_ad) <- c("K_M_NaCl", "ROS_NaCl", "MLE_NaCl", "K_M_NOx", "ROS_NOx", "MLE_NOx", "K_M_O2", "ROS_O2", "MLE_O2")

for(i in seq(1, 1000, by = 1)){
  data = list90
  NaCl <- data.frame(matrix(nrow = 1000, ncol = 3))
  
  means90_ad$K_M_NaCl[i] <- data[[i]][1,1]
  means90_ad$ROS_NaCl[i] <- data[[i]][2,1]
  means90_ad$MLE_NaCl[i] <- data[[i]][3,1]
  
  means90_ad$K_M_NOx[i] <- data[[i]][1,2]
  means90_ad$ROS_NOx[i] <- data[[i]][2,2]
  means90_ad$MLE_NOx[i] <- data[[i]][3,2]  
  
  means90_ad$K_M_O2[i] <- data[[i]][1,3]
  means90_ad$ROS_O2[i] <- data[[i]][2,3]
  means90_ad$MLE_O2[i] <- data[[i]][3,3]
  
}

# sum the means of ad

means_sum_k_m <- data.frame(matrix(nrow = 10, ncol = 3))
colnames(means_sum_k_m) <- c("meanNaCl", "meanNOx", "meanO2")
rownames(means_sum_k_m) <- c("0%","10%","20%","30%","40%","50%","60%","70%","80%","90%")
means_sum_k_m_seq = NULL
means_sum_k_m_seq <- list(means10_ad, means20_ad, means30_ad, means40_ad, means50_ad, means60_ad, means70_ad, means80_ad, means90_ad)


for(i in seq(1,9, by = 1)){
  for(j in seq(1,9, by = 1)){
  means_sum_k_m$meanNaCl[i+1] <- apply(means_sum_k_m_seq[[i]][1], 2 , FUN = mean)
  means_sum_k_m$meanNOx[i+1] <- apply(means_sum_k_m_seq[[i]][4], 2, FUN = mean)
  means_sum_k_m$meanO2[i+1] <- apply(means_sum_k_m_seq[[i]][7], 2, FUN = mean)
  }
}


means_sum_ros <- data.frame(matrix(nrow = 10, ncol = 3))
colnames(means_sum_ros) <- c("meanNaCl", "meanNOx", "meanO2")
rownames(means_sum_ros) <- c("0%","10%","20%","30%","40%","50%","60%","70%","80%","90%")
means_sum_ros_seq = NULL
means_sum_ros_seq <- list(means10_ad, means20_ad, means30_ad, means40_ad, means50_ad, means60_ad, means70_ad, means80_ad, means90_ad)


for(i in seq(1,9, by = 1)){
  for(j in seq(1,9, by = 1)){
    means_sum_ros$meanNaCl[i+1] <- apply(means_sum_ros_seq[[i]][2], 2 , FUN = mean)
    means_sum_ros$meanNOx[i+1] <- apply(means_sum_ros_seq[[i]][5], 2, FUN = mean)
    means_sum_ros$meanO2[i+1] <- apply(means_sum_ros_seq[[i]][8], 2, FUN = mean)
  }
}


means_sum_mle <- data.frame(matrix(nrow = 10, ncol = 3))
colnames(means_sum_mle) <- c("meanNaCl", "meanNOx", "meanO2")
rownames(means_sum_mle) <- c("0%","10%","20%","30%","40%","50%","60%","70%","80%","90%")
means_sum_mle_seq = NULL
means_sum_mle_seq <- list(means10_ad, means20_ad, means30_ad, means40_ad, means50_ad, means60_ad, means70_ad, means80_ad, means90_ad)


for(i in seq(1,9, by = 1)){
  for(j in seq(1,9, by = 1)){
    means_sum_mle$meanNaCl[i+1] <- apply(means_sum_mle_seq[[i]][2], 2 , FUN = mean)
    means_sum_mle$meanNOx[i+1] <- apply(means_sum_mle_seq[[i]][5], 2, FUN = mean)
    means_sum_mle$meanO2[i+1] <- apply(means_sum_mle_seq[[i]][8], 2, FUN = mean)
  }
}


means_sum_k_m[1,] <- means_sum[1,]
means_sum_ros[1,] <- means_sum[1,]
means_sum_mle[1,] <- means_sum[1,]

means_sum_k_m
means_sum_ros
means_sum_mle

censorment <- seq(0,90,10)

##########################
means_all = NULL
means_all <- data.frame(matrix(nrow = 10, ncol = 12))
colnames(means_all) <- c("meanNaCl", "meanNOx", "meanO2", "K_M_NaCl", "K_M_NOx", "K_M_O2", "ROS_NaCl", "ROS_NOx", "ROS_O2" , "MLE_NaCl", "MLE_NOx", "MLE_O2")
rownames(means_all) <- c("0%","10%","20%","30%","40%","50%","60%","70%","80%","90%")

for(i in seq(1, 10, by = 1)){
means_all$meanNaCl[i] <- means_sum$meanNaCl[i]
means_all$meanNOx[i]  <- means_sum$meanNOx[i]
means_all$meanO2[i]  <- means_sum$meanO2[i]
means_all$K_M_NaCl[i]  <- means_sum_k_m$meanNaCl[i]
means_all$K_M_NOx[i]   <- means_sum_k_m$meanNOx[i]
means_all$K_M_O2[i]    <- means_sum_k_m$meanO2[i]
means_all$ROS_NaCl[i]  <- means_sum_ros$meanNaCl[i]
means_all$ROS_NOx[i]   <- means_sum_ros$meanNOx[i]
means_all$ROS_O2[i]    <- means_sum_ros$meanO2[i]
means_all$MLE_NaCl[i]  <- means_sum_mle$meanNaCl[i]
means_all$MLE_NOx[i]   <- means_sum_mle$meanNOx[i]
means_all$MLE_O2[i]    <- means_sum_mle$meanO2[i]
}
means_all$censorment <- NULL
means_all$censorment <- seq(0,90,10)

means_all

means_all_t <- transpose(means_all)
colnames(means_all_t) <- rownames(means_all)
rownames(means_all_t) <- colnames(means_all)
means_all_t
means_all_t$group <- c("NaCl", "NOx", "O2","NaCl_K_M", "NOx_K_M", "O2_K_M","NaCl_ROS", "NOx_ROS", "O2_ROS","NaCl_MLE", "NOx_MLE", "O2_MLE")#, "Cen")
means_all_back <- transpose(means_all_t)

colnames(means_all_back) <- c("meanNaCl", "meanNOx", "meanO2", "K_M_NaCl", "K_M_NOx", "K_M_O2", "ROS_NaCl", "ROS_NOx", "ROS_O2" , "MLE_NaCl", "MLE_NOx", "MLE_O2")
rownames(means_all_back) <- c("0%","10%","20%","30%","40%","50%","60%","70%","80%","90%", "group")
means_all_back
##

g <- ggplot(means_all, aes(x=means_all$censorment)) +
        geom_line(aes(y=means_all$meanNaCl), colour = "red1")+
        geom_line(aes(y=means_all$meanNOx), colour = "green1")+
        geom_line(aes(y=means_all$meanO2), colour = "blue1")
g
g <- g+ theme(legend.position = "right")+
  xlab("Censorment [%]")+
  ylab("Mean")
g
g <- g +geom_line(aes(y=means_all$K_M_NaCl), colour = "red2")+
        geom_line(aes(y=means_all$K_M_NOx), colour = "green2")+
        geom_line(aes(y=means_all$K_M_O2), colour = "blue2")
g
g <- g +geom_line(aes(y=means_all$ROS_NaCl), colour = "red3")+
        geom_line(aes(y=means_all$ROS_NOx), colour = "green3")+
        geom_line(aes(y=means_all$ROS_O2), colour = "blue3")
g
g <- g +geom_line(aes(y=means_all$MLE_NaCl), colour = "red4")+
        geom_line(aes(y=means_all$MLE_NOx), colour = "green4")+
        geom_line(aes(y=means_all$MLE_O2), colour = "blue4")
        #+
        #scale_colour_discrete(guide = 'none') +
        #scale_x_discrete(expand=c(0, 1)) +
        #geom_dl(aes(label = means_all), method = list(dl.combine("first.points", "last.points")), cex = 0.8)
g
##
means_all_back = melt(means_all_back, id=c("group"))
means_all_back

ggplot(means_all_re) + geom_line(aes(x = variable, y = value, colour = group))+
  scale_colour_manual(values=c("red", "green", "blue","pink"))

ggplot(data = means_all, aes ( x = censorment)) +
  geom_line(aes(y=meanNaCl), colour = "NaCl")+
  geom_line(aes(y=meanNOx), colour = "NOx")+
  geom_line(aes(y=meanO2), colour = "O2")+
  geom_line(aes(y=K_M_NaCl), colour = "NaCl")+
  geom_line(aes(y=K_M_NOx), colour = "O2")+
  geom_line(aes(y=K_M_O2), colour = "O2")+
  geom_line(aes(y=ROS_NaCl), colour = "NaCl")+
  geom_line(aes(y=ROS_NOx), colour = "NOx")+
  geom_line(aes(y=ROS_O2), colour = "O2")+
  geom_line(aes(y=MLE_NaCl), colour = "NaCl")+
  geom_line(aes(y=MLE_NOx), colour = "NOx")+
  geom_line(aes(y=MLE_O2), colour = "O2")+
  scale_colour_manual("",
                      breaks = c("NaCl","NOx","O2"),
                      values = c("red","gree","blue"))+
  theme(legend.position = "bottom")+
  xlab("Censorment [%]")+
  ylab("Mean")
##

plot(x = means_all$censorment, y = means_all$meanNaCl, type = "o")
plot(x = means_all$censorment, y = means_all$meanNOx, type = "o")
##

ggplot(data = means_all_re, aes(x=variable, y=value)) + geom_line(aes(colour=as.factor(group)))


##

data_long <- gather( means_all_t, group, measurement, group,
                     factor_key = TRUE)
data_long$group = NULL
data_long <- gather(means_all_t, key = analysis, value = measurement, `0%`:`90%`, factor_key = TRUE)
data_long

g <- ggplot(data = data_long, aes(x = analysis, y = measurement)) + 
      geom_line(aes(x = analysis, y = measurement, group = group,  colour=group), size = 1)
g <- g + xlab("Censorment [%]")+ ylab("Mean")
g <- g + ggtitle("Comparision of KM, ROS and MLE for the mean of 3 variables")
g



################### 
#fehlersuche


for(i in seq(1,20, by = 1)){
  print(summary(iterat_dummies10[[i]]))
}
for(i in seq(1,20, by = 1)){
  print(summary(iterat10[[i]]))
}


dataframe6 = NULL
dataframe6 = data.frame(matrix(nrow = 1000, ncol = 3))
colnames(dataframe6) = c("iterat", "iterat10", "iterat_dummies10")  
dataframe6[1] <- iterat[[6]][3]
dataframe6[2] <- iterat10[[6]][3]
dataframe6[3] <- iterat_dummies10[[6]][3]


censto2_6 <- censtats(obs = dataframe6$iterat, censored = dataframe6$iterat_dummies10)
censto2_6

summary(dataframe6)


dataframe7 = NULL
dataframe7 = data.frame(matrix(nrow = 1000, ncol = 3))
colnames(dataframe7) = c("iterat", "iterat10", "iterat_dummies10")  
dataframe7[1] <- iterat[[9]][3]
dataframe7[2] <- iterat10[[9]][3]
dataframe7[3] <- iterat_dummies10[[9]][3]


censto2_7 <- censtats(obs = dataframe7$iterat, censored = dataframe7$iterat_dummies10)
censto2_7

summary(dataframe7)
