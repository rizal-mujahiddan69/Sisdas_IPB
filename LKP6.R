options(digits=13)

# Penerapan Algorithm LVQ
library(Dict)
dataku <- matrix(c(1,1,0,0,
                   1,0,1,1,
                   0,1,1,0,
                   0,0,1,1),
                 nrow=4,
                 ncol=4,
                 byrow = TRUE)

alphaku <- c(0.05, 0.2)
target <- matrix(c(1,2,1,2),ncol = 1)
euclidku <- function(a,b){return ((a-b)^2)}

vec_euclid <- function(a,b){
  sum_euc <- 0
  if(length(a) == length(b)){
    for(i in 1:length(a)){
      sum_euc <- sum_euc + euclidku(a,b)
    }
    return(sum_euc)
  }
  else{
    return(-1)
  }
}


LVQ <- function(trainingku,kelas,alp,epokku=1){
  ukuran <- dim(trainingku)
  kelas_uq <- unique(kelas)
  len_kelas <- length(kelas_uq)
  weight <- matrix(rep(0,ukuran[1]*len_kelas),
                   nrow=ukuran[1],
                   ncol=len_kelas)
  ind_uncount <- c()
  for(i in 1:len_kelas){
    letak <- which(kelas == kelas_uq[i])[1]
    ind_uncount <- c(ind_uncount,letak)
    weight[,i] <- trainingku[letak,]
  }
  ind_count <- (1:ukuran[1])
  ind_count <- setdiff(ind_count,ind_uncount)
  kelas_copy = kelas
  
  for(i in ind_count) {
    dist_euc <- c()
    for(j in 1:ncol(weight)){
      dist_euc <- c(dist_euc, 
                    vec_euclid(trainingku[i,],
                               weight[,j]))
    }
    ind_win <- which.min(dist_euc)
    if(kelas[i] != ind_win){
      weight[,ind_win] = weight[,ind_win] - 
        (alp[1] * (trainingku[i,] - weight[,ind_win]));
    }
    else{
      weight[,ind_win] = weight[,ind_win] +
        (alp[2] * (trainingku[i,] - weight[,ind_win]));
    }
  }
  
  iterasi <- epokku -1
  
  if(iterasi > 0){
    ind_count <- (1:ukuran[1])
    for(i in ind_count) {
      dist_euc <- c()
      for(j in 1:ncol(weight)){
        dist_euc <- c(dist_euc, 
                      vec_euclid(trainingku[i,],
                                 weight[,j]))
      }
      ind_win <- which.min(dist_euc)
      if(kelas[i] != ind_win){
        weight[,ind_win] = weight[,ind_win] - 
          (alp[1] * (trainingku[i,] - weight[,ind_win]));
      }
      else{
        weight[,ind_win] = weight[,ind_win] +
          (alp[2] * (trainingku[i,] - weight[,ind_win]));
      }
    }
  }
  return(weight)
}


hasil_weight <- LVQ(dataku,target,alp = alphaku,epokku = 2)
print(hasil_weight)