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

alphaku <- 0.05
d_alpha <- 0.2

target <- matrix(c(1,2,1,2),ncol = 1)
euclidku <- function(a,b){return ((a-b)^2)}

vec_euclid <- function(a,b){
  sum_euc <- 0
  if(length(a) == length(b)){
    for(i in 1:length(a)){
      sum_euc <- sum_euc + euclidku(a[i],b[i])
    }
    return(sum_euc)
  }
  else{
    return(-1)
  }
}


LVQ <- function(trainingku,kelas,alp,d_alp=0,epokku=1){
  weig <- Dict$new(siapa="rizal")
  weg_ind <- paste("weight_epoch",(1:epokku),sep="")
  
  ukuran <- dim(trainingku)
  kelas_uq <- unique(kelas)
  len_kelas <- length(kelas_uq)
  weight <- matrix(rep(0,ukuran[1]*len_kelas),
                   nrow=ukuran[1],
                   ncol=len_kelas)
  
  
  pan_t <- epokku*ukuran[1] - len_kelas
  
  weg_ind_t <- paste("weight_t",(0:pan_t),sep="")
  
  timeku <- 1
  weig_t <- Dict$new(siapa="rizal")
  
  ind_uncount <- c()
  for(i in 1:len_kelas){
    letak <- which(kelas == kelas_uq[i])[1]
    ind_uncount <- c(ind_uncount,letak)
    weight[,i] <- trainingku[letak,]
  }
  
  weig_t[weg_ind_t[timeku]] <- weight
  weig_t$remove("siapa")
  
  ind_count <- (1:ukuran[1])
  ind_count <- setdiff(ind_count,ind_uncount)
  timeku <- timeku + 1
  for(i in ind_count) {
    dist_euc <- c()
    for(j in 1:ncol(weight)){
      dist_euc <- c(dist_euc, 
                    vec_euclid(trainingku[i,],
                               weight[,j]))
    }
    print(dist_euc)
    ind_win <- which.min(dist_euc)
    if(kelas[i] != ind_win){
      weight[,ind_win] = weight[,ind_win] - 
        (alp * (trainingku[i,] - weight[,ind_win]));
    }
    else{
      weight[,ind_win] = weight[,ind_win] +
        (alp * (trainingku[i,] - weight[,ind_win]));
    }
    weig_t[weg_ind_t[timeku]] <- weight
    timeku <- timeku + 1
  }
  weig[weg_ind[1]] <- weight 
  
  alp = alp - (alp*d_alp)
  
  ## cek ya
  if(epokku > 1){
    ind_count <- (1:ukuran[1])
    for(ulangan in 2:epokku){
      for(i in ind_count) {
        dist_euc <- c()
        for(j in 1:ncol(weight)){
          dist_euc <- c(dist_euc, 
                        vec_euclid(trainingku[i,],
                                   weight[,j]))
        }
        print(dist_euc)
        ind_win <- which.min(dist_euc)
        if(kelas[i] != ind_win){
          weight[,ind_win] = weight[,ind_win] - 
            (alp * (trainingku[i,] - weight[,ind_win]));
        }
        else{
          weight[,ind_win] = weight[,ind_win] +
            (alp * (trainingku[i,] - weight[,ind_win]));
        }
        weig_t[weg_ind_t[timeku]] <- weight
        timeku <- timeku + 1
      }
      alp = alp - (alp*d_alp)
      weig[weg_ind[(ulangan)]] <- weight
    }
  }
  weig$remove("siapa")
  attrib <- list(berat_epok = weig,berat_waktu = weig_t)
  return(attrib)
}

hasil_weight <- LVQ(dataku,target,alp = alphaku,
                      d_alp = d_alpha,epokku = 2)
#print(hasil_weight)

