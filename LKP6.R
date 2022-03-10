options(digits=13)

# Penerapan Algorithm LVQ
library(Dict)
dataku <- matrix(c(1,1,1,0,
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


# Parameter trainingku itu untuk training data
# Parameter kelas itu target class
# Parameter alp itu untuk learning rate (alpha)
# Parameter d_alp itu untuk parameter derivatif learning rate
# Parameter epokku untuk menentukan epoch


LVQ <- function(trainingku,kelas,alp,d_alp=0,epokku=1){
  weig <- Dict$new(siapa="rizal")
  weig_t <- Dict$new(siapa="rizal")
  dist_t <- Dict$new(siapa="rizal")
  
  ukuran <- dim(trainingku)
  kelas_uq <- unique(kelas)
  len_kelas <- length(kelas_uq)
  weight <- matrix(rep(0,ukuran[1]*len_kelas),
                   nrow=ukuran[1],
                   ncol=len_kelas)
  pan_t <- epokku*ukuran[1] - len_kelas
  
  weg_ind    <- paste("weight_epoch",(1:epokku),sep="")
  weg_ind_t  <- paste("weight_t",(1:pan_t),sep="")
  dist_ind_t <- paste("Distance_t",(1:pan_t),sep="")
  
  timeku <- 1
  
  
  ind_uncount <- c()
  for(i in 1:len_kelas){
    letak <- which(kelas == kelas_uq[i])[1]
    ind_uncount <- c(ind_uncount,letak)
    weight[,i] <- trainingku[letak,]
  }
  init_weight <- weight
  weig_t$remove("siapa")
  
  
  ind_count <- (1:ukuran[1])
  ind_count <- setdiff(ind_count,ind_uncount)
  for(i in ind_count) {
    dist_euc <- c()
    for(j in 1:ncol(weight)){
      dist_euc <- c(dist_euc, 
                    vec_euclid(trainingku[i,],
                               weight[,j]))
    }
    #print(dist_euc)
    
    ind_win <- which.min(dist_euc)
    if(kelas[i] != ind_win){
      weight[,ind_win] = weight[,ind_win] - 
        (alp * (trainingku[i,] - weight[,ind_win]));
    }
    else{
      weight[,ind_win] = weight[,ind_win] +
        (alp * (trainingku[i,] - weight[,ind_win]));
    }
    dist_t[dist_ind_t[timeku]] <- dist_euc
    
    weig_t[weg_ind_t[timeku]]  <- weight
    timeku <- timeku + 1
    
    # print alp
    #print(alp)
  }
  alp = (alp*d_alp)
  weig[weg_ind[1]] <- weight 
  
  
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
        #print(dist_euc)
        ind_win <- which.min(dist_euc)
        if(kelas[i] != ind_win){
          weight[,ind_win] = weight[,ind_win] - 
            (alp * (trainingku[i,] - weight[,ind_win]));
        }
        else{
          weight[,ind_win] = weight[,ind_win] +
            (alp * (trainingku[i,] - weight[,ind_win]));
        }
        dist_t[dist_ind_t[timeku]] <- dist_euc
        weig_t[weg_ind_t[timeku]] <- weight
        timeku <- timeku + 1
        
        # print alp
        #print(alp)
      }
      alp = (alp*d_alp)
      weig[weg_ind[(ulangan)]] <- weight
    }
  }
  weig$remove("siapa")
  dist_t$remove("siapa")
  
  # Membuat Attribute yah
  
  attrib <- list(weight_epoch = weig,weight_t = weig_t,
                 distance_t = dist_t,awal_weight = init_weight)
  return(attrib)
}

hasil_weight <- LVQ(dataku,target,alp = alphaku,
                      d_alp = d_alpha,epokku = 2)

letak <- length(hasil_weight$weight_t$keys)

print(hasil_weight$awal_weight)

for(ii in 1:letak){
  distanceku <- hasil_weight$distance_t$values[ii] 
  weightku   <- hasil_weight$weight_t$values[ii]
  print(paste("pembatas",ii))
  print("distance",end="")
  print(distanceku)
  
  print("weight",end="")
  print(weightku)
}