options(digits=13)

library(Dict)
dataku <- matrix(c(1,1,1,0,
                   1,0,1,1,
                   0,1,1,0,
                   0,0,1,1),
                 nrow=4,
                 ncol=4,
                 byrow = TRUE)


weightku <- matrix(c(0.2,0.8,
                     0.6,0.4,
                     0.5,0.7,
                     0.9,0.3),
                   nrow=4,
                   ncol=2,
                   byrow = TRUE)

weight_t <- t(weightku)
alphaku <- c(rep(0.6,4),rep(0.3,4))

euclidku <- function(a,b){return ((a-b)^2)}
upd_learn <- function(i,w,alp){
  return(w+alp*(i-w))
}

mat_euclidku <- function(xx,yy,alp,epokku=1){
  t <- 1
  clus <- matrix(1:(nrow(xx)*epokku),nrow = epokku)
  weg_ind <- paste("mat",1:(nrow(xx)*epokku),sep="")
  weig <- Dict$new(siapa="rizal")
  
  
  
  for(ulangan in 1:epokku){
    clus_sem <- c()
    for(i in 1:nrow(xx)){
      ban_dis <- c()
      cat(paste("\nt=",t,"\n"))
      for(y in 1:nrow(yy)){
        dis_sum <- 0
        for(j in 1:ncol(xx)){
          distan <- euclidku(xx[i,j],yy[y,j])
          dis_sum <- dis_sum + distan
        }
        cat(paste("data ke ",i,"berat ke",y," = ",dis_sum,"\n"))
        ban_dis <- c(ban_dis,dis_sum)
      }
      menang_idx <- which.min(ban_dis)
      clus_sem <- c(clus_sem,menang_idx)
      yy[menang_idx,] <- upd_learn(xx[i,],yy[menang_idx,],alp[t])
      cat(paste("\n data ke",i,"menang jadi cluster",menang_idx,"\n"))
      print(yy)
      
      mat_yy <- matrix(yy,nrow = 1)
      weig[weg_ind[t]] <- mat_yy
      
      
      t <- t + 1
    }
    clus[ulangan,] <- clus_sem
    cat("\n")
    print(paste("epokku",ulangan))
    for(gg in 1:nrow(xx)){
      print(paste("data ke-",gg,"--> cluster ke-",clus_sem[gg]))
    }
  }
  weig$remove("siapa")
  hasil_yy <- list(weight=weig,cluster=clus)
  print(clus)
  return(hasil_yy)
}


hasil <- mat_euclidku(dataku,weight_t,alp = alphaku,epokku = 2)
print(hasil)