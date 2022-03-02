konversi_temperatur <- function(suhu, darimana, kemana){
  list_konv <- list(fahrenheit = list(banding = 9,konstanta = 32),
                    celcius = list(banding = 5, konstanta = 0),
                    reamur = list(banding = 4, konstanta = 0),
                    kelvin = list(banding = 5, konstanta = 273.15))
  #untuk variabel list_konv ini untuk mendapatkan perbandingan dan konstanta
  dari <- list() # variabel untuk mendapatkan banding dan konstanta dari suhu asal
  ke   <- list() # variabel untuk mendapatkan banding dan konstanta dari suhu asal

  # loop ini untuk mencari string yang cocok nama suhu (disamakan awal karaternya)
  for(jenis_suhu in names(list_konv)){
    if(tolower(substr(jenis_suhu,1,nchar(darimana))) == tolower(darimana)){
          dari <- list_konv[[jenis_suhu]]
    }
    if(tolower(substr(jenis_suhu,1,nchar(kemana))) == tolower(kemana)){
      ke <- list_konv[[jenis_suhu]]
    }
  }
  
  # cek jika ada nama suhu yang ada
  if(length(dari)!=0 && length(ke)!=0){
    hasilnya <- ( ( (suhu - dari$konstanta) * 
                    (ke$banding/dari$banding)
                  ) + ke$konstanta)
  }
  else{
    hasilnya <- "Suhunya tidak tersedia "
  }
  return(hasilnya)
}
# untuk codingan yang ini disarankan running code per baris
dari_suhu <- readline("dari suhu apa?") 
dengan_suhu <- as.numeric(readline("dengan angka ?"))
ke_suhu   <- readline("ke suhu apa ?")

# untuk menghitung jawabannya
jawaban_pertama <- konversi_temperatur(dengan_suhu,dari_suhu,ke_suhu)

# untuk menuliskan lambang suhu
lambang_pertama <- paste("\u00B0",toupper(substr(dari_suhu,1,1)),sep='')
lambang_kedua   <- paste("\u00B0",toupper(substr(ke_suhu,1,1)),sep='')

# menuliskan jawabannya dengan format
print(paste(dengan_suhu,lambang_pertama,' = ',jawaban_pertama,lambang_kedua,sep=''))