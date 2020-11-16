library(readr)
data_TLKM <- read_csv("Data TLKM.csv", col_types = cols(Date = col_character(), High = col_skip(), Low = col_skip()))
View(data_TLKM)

#Ambil harga penutupan saham tiap harinya
data <- data_TLKM[,2]
View(data)

#Mengatur data
saham_tseries=ts(data,frequency=12,start=c(2018,10),end=c(2020,10))
plot(saham_tseries, main='Grafik saham TLKM 2 tahun kebelakang')

#dekomposisi data
dec <- decompose(saham_tseries)
dec
plot(dec)
