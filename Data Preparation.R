library(readr)
TLKM_bulanan <- read_delim("TLKM bulanan.csv",";", escape_double = FALSE, col_types = cols(Vol. = col_skip()),trim_ws = TRUE)
View(TLKM_bulanan)

#Ambil harga penutupan saham tiap harinya
data <- TLKM_bulanan[,2]
View(data)

#Mengatur data
saham_tseries=ts(data,frequency=12,start=c(2016,1),end=c(2020,11))
plot(saham_tseries, main='Grafik saham TLKM 2 tahun kebelakang')

#dekomposisi data
dec <- decompose(saham_tseries)
dec
plot(dec)
#