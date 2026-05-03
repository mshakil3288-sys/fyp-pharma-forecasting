# ============================================
# IMPORT DAN SEDIA DATA UNTUK SIRI MASA (TIADA PLOT)
# ============================================

# Kosongkan ruang kerja
rm(list = ls())

# Baca data
data <- read.csv("C://Users//Azie//Downloads//Sum of Cifvalue.csv")

# Bersihkan nama lajur (buang titik/ruang)
names(data) <- c("tahun_bulan", "nilai_cif")

# Ekstrak tahun dan bulan
data$tahun <- as.numeric(substr(data$tahun_bulan, 1, 4))
data$bulan <- as.numeric(substr(data$tahun_bulan, 5, 6))

# Cipta lajur tarikh yang betul
data$tarikh <- as.Date(paste(data$tahun, data$bulan, "01", sep = "-"))

# Susun mengikut tarikh
data <- data[order(data$tarikh), ]

# Cipta objek siri masa (kekerapan bulanan = 12)
ts_data <- ts(data$nilai_cif, 
              start = c(data$tahun[1], data$bulan[1]), 
              frequency = 12)

# Tunjukkan apa yang kami ada
cat("Import data selesai!\n")
cat("Jumlah baris:", nrow(data), "\n")
cat("Julat tarikh:", as.character(data$tarikh[1]), "hingga", 
    as.character(data$tarikh[nrow(data)]), "\n")
cat("Beberapa nilai pertama:\n")
print(head(data, 5))
cat("\nObjek siri masa telah dicipta.\n")