cat("\n", rep("=", 80), "\n", sep = "")
cat("VISUALISASI PEMBEZAAN PERTAMA (D = 1)\n")
cat(rep("=", 80), "\n\n")

library(forecast)


cat("1. PENYEDIAAN DATA\n")
cat(rep("-", 50), "\n")


if (exists("stationarity_results")) {
  ts_original <- stationarity_results$ts_original
  d_recommended <- stationarity_results$recommended_d
  data_type <- stationarity_results$data_used
  cat("Menggunakan data dari ujian kepegunan\n")
  cat("  Jenis: Data", data_type, "\n")
  cat("  d disyorkan dari ujian:", d_recommended, "\n\n")
} else if (exists("dummy_info")) {
  ts_original <- dummy_info$ts_data
  d_recommended <- 1  # Lalai jika tiada ujian dijalankan
  data_type <- dummy_info$data_type
  cat("Menggunakan data dari dummy_info \n")
  cat("  Lalai kepada d = 1\n\n")
} else {
  stop("Tiada data siri masa ditemui.")
}


ts_diff <- diff(ts_original)
n_original <- length(ts_original)
n_diff <- length(ts_diff)


ts_original_millions <- ts_original / 1e6
ts_diff_millions <- ts_diff / 1e6

cat("Pembezaan Digunakan:\n")
cat("  Panjang siri asal:", n_original, "\n")
cat("  Panjang siri dibezakan:", n_diff, "\n")
cat("  Pemerhatian hilang:", n_original - n_diff, "\n")
cat("  Min asal: $", format(round(mean(ts_original)), big.mark = ","), "\n", sep = "")
cat("  Min beza: $", format(round(mean(ts_diff)), big.mark = ","), "\n", sep = "")
cat("  SP beza: $", format(round(sd(ts_diff)), big.mark = ","), "\n\n", sep = "")


cat(" VISUALISASI: SIRI ASAL vs DIBEZAKAN\n")
cat(rep("-", 50), "\n")


par(mfrow = c(1, 2), mar = c(4, 4, 4, 2), oma = c(0, 0, 2, 0))


plot(time(ts_original), ts_original_millions,
     type = "l",
     col = "#1E3A8A",
     lwd = 2.5,
     main = "(A) Siri Asal (Juta $)",
     xlab = "Tahun",
     ylab = "Nilai CIF (Juta $)",
     ylim = c(min(ts_original_millions) * 0.95, 
              max(ts_original_millions) * 1.05))

grid(col = "gray90", lty = "dotted")


abline(h = mean(ts_original_millions), 
       col = "red", lty = 2, lwd = 1.5)


orig_stats <- paste(
  "Min: $", round(mean(ts_original_millions), 1), "J\n",
  "SP: $", round(sd(ts_original_millions), 1), "J",
  sep = ""
)

text(max(time(ts_original)) * 0.98, 
     min(ts_original_millions) * 1.02,
     orig_stats,
     pos = 2, cex = 0.8, col = "darkblue")


plot(time(ts_diff), ts_diff_millions,
     type = "l",
     col = "#DC2626",
     lwd = 2,
     main = "(B) Perbezaan Pertama D=1 (Juta $)",
     xlab = "Tahun",
     ylab = "Perubahan Bulanan (Juta $)",
     ylim = c(min(ts_diff_millions) * 1.1, 
              max(ts_diff_millions) * 1.1))

grid(col = "gray90", lty = "dotted")


abline(h = 0, col = "black", lty = 3, lwd = 1)


abline(h = mean(ts_diff_millions), 
       col = "blue", lty = 2, lwd = 1.5)


diff_stats <- paste(
  "Min: $", round(mean(ts_diff_millions), 1), "J\n",
  "SP: $", round(sd(ts_diff_millions), 1), "J\n",
  "Min: $", round(min(ts_diff_millions), 1), "J\n",
  "Maks: $", round(max(ts_diff_millions), 1), "J",
  sep = ""
)

text(max(time(ts_diff)) * 0.98, 
     min(ts_diff_millions) * 1.05,
     diff_stats,
     pos = 2, cex = 0.7, col = "darkred")


mtext(paste("Pembezaan Pertama (D=1) - Data ", data_type, sep = ""), 
      outer = TRUE, cex = 1.2, font = 2, line = -1)


cat("3. TABURAN PERBEZAAN\n")
cat(rep("-", 50), "\n")


cat("Statistik Perubahan Bulanan:\n")
cat("  Perubahan positif:", sum(ts_diff > 0), "(", 
    round(sum(ts_diff > 0)/n_diff*100, 1), "%)\n", sep = "")
cat("  Perubahan negatif:", sum(ts_diff < 0), "(", 
    round(sum(ts_diff < 0)/n_diff*100, 1), "%)\n", sep = "")
cat("  Perubahan sifar:", sum(ts_diff == 0), "(", 
    round(sum(ts_diff == 0)/n_diff*100, 1), "%)\n\n", sep = "")


large_increase <- sum(ts_diff > sd(ts_diff) * 2)
large_decrease <- sum(ts_diff < -sd(ts_diff) * 2)

cat("Perubahan Besar (>2 SP):\n")
cat("  Peningkatan besar:", large_increase, "\n")
cat("  Penurunan besar:", large_decrease, "\n")
cat("  Jumlah perubahan besar:", large_increase + large_decrease, 
    "(", round((large_increase + large_decrease)/n_diff*100, 1), "%)\n\n", sep = "")


cat("4. MENYIMPAN SIRI DIBEZAKAN\n")
cat(rep("-", 50), "\n")


differenced_data <- list(
  ts_original = ts_original,
  ts_original_millions = ts_original_millions,
  ts_differenced = ts_diff,
  ts_differenced_millions = ts_diff_millions,
  d_value = 1,
  data_type = data_type,
  diff_statistics = list(
    min = min(ts_diff),
    maks = max(ts_diff),
    n_positif = sum(ts_diff > 0),
    n_negatif = sum(ts_diff < 0),
    n_sifar = sum(ts_diff == 0)
  ),
  differencing_date = Sys.Date()
)

assign("differenced_data", differenced_data, envir = .GlobalEnv)

par(mfrow = c(1, 1), mar = c(5.1, 4.1, 4.1, 2.1), oma = c(0, 0, 0, 0))