cat("\n", rep("=", 80), "\n", sep = "")
cat("DIBETULKAN: PEMBOLEHUBAH DUMMY UNTUK PEMATAHAN STRUKTUR\n")
cat(rep("=", 80), "\n\n")


cat("1. PERSEDIAAN DAN PENGESAHAN\n")
cat(rep("-", 50), "\n")


if(exists("ts_winsorized")) {
  ts_analysis <- ts_winsorized
  data_type <- "Di-Winsorisasi"
} else {
  ts_analysis <- ts_data
  data_type <- "Asal"
  cat("Menggunakan data asal (data winsorisasi tidak ditemui)\n")
}

if(!exists("break_dates") || length(break_dates) < 3) {
  stop("Memerlukan 3 tarikh pematahan dari analisis Bai-Perron")
}

cat("Menggunakan data", data_type, "dengan", length(ts_analysis), "pemerhatian\n")
cat("Tarikh pematahan:", paste(round(break_dates[1:3], 3), collapse = ", "), "\n\n")


cat("2. MENCIPTA PEMBOLEHUBAH PATUNG\n")
cat(rep("-", 50), "\n")


time_idx <- as.numeric(time(ts_analysis))


b1 <- break_dates[1]
b2 <- break_dates[2]
b3 <- break_dates[3]


format_date <- function(x) {
  year <- floor(x)
  month <- round((x - year) * 12) + 1
  month_names <- c("Jan","Feb","Mac","Apr","Mei","Jun",
                   "Jul","Ogo","Sep","Okt","Nov","Dis")
  paste(month_names[month], year)
}

cat("Tarikh Pematahan:\n")
cat("  Pematahan 1:", format_date(b1), "\n")
cat("  Pematahan 2:", format_date(b2), "\n")
cat("  Pematahan 3:", format_date(b3), "\n\n")


d1 <- ifelse(time_idx >= b1, 1, 0)
d2 <- ifelse(time_idx >= b2, 1, 0)
d3 <- ifelse(time_idx >= b3, 1, 0)

cat("Statistik PATUNG:\n")
stats <- data.frame(
  Dummy = c("D1", "D2", "D3"),
  Satu = c(sum(d1), sum(d2), sum(d3)),
  Peratus = round(c(mean(d1), mean(d2), mean(d3)) * 100, 1)
)
print(stats)
cat("\n")


cat("3. PEMILIHAN PATUNG\n")
cat(rep("-", 50), "\n")


xreg_matrix <- cbind(d1, d2)
colnames(xreg_matrix) <- c("Pematahan1", "Pematahan2")

cat(" Menggunakan dummy untuk Pematahan 1 dan Pematahan 2 sahaja\n")
cat(" Segmen akhir diimplikasi melalui kombinasi pintasan + dummy\n")
cat("\nMatriks XREG:", nrow(xreg_matrix), "×", ncol(xreg_matrix), "\n\n")


cat("3.5 UJIAN MULTIKOLINEARITI (D1, D2, D3)\n")
cat(rep("-", 50), "\n")

dummy_df <- data.frame(D1 = d1, D2 = d2, D3 = d3)


cat("A) Korelasi antara PATUNG (petunjuk awal, bukan bukti sempurna):\n")
cor_mat <- cor(dummy_df)
print(round(cor_mat, 4))
cat("\n")


cat("B) Semakan multikolineariti sempurna (rank & alias):\n")
X_full <- model.matrix(~ D1 + D2 + D3, data = dummy_df) 
rank_X <- qr(X_full)$rank
p_X <- ncol(X_full)

cat("  Bilangan lajur (termasuk pintasan):", p_X, "\n")
cat("  Rank matriks reka bentuk:", rank_X, "\n")

if(rank_X < p_X) {
  cat("  Matriks singular → multikolineariti sempurna wujud.\n")
  cat("  Alias (pemboleh ubah linear dependen):\n")
  ali <- alias(lm(rep(1, nrow(dummy_df)) ~ D1 + D2 + D3, data = dummy_df))
  print(ali)
} else {
  cat("  Tiada singulariti dikesan pada kombinasi ini.\n")
}
cat("\n")


cat("C) VIF (jika boleh dianggar):\n")
if(!requireNamespace("car", quietly = TRUE)) {
  install.packages("car")
}
library(car)


try({
  m_full <- lm(ts_analysis ~ D1 + D2 + D3, data = dummy_df)
  v_full <- car::vif(m_full)
  print(v_full)
}, silent = TRUE)


m_2 <- lm(ts_analysis ~ D1 + D2, data = dummy_df)
print(car::vif(m_2))
cat("\n")


cat("4. ANALISIS SEGMEN\n")
cat(rep("-", 50), "\n")

segments <- list(
  S1 = time_idx < b1,
  S2 = time_idx >= b1 & time_idx < b2,
  S3 = time_idx >= b2 & time_idx < b3,
  S4 = time_idx >= b3
)

means <- sapply(segments, function(idx) mean(ts_analysis[idx], na.rm = TRUE))
n_obs <- sapply(segments, sum)

cat("Statistik Segmen:\n")
for(i in 1:4) {
  cat(sprintf("  Segmen %d: $%s (n=%d, %.1f%%)\n",
              i,
              format(round(means[i]), big.mark = ","),
              n_obs[i],
              n_obs[i]/length(ts_analysis)*100))
}
cat("\n")


cat("5. VISUALISASI\n")
cat(rep("-", 50), "\n")

par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

plot(time_idx, d1, type = "s", col = "#DC2626", lwd = 2,
     main = "Pembolehubah Patung\n(Menggunakan D1 & D2 sahaja)",
     xlab = "Tahun", ylab = "Nilai PATUNG",
     ylim = c(-0.1, 1.1), yaxt = "n")
lines(time_idx, d2, type = "s", col = "#059669", lwd = 2, lty = 2)
lines(time_idx, d3, type = "s", col = "gray", lwd = 1, lty = 3)

abline(v = c(b1, b2, b3), col = "gray40", lty = 3, lwd = 1)
axis(2, at = c(0, 1), labels = c("0", "1"), las = 1)

legend("topleft",
       legend = c("Pematahan 1 (digunakan)", "Pematahan 2 (digunakan)", "Pematahan 3 (indikator)"),
       col = c("#DC2626", "#059669", "gray"),
       lwd = c(2, 2, 1),
       lty = 1:3, cex = 0.8, bg = "white")

means_millions <- means / 1e6
seg_labels <- c("S1", "S2", "S3", "S4")
seg_colors <- c("#1E3A8A", "#DC2626", "#059669", "gray")

barplot(means_millions, names.arg = seg_labels, col = seg_colors,
        main = "Min Segmen (Juta $)",
        ylab = "Purata Nilai CIF ($J)",
        border = NA, ylim = c(0, max(means_millions) * 1.1))

text(1:4, means_millions + max(means_millions)*0.05,
     labels = paste("$", round(means_millions, 1), "J", sep = ""),
     cex = 0.8, col = "darkred", font = 2)

par(mfrow = c(1, 1))


cat("6. MENYIMPAN UNTUK PEMODELAN\n")
cat(rep("-", 50), "\n")

dummy_info <- list(
  ts_data = ts_analysis,
  xreg = xreg_matrix,
  break_dates = c(b1, b2, b3),
  break_labels = c(format_date(b1), format_date(b2), format_date(b3)),
  segment_means = means,
  data_type = data_type,
  note = "Menggunakan 2 PATUNG sahaja. 3 PATUNG + pintasan → singular."
)

assign("dummy_info", dummy_info, envir = .GlobalEnv)
assign("xreg_matrix", xreg_matrix, envir = .GlobalEnv)

cat("✓ Disimpan sebagai 'dummy_info' dan 'xreg_matrix'\n")
cat("✓ Sedia untuk pemodelan ARIMAX dengan 2 PATUNG\n\n")


cat("7. ARAHAN PEMODELAN\n")
cat(rep("-", 50), "\n")

cat("Untuk sesuaikan model ARIMAX:\n\n")
cat("library(forecast)\n")
cat("model <- auto.arima(dummy_info$ts_data,\n")
cat("                    xreg = dummy_info$xreg,\n")
cat("                    seasonal = FALSE)\n\n")

cat("Interpretasi Model (2 dummy):\n")
cat("  Pintasan: Aras asas sebelum", format_date(b1), "\n")
cat("  Pematahan1: Perubahan selepas", format_date(b1), "\n")
cat("  Pematahan2: Perubahan selepas", format_date(b2), "\n")
cat("  Segmen selepas", format_date(b3), "diwakili oleh kombinasi pintasan + dummy\n\n")

cat("\n", rep("=", 80), "\n", sep = "")
cat("PENCIPTAAN PEMBOLEHUBAH PATUNG + UJIAN MULTIKOLINEARITI SELESAI ✓\n")
cat(rep("=", 80), "\n\n")
