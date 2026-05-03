cat("\n", rep("=", 80), "\n", sep = "")
cat("PENGESANAN PENCILAN - KAEDAH PERSENTIL\n")
cat(rep("=", 80), "\n\n")

cat("1. MENETAPKAN AMBANG PERSENTIL\n")
cat(rep("-", 50), "\n\n")

lower_percentile <- 0.05
upper_percentile <- 0.95

cat("Ambang pengesanan:\n")
cat("  Ambang bawah: persentil ke-5 (5%)\n")
cat("  Ambang atas : persentil ke-95 (95%)\n\n")

cat("2. MENGIRA NILAI AMBANG\n")
cat(rep("-", 50), "\n\n")

lower_threshold <- as.numeric(quantile(ts_data, lower_percentile, na.rm = TRUE))
upper_threshold <- as.numeric(quantile(ts_data, upper_percentile, na.rm = TRUE))

cat("Nilai ambang:\n")
cat("  Ambang bawah (P5) : $", format(round(lower_threshold), big.mark = ","), "\n", sep = "")
cat("  Ambang atas  (P95): $", format(round(upper_threshold), big.mark = ","), "\n\n", sep = "")

cat("3. KEPUTUSAN PENGESANAN PENCILAN\n")
cat(rep("-", 50), "\n\n")

lower_outliers <- which(ts_data < lower_threshold)
upper_outliers <- which(ts_data > upper_threshold)
all_outliers <- sort(c(lower_outliers, upper_outliers))

cat("Pencilan dikesan:\n")
cat("  Pencilan bawah: ", length(lower_outliers), "\n", sep = "")
cat("  Pencilan atas : ", length(upper_outliers), "\n", sep = "")
cat("  Jumlah pencilan: ", length(all_outliers), "\n", sep = "")
cat("  Peratusan data: ", round(length(all_outliers)/length(ts_data)*100, 2), "%\n\n", sep = "")


ts_index_to_ym <- function(ts_obj, idx) {
  st <- start(ts_obj)          # c(year, month)
  yr0 <- st[1]
  mo0 <- st[2]
  # idx bermula 1
  total_mo <- (mo0 - 1) + (idx - 1)
  yr <- yr0 + (total_mo %/% 12)
  mo <- (total_mo %% 12) + 1
  sprintf("%04d-%02d", yr, mo)
}

if (length(all_outliers) > 0) {
  cat("BUTIRAN PENCILAN (Top 10 paling jauh dari median):\n")
  cat("------------------------------------------------------------\n")
  cat(sprintf("%-10s %-15s %-10s %-18s\n",
              "Tarikh", "Nilai (USD)", "Jenis", "Perubahan dr Median"))
  cat("------------------------------------------------------------\n")
  
  median_val <- median(ts_data, na.rm = TRUE)
  
  outlier_info <- data.frame(
    index = all_outliers,
    tarikh = ts_index_to_ym(ts_data, all_outliers),
    value = as.numeric(ts_data[all_outliers]),
    type  = ifelse(all_outliers %in% lower_outliers, "BAWAH", "ATAS")
  )
  
  outlier_info$pct_from_median <- (outlier_info$value - median_val)/median_val * 100
  outlier_info$abs_dev <- abs(outlier_info$value - median_val)
  
  outlier_info <- outlier_info[order(-outlier_info$abs_dev), ]
  
  topn <- min(10, nrow(outlier_info))
  for (i in 1:topn) {
    row <- outlier_info[i, ]
    cat(sprintf("%-10s $%-14s %-10s %+.1f%%\n",
                row$tarikh,
                format(round(row$value), big.mark = ","),
                row$type,
                row$pct_from_median))
  }
  
  if (nrow(outlier_info) > 10) {
    cat("... dan ", nrow(outlier_info) - 10, " pencilan lagi\n", sep = "")
  }
  cat("\n")
}

cat("4. VISUALISASI\n")
cat(rep("-", 50), "\n\n")

op <- par(no.readonly = TRUE)

# Margin & saiz font lebih “UKM-friendly”
par(
  mar = c(5, 5.5, 3.5, 5) + 0.1,
  cex.main = 0.9,
  font.main = 2,
  cex.lab = 1.0,
  cex.axis = 0.9
)

plot(ts_data,
     main = "Nilai CIF Import Farmaseutikal dengan Pencilan (Persentil)",
     ylab = "",
     xlab = "Tahun",
     col = "darkblue",
     lwd = 2,
     type = "l")

mtext("Nilai CIF (USD)", side = 2, line = 4)

grid(col = "gray85", lty = "dotted")

abline(h = lower_threshold, col = "red", lty = 2, lwd = 1.3)
abline(h = upper_threshold, col = "red", lty = 2, lwd = 1.3)

if (length(lower_outliers) > 0) {
  points(time(ts_data)[lower_outliers], ts_data[lower_outliers],
         col = "orange", pch = 19, cex = 1.1)
}
if (length(upper_outliers) > 0) {
  points(time(ts_data)[upper_outliers], ts_data[upper_outliers],
         col = "red", pch = 19, cex = 1.2)
}

legend("topleft",
       legend = c("Siri Masa", "Pencilan Bawah", "Pencilan Atas", "Ambang Persentil"),
       col = c("darkblue", "orange", "red", "red"),
       lty = c(1, NA, NA, 2),
       pch = c(NA, 19, 19, NA),
       lwd = c(2, NA, NA, 1.3),
       cex = 0.8,
       bg = "white")


mtext(paste0("P5: ", format(round(lower_threshold), big.mark = ",")),
      side = 4, at = lower_threshold, col = "red", las = 1, cex = 0.75)
mtext(paste0("P95: ", format(round(upper_threshold), big.mark = ",")),
      side = 4, at = upper_threshold, col = "red", las = 1, cex = 0.75)

par(op)

cat("\n5. MENYIMPAN KEPUTUSAN\n")
cat(rep("-", 50), "\n\n")

outlier_results <- list(
  lower_percentile = lower_percentile,
  upper_percentile = upper_percentile,
  lower_threshold = lower_threshold,
  upper_threshold = upper_threshold,
  lower_outliers = lower_outliers,
  upper_outliers = upper_outliers,
  all_outliers = all_outliers,
  n_outliers = length(all_outliers),
  outlier_percentage = round(length(all_outliers)/length(ts_data)*100, 2)
)

cat("Keputusan pengesanan pencilan disimpan:\n")
cat("  - Ambang bawah (P5): $", format(round(lower_threshold), big.mark = ","), "\n", sep = "")
cat("  - Ambang atas (P95): $", format(round(upper_threshold), big.mark = ","), "\n", sep = "")
cat("  - Jumlah pencilan: ", outlier_results$n_outliers, " (",
    outlier_results$outlier_percentage, "% data)\n", sep = "")

cat("\n", rep("=", 80), "\n", sep = "")
cat("PENGESANAN PENCILAN SELESAI ✓\n")
cat(rep("=", 80), "\n\n")