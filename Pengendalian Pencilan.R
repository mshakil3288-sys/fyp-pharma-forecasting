cat("\n", rep("=", 80), "\n", sep = "")
cat("RAWATAN PENCILAN - WINSORIZING (Kaedah Persentil)\n")
cat(rep("=", 80), "\n\n")

cat("1. MENGGUNAKAN AMBANG DARI PENGESANAN PENCILAN\n")
cat(rep("-", 50), "\n\n")


if (exists("outlier_results")) {
  lower_threshold <- as.numeric(outlier_results$lower_threshold)
  upper_threshold <- as.numeric(outlier_results$upper_threshold)
  lower_percentile <- outlier_results$lower_percentile
  upper_percentile <- outlier_results$upper_percentile
  cat("Menggunakan ambang yang sama seperti pengesanan pencilan:\n")
} else {
  lower_percentile <- 0.05
  upper_percentile <- 0.95
  lower_threshold <- as.numeric(quantile(ts_data, lower_percentile, na.rm = TRUE))
  upper_threshold <- as.numeric(quantile(ts_data, upper_percentile, na.rm = TRUE))
  cat("Mengira ambang baru (persentil ke-5/95):\n")
}

cat("  Ambang bawah: $", format(round(lower_threshold), big.mark = ","), 
    " (persentil ke-", round(lower_percentile * 100, 1), ")\n", sep = "")
cat("  Ambang atas: $", format(round(upper_threshold), big.mark = ","), 
    " (persentil ke-", round(upper_percentile * 100, 1), ")\n\n", sep = "")

cat("2. MELAKSANAKAN WINSORIZING\n")
cat(rep("-", 50), "\n\n")


ts_winsorized <- ts_data
ts_winsorized <- pmax(ts_winsorized, lower_threshold)
ts_winsorized <- pmin(ts_winsorized, upper_threshold)

adjusted_values <- which(as.numeric(ts_data) != as.numeric(ts_winsorized))
n_adjusted <- length(adjusted_values)

cat("Winsorizing selesai:\n")
cat("  - Nilai diselaraskan: ", n_adjusted, "\n", sep = "")
cat("  - Peratusan data: ", round(n_adjusted/length(ts_data)*100, 2), "%\n", sep = "")


ts_index_to_ym <- function(ts_obj, idx) {
  st <- start(ts_obj)  # c(year, month)
  yr0 <- st[1]; mo0 <- st[2]
  total_mo <- (mo0 - 1) + (idx - 1)
  yr <- yr0 + (total_mo %/% 12)
  mo <- (total_mo %% 12) + 1
  sprintf("%04d-%02d", yr, mo)
}

if (n_adjusted > 0) {
  cat("\nPenyesuaian teratas yang dibuat (Top 5):\n")
  cat("  Tarikh     Asal (USD)        Diselaraskan (USD)   Perubahan\n")
  cat("  ------------------------------------------------------------\n")
  
 
  delta_abs <- abs(as.numeric(ts_winsorized[adjusted_values]) - as.numeric(ts_data[adjusted_values]))
  ord <- order(-delta_abs)
  top_idx <- adjusted_values[ord]
  
  for (i in 1:min(5, n_adjusted)) {
    idx <- top_idx[i]
    orig_val <- as.numeric(ts_data[idx])
    adj_val  <- as.numeric(ts_winsorized[idx])
    change_pct <- (adj_val - orig_val)/orig_val * 100
    
    cat(sprintf("  %-8s $%-15s $%-18s %+.1f%%\n",
                ts_index_to_ym(ts_data, idx),
                format(round(orig_val), big.mark = ","),
                format(round(adj_val), big.mark = ","),
                change_pct))
  }
  
  if (n_adjusted > 5) cat("  ... dan ", n_adjusted - 5, " penyesuaian lagi\n", sep = "")
}

cat("\n3. PERBANDINGAN STATISTIK\n")
cat(rep("-", 50), "\n\n")

orig_stats <- c(
  min   = min(ts_data, na.rm = TRUE),
  max   = max(ts_data, na.rm = TRUE),
  range = max(ts_data, na.rm = TRUE) - min(ts_data, na.rm = TRUE)
)

wins_stats <- c(
  min   = min(ts_winsorized, na.rm = TRUE),
  max   = max(ts_winsorized, na.rm = TRUE),
  range = max(ts_winsorized, na.rm = TRUE) - min(ts_winsorized, na.rm = TRUE)
)

cat("Perbandingan Statistik:\n")
cat("-------------------------------------------------------------\n")
cat(sprintf("%-10s %18s %18s %12s\n", "Statistik", "Asal", "Winsorize", "Perubahan"))
cat("-------------------------------------------------------------\n")

for (stat in c("min", "max", "range")) {
  orig_val <- orig_stats[stat]
  wins_val <- wins_stats[stat]
  change_pct <- (wins_val - orig_val)/orig_val * 100
  
  cat(sprintf("%-10s $%-17s $%-17s %11s\n",
              toupper(stat),
              format(round(orig_val), big.mark = ","),
              format(round(wins_val), big.mark = ","),
              if (stat == "range") sprintf("%+.1f%%", change_pct) else "—"))
}

cat("\n4. PERBANDINGAN VISUAL\n")
cat(rep("-", 50), "\n\n")

op <- par(no.readonly = TRUE)


par(
  mar = c(5, 5.5, 3.5, 6) + 0.1,
  cex.main = 0.9,  
  font.main = 2,
  cex.lab = 1.0,
  cex.axis = 0.9
)

plot(ts_data,
     main = "Nilai CIF Asal vs Nilai Winsorisasi\n(Kaedah Persentil: ke-5 dan ke-95)",
     ylab = "",
     xlab = "Tahun",
     col = "darkblue",
     lwd = 2.2,
     type = "l")

mtext("Nilai CIF (USD)", side = 2, line = 4)

lines(ts_winsorized,
      col = "red",
      lwd = 1.8,
      lty = 2)

grid(col = "gray85", lty = "dotted")

abline(h = lower_threshold, col = "darkgreen", lty = 3, lwd = 1.2)
abline(h = upper_threshold, col = "darkgreen", lty = 3, lwd = 1.2)

if (n_adjusted > 0) {
  points(time(ts_data)[adjusted_values], ts_winsorized[adjusted_values],
         col = "orange", pch = 19, cex = 1.05)
  
 
  for (i in 1:min(10, n_adjusted)) {
    idx <- adjusted_values[i]
    segments(time(ts_data)[idx], ts_data[idx],
             time(ts_data)[idx], ts_winsorized[idx],
             col = "orange", lty = 3, lwd = 0.8)
  }
}

legend("topleft",
       legend = c("Asal", "Di-winsorize", "Titik diselaraskan", "Ambang (P5/P95)"),
       col = c("darkblue", "red", "orange", "darkgreen"),
       lty = c(1, 2, NA, 3),
       pch = c(NA, NA, 19, NA),
       lwd = c(2.2, 1.8, NA, 1.2),
       bg = "white",
       cex = 0.8,
       inset = 0.02)

format_million <- function(x) paste0("$", round(x/1e6, 1), "M")

mtext(paste0("P5:  ", format_million(lower_threshold)),
      side = 4, at = lower_threshold, col = "darkgreen", las = 1, cex = 0.75, line = 0.5)
mtext(paste0("P95: ", format_million(upper_threshold)),
      side = 4, at = upper_threshold, col = "darkgreen", las = 1, cex = 0.75, line = 0.5)

par(op)

cat("5. PENYIMPANAN DATA\n")
cat(rep("-", 50), "\n\n")

winsorized_data <- list(
  ts_winsorized = ts_winsorized,
  lower_percentile = lower_percentile,
  upper_percentile = upper_percentile,
  lower_threshold = lower_threshold,
  upper_threshold = upper_threshold,
  n_adjusted = n_adjusted,
  adjusted_indices = adjusted_values,
  original_stats = orig_stats,
  winsorized_stats = wins_stats
)

assign("ts_winsorized", ts_winsorized, envir = .GlobalEnv)
assign("winsorized_data", winsorized_data, envir = .GlobalEnv)

cat("  - Diselaraskan: ", n_adjusted, " nilai (", 
    round(n_adjusted/length(ts_data)*100, 1), "% data)\n", sep = "")
cat("  - Disimpan sebagai: 'ts_winsorized' dan 'winsorized_data'\n\n")