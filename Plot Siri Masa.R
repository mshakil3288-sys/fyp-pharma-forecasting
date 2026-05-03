basic_stats <- list(
  Min = min(ts_data, na.rm = TRUE),
  Max = max(ts_data, na.rm = TRUE),
  Mean = mean(ts_data, na.rm = TRUE),
  Variance = var(ts_data, na.rm = TRUE),
  SD = sd(ts_data, na.rm = TRUE),
  P5 = quantile(ts_data, 0.05, na.rm = TRUE),
  P25 = quantile(ts_data, 0.25, na.rm = TRUE),
  Median = median(ts_data, na.rm = TRUE),
  P75 = quantile(ts_data, 0.75, na.rm = TRUE),
  P95 = quantile(ts_data, 0.95, na.rm = TRUE),
  N = length(ts_data),
  Start = format(start(ts_data)),
  End = format(end(ts_data))
)


format_money <- function(x) {
  if (abs(x) >= 1e9) {
    return(paste0("$", round(x/1e9, 3), "B"))  
  } else if (abs(x) >= 1e6) {
    return(paste0("$", round(x/1e6, 1), "J"))  
  } else if (abs(x) >= 1e3) {
    return(paste0("$", round(x/1e3, 1), "K"))  
  } else {
    return(paste0("$", round(x, 0)))
  }
}


format_variance <- function(x) {
  format(x, scientific = TRUE, digits = 3)
}

cat("\n=== STATISTIK ASAS ===\n")
cat("Tempoh Masa:", basic_stats$Start, "hingga", basic_stats$End, "\n")
cat("Bilangan Pemerhatian:", basic_stats$N, "\n\n")

cat("Statistik Lokasi:\n")
cat("  Minimum:", format_money(basic_stats$Min), "\n")
cat("  Persentil ke-5:", format_money(basic_stats$P5), "\n")
cat("  Persentil ke-25:", format_money(basic_stats$P25), "\n")
cat("  Median:", format_money(basic_stats$Median), "\n")
cat("  Purata (Mean):", format_money(basic_stats$Mean), "\n")
cat("  Persentil ke-75:", format_money(basic_stats$P75), "\n")
cat("  Persentil ke-95:", format_money(basic_stats$P95), "\n")
cat("  Maksimum:", format_money(basic_stats$Max), "\n\n")

cat("Statistik Serakan:\n")
cat("  Varians (USD^2):", format_variance(basic_stats$Variance), "\n")
cat("  Sisihan Piawai:", format_money(basic_stats$SD), "\n")
cat("  Julat:", format_money(basic_stats$Max - basic_stats$Min), "\n")



op <- par(no.readonly = TRUE)

par(
  mar = c(5, 5.5, 3.5, 2) + 0.1,  
  cex.main = 0.9,                 
  font.main = 2,                  
  cex.lab = 1.0,
  cex.axis = 0.9
)

plot(ts_data,
     main = "Nilai CIF Import Farmaseutikal (Bulanan)",
     ylab = "",
     xlab = "Tahun",
     col = "darkblue",
     lwd = 2)

mtext("Nilai CIF (USD)", side = 2, line = 4)


grid(col = "gray85", lty = "dotted")


abline(h = basic_stats$Mean, col = "red", lwd = 1.5, lty = 2)


abline(h = basic_stats$P5,  col = "orange", lwd = 1.2, lty = 3)
abline(h = basic_stats$P95, col = "orange", lwd = 1.2, lty = 3)


legend("topleft",
       legend = c("Nilai CIF", "Purata (Mean)", "Persentil ke-5 & ke-95"),
       col = c("darkblue", "red", "orange"),
       lwd = c(2, 1.5, 1.2),
       lty = c(1, 2, 3),
       cex = 0.8,
       bg = "white",
       inset = 0.02)

par(op)