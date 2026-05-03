cat("\n", rep("=", 80), "\n", sep = "")
cat("ANALISIS ACF DAN PACF - SIRI DIBEZAKAN\n")
cat(rep("=", 80), "\n\n")

library(forecast)


cat("1. PENYEDIAAN DATA\n")
cat(rep("-", 50), "\n")

if (exists("differenced_data")) {
  ts_diff <- differenced_data$ts_differenced
  data_type <- differenced_data$data_type
  cat("Menggunakan siri dibezakan dari 'differenced_data'\n")
  cat("  Jenis data:", data_type, "\n")
  cat("  Pemerhatian:", length(ts_diff), "\n")
  cat("  Pembezaan: D = 1\n\n")
} else {
  stop("Tiada data dibezakan ditemui.")
}

cat("2. VISUALISASI: PLOT ACF DAN PACF\n")
cat(rep("-", 50), "\n")


acf_vals <- Acf(ts_diff, plot = FALSE, lag.max = 36)
pacf_vals <- Pacf(ts_diff, plot = FALSE, lag.max = 36)


par(mfrow = c(1, 2), mar = c(4, 4, 4, 2), oma = c(0, 0, 2, 0))


plot(acf_vals, 
     main = "(A) ACF ",
     xlab = "Lengah (bulan)",
     ylab = "Autokorelasi",
     ylim = c(-1, 1),
     col = "#DC2626",
     lwd = 2)


ci <- 1.96/sqrt(length(ts_diff))
abline(h = c(ci, -ci), col = "blue", lty = 2, lwd = 1)


plot(pacf_vals, 
     main = "(B) PACF",
     xlab = "Lengah (bulan)",
     ylab = "Autokorelasi Separa",
     ylim = c(-1, 1),
     col = "#059669",
     lwd = 2)


abline(h = c(ci, -ci), col = "blue", lty = 2, lwd = 1)


mtext(paste("Analisis Autokorelasi - Data winsorisasi"), 
      outer = TRUE, cex = 1.2, font = 2, line = -1)


cat("3. CADANGAN TERTIB ARIMA\n")
cat(rep("-", 50), "\n")


sig_acf_lags <- which(abs(acf_vals$acf) > ci & acf_vals$lag > 0)
sig_pacf_lags <- which(abs(pacf_vals$acf) > ci)

cat("Korelasi signifikan (keyakinan 95%):\n")

if (length(sig_acf_lags) > 0) {
  cat("\nLengah ACF signifikan:\n")
  for (lag_idx in sig_acf_lags) {
    lag <- acf_vals$lag[lag_idx]
    value <- acf_vals$acf[lag_idx]
    cat(sprintf("  Lengah %2d: ACF = %6.3f\n", lag, value))
  }
}

if (length(sig_pacf_lags) > 0) {
  cat("\nLengah PACF signifikan:\n")
  for (lag_idx in sig_pacf_lags) {
    lag <- pacf_vals$lag[lag_idx]
    value <- pacf_vals$acf[lag_idx]
    cat(sprintf("  Lengah %2d: PACF = %6.3f\n", lag, value))
  }
}

cat("\nAnalisis Corak:\n")


if (length(sig_pacf_lags) > 0) {
  max_sig_pacf_lag <- max(pacf_vals$lag[sig_pacf_lags])
  cat("  • PACF signifikan pada lengah:", 
      paste(pacf_vals$lag[sig_pacf_lags], collapse = ", "), "\n")
  cat("   Mencadangkan komponen AR(", max_sig_pacf_lag, ")\n", sep = "")
}


if (length(sig_acf_lags) > 0) {
  max_sig_acf_lag <- max(acf_vals$lag[sig_acf_lags])
  cat("  ACF signifikan pada lengah:", 
      paste(acf_vals$lag[sig_acf_lags], collapse = ", "), "\n")
  cat("  Mencadangkan komponen MA(", max_sig_acf_lag, ")\n", sep = "")
}


if (length(sig_acf_lags) > 0 && length(sig_pacf_lags) > 0) {
  cat("   Komponen AR dan MA kedua-duanya mungkin diperlukan\n")
  cat("   Pertimbangkan model ARMA\n")
}

if (length(sig_acf_lags) == 0 && length(sig_pacf_lags) == 0) {
  cat("  Tiada autokorelasi signifikan\n")
  cat("  Mencadangkan hingar putih/jalan rawak\n")
}


cat("\n4. MENYIMPAN KEPUTUSAN\n")
cat(rep("-", 50), "\n")

acf_pacf_results <- list(
  ts_differenced = ts_diff,
  data_type = data_type,
  acf_values = acf_vals,
  pacf_values = pacf_vals,
  significant_acf_lags = if(length(sig_acf_lags) > 0) acf_vals$lag[sig_acf_lags] else numeric(0),
  significant_pacf_lags = if(length(sig_pacf_lags) > 0) pacf_vals$lag[sig_pacf_lags] else numeric(0),
  confidence_interval = ci,
  analysis_date = Sys.Date()
)

assign("acf_pacf_results", acf_pacf_results, envir = .GlobalEnv)

par(mfrow = c(1, 1), mar = c(5.1, 4.1, 4.1, 2.1), oma = c(0, 0, 0, 0))