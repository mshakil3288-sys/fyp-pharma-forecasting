cat("\n", rep("=", 80), "\n", sep = "")
cat("PENGESAHAN DALAM-SAMPEL (80/20 BAHAGIAN)\n")
cat(rep("=", 80), "\n\n")

library(forecast)


cat("1. MEMUAT DATA DAN KOMPONEN MODEL\n")
cat(rep("-", 50), "\n")

if (!exists("final_arimax_auto")) {
  stop("Jalankan pembinaan model ARIMAX dahulu")
}


ts_original <- final_arimax_auto$ts_original
xreg <- final_arimax_auto$xreg_matrix
arima_order <- final_arimax_auto$arima_order

cat(" Data siri masa dimuatkan\n")
cat("  Jumlah pemerhatian:", length(ts_original), "\n")
cat("  Tertib ARIMA: (", arima_order[1], ",", arima_order[2], ",", 
    arima_order[3], ")\n", sep = "")
cat("  Pembolehubah PATUNG:", ncol(xreg), "\n\n")


cat("2. PEMBAHAGIAN DATA: 80% LATIHAN, 20% UJIAN\n")
cat(rep("-", 50), "\n")


n_total <- length(ts_original)
n_train <- round(n_total * 0.8)  
n_test <- n_total - n_train       


train_original <- window(ts_original, end = time(ts_original)[n_train])
test_original <- window(ts_original, start = time(ts_original)[n_train + 1])


train_diff <- diff(train_original)

cat("Butiran Pembahagian Data:\n")
cat("  Set latihan: ", n_train, " pemerhatian pertama (80%)\n", sep = "")
cat("    Tempoh: ", format(time(train_original)[1], nsmall = 2), 
    " hingga ", format(time(train_original)[n_train], nsmall = 2), "\n", sep = "")
cat("  Set ujian: ", n_test, " pemerhatian terakhir (20%)\n", sep = "")
cat("    Tempoh: ", format(time(test_original)[1], nsmall = 2), 
    " hingga ", format(time(test_original)[n_test], nsmall = 2), "\n\n", sep = "")


cat("3. MENYEDIAKAN PEMBOLEHUBAH PATUNG\n")
cat(rep("-", 50), "\n")


if (nrow(xreg) == length(final_arimax_auto$ts_diff)) {
  train_xreg <- xreg[1:(n_train-1), , drop = FALSE]  
  test_xreg <- xreg[n_train:(n_total-1), , drop = FALSE]
  cat("Matriks xreg disediakan untuk data dibezakan\n")
} else {
  train_xreg <- xreg[2:n_train, , drop = FALSE] 
  test_xreg <- xreg[(n_train+1):n_total, , drop = FALSE]
  cat(" xreg dilaraskan untuk data dibezakan\n")
}

cat("  xreg latihan:", nrow(train_xreg), "baris\n")
cat("  xreg ujian: ", nrow(test_xreg), "baris\n\n")


cat("4. MEMBINA MODEL PADA DATA LATIHAN (80%)\n")
cat(rep("-", 50), "\n")

cat("Melatih model ARIMAX pada ", n_train, " pemerhatian...\n", sep = "")


train_model <- Arima(train_diff,
                     order = c(arima_order[1], 0, arima_order[3]),  
                     xreg = train_xreg,
                     include.mean = FALSE)

cat(" Model dilatih dengan jayanya\n")
cat("  AIC latihan:", round(train_model$aic, 2), "\n")
cat("  BIC latihan:", round(train_model$bic, 2), "\n\n")


cat("5. MERAMAL PADA DATA UJIAN (20%)\n")
cat(rep("-", 50), "\n")

cat("Menjana ramalan ", n_test, "-langkah ke hadapan...\n", sep = "")


fc_diff <- forecast(train_model, h = n_test, xreg = test_xreg)


last_train <- as.numeric(tail(train_original, 1))
fc_original <- last_train + cumsum(fc_diff$mean)
fc_lower <- last_train + cumsum(fc_diff$lower[,2])  
fc_upper <- last_train + cumsum(fc_diff$upper[,2])


fc_ts <- ts(fc_original, 
            start = end(train_original) + c(0, 1),
            frequency = frequency(ts_original))

cat(" Ramalan dihasilkan\n\n")

cat("6. METRIK KETEPATAN DALAM-SAMPEL\n")
cat(rep("-", 50), "\n")


test_errors <- test_original - fc_ts


oos_mae <- mean(abs(test_errors), na.rm = TRUE)
oos_rmse <- sqrt(mean(test_errors^2, na.rm = TRUE))
oos_mape <- mean(abs(test_errors/test_original) * 100, na.rm = TRUE)

cat("Ketepatan Ramalan Dalam-Sampel:\n")
cat(sprintf("  MAE:  $%s\n", format(round(oos_mae), big.mark = ",")))
cat(sprintf("  RMSE: $%s\n", format(round(oos_rmse), big.mark = ",")))
cat(sprintf("  MAPE: %.1f%%\n\n", oos_mape))


cat("Interpretasi MAPE:\n")
if (oos_mape < 10) {
  cat("  Ketepatan ramalan cemerlang\n")
} else if (oos_mape < 20) {
  cat("  Ketepatan ramalan baik\n")
} else if (oos_mape < 30) {
  cat("  Ketepatan ramalan sederhana\n")
} else if (oos_mape < 50) {
  cat("  Ketepatan ramalan lemah\n")
} else {
  cat("  Ketepatan ramalan sangat lemah\n")
}
cat("\n")


cat("7. VISUALISASI: RAMALAN VS SEBENAR\n")
cat(rep("-", 50), "\n")


par(mfrow = c(1, 1), 
    mar = c(5, 6, 4, 2),  
    mgp = c(4.0, 1, 0))   


y_min <- min(c(ts_original, fc_lower), na.rm = TRUE) * 0.98
y_max <- max(c(ts_original, fc_upper), na.rm = TRUE) * 1.02


plot(ts_original, type = "l", col = "gray70", lwd = 2.5,
     main = "Pengesahan Ramalan Dalam-Sampel ARIMAX",
     xlab = "Tahun", 
     ylab = "Nilai CIF ($)",  
     xlim = c(start(train_original)[1], end(ts_original)[1]),
     ylim = c(y_min, y_max),
     cex.lab = 1.1)          

abline(v = time(ts_original)[n_train + 1], 
       col = "gray", lty = 2, lwd = 1.5)


lines(fc_ts, col = "#DC2626", lwd = 3)


polygon(c(time(fc_ts), rev(time(fc_ts))),
        c(fc_lower, rev(fc_upper)),
        col = rgb(0.86, 0.08, 0.24, 0.2), border = NA)


legend("topleft",
       legend = c("Data Latihan", 
                  "Data Ujian",
                  "Ramalan",
                  "Selang Keyakinan"),
       col = c("gray70", "gray70", "#DC2626", 
               rgb(0.86, 0.08, 0.24, 0.2)),
       lwd = c(2.5, 2.5, 3, 10),
       lty = c(1, 1, 1, 1),
       bg = "white",
       cex = 0.8,
       inset = 0.02)


text(x = time(ts_original)[n_train + 1],
     y = par("usr")[4] * 0.98,
     labels = "Ujian",
     col = "darkred",
     cex = 0.8,
     font = 2,
     pos = 4)

text(x = time(ts_original)[n_train],
     y = par("usr")[4] * 0.98,
     labels = "Latihan",
     col = "darkgreen",
     cex = 0.8,
     font = 2,
     pos = 2)


cat("8. MEMBINA MODEL AKHIR PADA 100% DATA\n")
cat(rep("-", 50), "\n")

cat("Sekarang melatih model akhir pada SEMUA ", n_total, " pemerhatian...\n", sep = "")


if (nrow(xreg) == length(final_arimax_auto$ts_diff)) {
  full_xreg <- xreg
} else {
  full_xreg <- xreg[-1, , drop = FALSE]  
}


final_model <- Arima(final_arimax_auto$ts_diff,
                     order = c(arima_order[1], 0, arima_order[3]),
                     xreg = full_xreg,
                     include.mean = FALSE)

cat("  Model akhir dibina pada 100% data\n")
cat("  AIC akhir:", round(final_model$aic, 2), "\n")
cat("  BIC akhir:", round(final_model$bic, 2), "\n\n")


cat("9. MENYIMPAN KEPUTUSAN PENGESAHAN\n")
cat(rep("-", 50), "\n")

validation_results <- list(
  train_data = train_original,
  test_data = test_original,
  train_size = n_train,
  test_size = n_test,
  train_model = train_model,      
  final_model = final_model,      
  forecast = fc_ts,
  forecast_lower = fc_lower,
  forecast_upper = fc_upper,
  out_of_sample_accuracy = c(
    MAE = oos_mae,
    RMSE = oos_rmse,
    MAPE = oos_mape
  ),
  
  arima_order = arima_order,
  test_errors = test_errors,

  information_criteria = c(
    AIC = final_model$aic,
    BIC = final_model$bic
  ),
  
  validation_date = Sys.Date()
)

assign("arimax_proper_validation", validation_results, envir = .GlobalEnv)






cat("Keputusan Pengesahan Dalam-Sampel:\n\n")

cat("1. KAEDAH PENGESAHAN:\n")
cat("   • Dilatih pada ", n_train, " pemerhatian pertama (", 
    round(n_train/n_total*100, 1), "%)\n", sep = "")
cat("   • Diuji pada ", n_test, " pemerhatian terakhir (", 
    round(n_test/n_total*100, 1), "%)\n", sep = "")

cat("\n2. PRESTASI DALAM-SAMPEL:\n")
cat(sprintf("   • MAPE: %.1f%%\n", oos_mape))
cat(sprintf("   • RMSE: $%s\n", format(round(oos_rmse), big.mark = ",")))
cat(sprintf("   • MAE:  $%s\n", format(round(oos_mae), big.mark = ",")))

cat("\n3. MODEL AKHIR (100% DATA):\n")
cat(sprintf("   • AIC: %.1f\n", final_model$aic))
cat(sprintf("   • BIC: %.1f\n", final_model$bic))

# Reset parameter plot
par(mfrow = c(1, 1), mar = c(5.1, 4.1, 4.1, 2.1))