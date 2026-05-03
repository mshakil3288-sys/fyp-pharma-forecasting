cat("\n", rep("=", 80), "\n", sep = "")
cat("PEMBINAAN MODEL ARIMAX PADA DATA DIBEZAKAN\n")
cat(rep("=", 80), "\n\n")

library(forecast)


cat("1. PENYEDIAAN DATA\n")
cat(rep("-", 50), "\n")

if (exists("arima_selection")) {
  ts_original <- arima_selection$ts_original
  data_type <- arima_selection$data_type
} else if (exists("ts_winsorized")) {
  ts_original <- ts_winsorized
  data_type <- "Di-Winsorize"
} else {
  stop("Tiada data tersedia")
}

cat("Menggunakan data", data_type, "\n")
cat("Pemerhatian asal:", length(ts_original), "\n")


ts_diff <- diff(ts_original, differences = 1)
cat("  Data dibezakan (d=1):", length(ts_diff), "pemerhatian\n")
cat("  Min ΔCIF:", round(mean(ts_diff), 2), "\n")
cat("  SP ΔCIF:", round(sd(ts_diff), 2), "\n\n")


if (!exists("xreg_matrix")) {
  stop("Tiada pembolehubah patung ditemui")
}


if (nrow(xreg_matrix) == length(ts_original)) {
  xreg <- xreg_matrix[-1, , drop = FALSE]  
  cat("Patung dilaraskan untuk data dibezakan\n")
} else if (nrow(xreg_matrix) == length(ts_diff)) {
  xreg <- xreg_matrix
  cat("Patung sudah sepadan dengan data dibezakan\n")
} else {
  stop("Matriks patung tidak sepadan dengan data")
}

cat("  Pembolehubah patung:", ncol(xreg), "\n\n")


cat("2. MEMBINA MODEL ARIMAX DENGAN AUTO.ARIMA\n")
cat(rep("-", 50), "\n")

arimax_model <- auto.arima(ts_diff,
                           xreg = xreg,
                           seasonal = FALSE,
                           stepwise = TRUE,
                           approximation = FALSE,
                           trace = TRUE,
                           allowdrift = TRUE,
                           ic = "aic",
                           max.order = 7,
                           max.p = 3,
                           max.q = 3,
                           max.d = 0) 
cat("\n AUTO.ARIMA DIPILIH:\n")
cat("  ARIMA(", arimax_model$arma[1], ",0,", arimax_model$arma[2], 
    ") pada data dibezakan\n", sep = "")
cat("  Setara dengan: ARIMA(", arimax_model$arma[1], ",1,", 
    arimax_model$arma[2], ") pada data asal\n", sep = "")
cat("  AIC:", round(arimax_model$aic, 2), "\n")
cat("  BIC:", round(arimax_model$bic, 2), "\n\n")

aicc_value <- arimax_model$aicc

cat("AICc (Akaike Information Criterion corrected):\n")
cat("  AICc =", round(aicc_value, 2), "\n")

cat("3. RINGKASAN MODEL\n")
cat(rep("-", 50), "\n")

coefs <- coef(arimax_model)

cat("Pekali Model (anggaran):\n")
for (nm in names(coefs)) {
  cat(sprintf("  %-15s : %10.4f\n", nm, coefs[nm]))
}

cat("\n")

cat("4. SEMAK DIAGNOSTIK PANTAS\n")
cat(rep("-", 50), "\n")

residuals_model <- residuals(arimax_model)

cat("Statistik Sisa:\n")
cat("  Min:", round(mean(residuals_model), 4), "\n")
cat("  Sisihan Piawai:", round(sd(residuals_model), 4), "\n")


lb_test <- Box.test(residuals_model, lag = 20, type = "Ljung-Box")
cat("\nUjian Ljung-Box (H0: sisa adalah hingar putih):\n")
cat("  p-nilai =", round(lb_test$p.value, 4), "\n")

if (lb_test$p.value > 0.05) {
  cat("  Sisa adalah hingar putih \n")
} else {
  cat("  Sisa menunjukkan autokorelasi\n")
}
cat("\n")

cat("5. MENYIMPAN MODEL\n")
cat(rep("-", 50), "\n")

final_arimax <- list(
  model = arimax_model,
  ts_original = ts_original,
  ts_diff = ts_diff,
  xreg_matrix = xreg,
  data_type = data_type,
  arima_order = c(arimax_model$arma[1], 1, arimax_model$arma[2]),
  model_summary = summary(arimax_model),
  model_date = Sys.Date()
)

assign("final_arimax_auto", final_arimax, envir = .GlobalEnv)

cat(rep("=", 80), "\n\n")