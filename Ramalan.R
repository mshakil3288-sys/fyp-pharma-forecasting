cat("\n", rep("=", 80), "\n", sep = "")
cat("RAMALAN AKHIR: CIF IMPORT FARMASEUTIKAL (18 BULAN)\n")
cat(rep("=", 80), "\n\n")

library(forecast)


yearmonth_format <- function(year, month, freq) {
  if (freq == 12) {
   
    month_names <- c("Jan", "Feb", "Mac", "Apr", "Mei", "Jun", 
                     "Jul", "Ogo", "Sep", "Okt", "Nov", "Dis")
    return(paste(month_names[month], year))
  } else {
    return(paste(year, sprintf("%.2f", month/freq), sep = "."))
  }
}


cat("1. MEMUAT MODEL ARIMAX AKHIR\n")
cat(rep("-", 50), "\n")

if (!exists("final_arimax_auto")) {
  stop("Jalankan pembinaan model ARIMAX dahulu (CHUNK 10)")
}

model <- final_arimax_auto$model
ts_original <- final_arimax_auto$ts_original
xreg <- final_arimax_auto$xreg_matrix

# Dapatkan kekerapan data
data_freq <- frequency(ts_original)
last_year <- end(ts_original)[1]
last_month <- end(ts_original)[2]

cat("Model ARIMAX dimuatkan\n")
cat("  Model: ARIMA(", model$arma[1], ",1,", model$arma[2], 
    ") dengan pematahan struktur\n", sep = "")
cat("  Pemerhatian terakhir: ", 
    yearmonth_format(last_year, last_month, data_freq), 
    " ($", format(round(tail(ts_original, 1)), big.mark = ","), ")\n", sep = "")
cat("  Jumlah data sejarah: ", length(ts_original), " bulan\n\n", sep = "")


cat("2. MENETAPKAN UFUK RAMALAN\n")
cat(rep("-", 50), "\n")

h <- 18  

cat(" Meramal ", h, " bulan ke hadapan (1.5 tahun)\n", sep = "")


fc_start_year <- last_year
fc_start_month <- last_month + 1


if (fc_start_month > data_freq) {
  fc_start_year <- fc_start_year + floor((fc_start_month - 1) / data_freq)
  fc_start_month <- ((fc_start_month - 1) %% data_freq) + 1
}

fc_end_month <- last_month + h
fc_end_year <- last_year + floor((fc_end_month - 1) / data_freq)
fc_end_month <- ((fc_end_month - 1) %% data_freq) + 1

cat("  Tempoh ramalan: ", 
    yearmonth_format(fc_start_year, fc_start_month, data_freq), 
    " hingga ", 
    yearmonth_format(fc_end_year, fc_end_month, data_freq), "\n\n", sep = "")


cat("3. MENYEDIAKAN DATA MASA DEPAN\n")
cat(rep("-", 50), "\n")


future_xreg <- matrix(1, nrow = h, ncol = ncol(xreg))
colnames(future_xreg) <- colnames(xreg)

cat("Matriks xreg masa depan dicipta\n")
cat("  Semua dummy ditetapkan kepada 1 (pematahan struktur telah berlaku)\n")
cat("  Dimensi matriks: ", h, " × ", ncol(xreg), "\n\n", sep = "")


cat("4. MENGHASILKAN RAMALAN\n")
cat(rep("-", 50), "\n")

cat("Menghasilkan ramalan menggunakan model ARIMAX...\n")


fc_diff <- forecast(model, h = h, xreg = future_xreg)


last_value <- as.numeric(tail(ts_original, 1))
fc_mean <- last_value + cumsum(fc_diff$mean)
fc_lower <- last_value + cumsum(fc_diff$lower[,2])  # 95% CI
fc_upper <- last_value + cumsum(fc_diff$upper[,2])


fc_ts <- ts(fc_mean, 
            start = c(fc_start_year, fc_start_month),
            frequency = frequency(ts_original))

fc_lower_ts <- ts(fc_lower,
                  start = c(fc_start_year, fc_start_month),
                  frequency = frequency(ts_original))

fc_upper_ts <- ts(fc_upper,
                  start = c(fc_start_year, fc_start_month),
                  frequency = frequency(ts_original))

cat(" Ramalan dihasilkan dengan jayanya\n\n")


cat("5. KEPUTUSAN RAMALAN\n")
cat(rep("-", 50), "\n")


forecast_dates <- seq(as.Date(paste(last_year, last_month, "01", sep = "-")),
                      by = "month", length.out = h + 1)[-1]


forecast_table <- data.frame(
  Period = 1:h,
  Year = as.numeric(format(forecast_dates, "%Y")),
  Month = format(forecast_dates, "%b"),
  Date = format(forecast_dates, "%b %Y"),
  Point_Forecast = round(fc_mean),
  Lower_95 = round(fc_lower),
  Upper_95 = round(fc_upper),
  CI_Width = round(fc_upper - fc_lower)
)


forecast_table_formatted <- data.frame(
  Period = forecast_table$Period,
  Date = forecast_table$Date,
  Forecast = paste0("$", format(forecast_table$Point_Forecast, big.mark = ",")),
  `95%_CI_Lower` = paste0("$", format(forecast_table$Lower_95, big.mark = ",")),
  `95%_CI_Upper` = paste0("$", format(forecast_table$Upper_95, big.mark = ",")),
  `CI_Width` = paste0("$", format(forecast_table$CI_Width, big.mark = ","))
)

colnames(forecast_table_formatted) <- c("Tempoh", "Bulan", "Ramalan ($)", "Bawah 95% ($)", "Atas 95% ($)", "Lebar CI ($)")

cat("Ramalan Bulanan (1.5 Tahun Akan Datang):\n\n")
print(forecast_table_formatted, row.names = FALSE)


cat("\nStatistik Ringkasan Ramalan (1.5 Tahun Akan Datang):\n")
cat("  Purata ramalan bulanan: $", 
    format(round(mean(fc_mean)), big.mark = ","), "\n", sep = "")
cat("  Minimum ramalan bulanan: $", 
    format(round(min(fc_mean)), big.mark = ","), "\n", sep = "")
cat("  Maksimum ramalan bulanan: $", 
    format(round(max(fc_mean)), big.mark = ","), "\n", sep = "")
cat("  Julat ramalan: $", 
    format(round(min(fc_mean)), big.mark = ","), 
    " hingga $", 
    format(round(max(fc_mean)), big.mark = ","), "\n", sep = "")
cat("  Purata lebar CI: $", 
    format(round(mean(fc_upper - fc_lower)), big.mark = ","), "\n", sep = "")
cat("  Jumlah import diramalkan (18 bulan): $", 
    format(round(sum(fc_mean)), big.mark = ","), "\n", sep = "")
cat("  95% CI untuk jumlah: [$", 
    format(round(sum(fc_lower)), big.mark = ","), 
    ", $", 
    format(round(sum(fc_upper)), big.mark = ","), "]\n\n", sep = "")


cat("Ringkasan Ramalan Suku Tahunan:\n")
quarters <- ceiling(forecast_table$Period / 3)
quarterly_summary <- aggregate(forecast_table$Point_Forecast, 
                               by = list(Quarter = quarters), 
                               FUN = sum)

for (q in 1:nrow(quarterly_summary)) {
  months_range <- paste0("B", (q-1)*3 + 1, "-B", q*3)
  cat(sprintf("  Suku %d (%s): $%s\n", 
              q, 
              months_range,
              format(round(quarterly_summary$x[q]), big.mark = ",")))
}
cat("\n")


cat("6. VISUALISASI RAMALAN\n")
cat(rep("-", 50), "\n")


par(mfrow = c(1, 1), mar = c(5, 6, 4, 2),mgp=c(4,1,0))


y_min <- min(c(ts_original, fc_lower), na.rm = TRUE) * 0.95
y_max <- max(c(ts_original, fc_upper), na.rm = TRUE) * 1.05


x_start <- start(ts_original)[1]
x_end <- end(ts_original)[1] + (h/12) * 1.1


plot(ts_original, type = "l", col = "gray70", lwd = 2.5,
     main = paste("Ramalan CIF Import Farmaseutikal\n",
                  "ARIMAX(", model$arma[1], ",1,", model$arma[2], 
                  ") dengan Pematahan Struktur\n",
                  "Ufuk Ramalan: 18 Bulan (1.5 Tahun)", sep = ""),
     xlab = "Tahun", ylab = "Nilai CIF ($)",
     xlim = c(x_start, x_end),
     ylim = c(y_min, y_max))


lines(fc_ts, col = "#DC2626", lwd = 3)
lines(fc_lower_ts, col = "#DC2626", lty = 2, lwd = 1.5)
lines(fc_upper_ts, col = "#DC2626", lty = 2, lwd = 1.5)


polygon(c(time(fc_ts), rev(time(fc_ts))),
        c(fc_lower, rev(fc_upper)),
        col = rgb(0.86, 0.08, 0.24, 0.2), border = NA)


legend("topleft",
       legend = c("CIF Sejarah", 
                  "Ramalan ARIMAX", 
                  "Selang Keyakinan 95%"),
       col = c("gray70", "#DC2626", rgb(0.86, 0.08, 0.24), "gray"),
       lwd = c(2.5, 3, 10),
       lty = c(1, 1, 1),
       bg = "white",
       cex = 0.9)



if (exists("arimax_validation_simple")) {
  mape_val <- arimax_validation_simple$accuracy["MAPE"]
  accuracy_text <- paste("MAPE Luar-Sampel: ", round(mape_val, 1), "%", sep = "")
  
  text(x = x_start,
       y = y_max * 0.98,
       labels = accuracy_text,
       adj = c(0, 1), cex = 0.8, col = "darkgreen", font = 2)
}

cat("7. MENYIMPAN KEPUTUSAN\n")
cat(rep("-", 50), "\n")

final_forecast <- list(

  model_info = list(
    arima_order = c(model$arma[1], 1, model$arma[2]),
    model_type = "ARIMAX dengan pematahan struktur",
    last_historical_date = c(last_year, last_month),
    last_historical_value = tail(ts_original, 1),
    forecast_horizon = "18 bulan (1.5 tahun)"
  ),
  
  forecast = list(
    horizon = h,
    dates = forecast_dates,
    point_forecast = fc_mean,
    lower_95 = fc_lower,
    upper_95 = fc_upper,
    time_series = fc_ts
  ),
  
  summary = list(
    total_18month = sum(fc_mean),
    total_lower = sum(fc_lower),
    total_upper = sum(fc_upper),
    monthly_average = mean(fc_mean),
    monthly_min = min(fc_mean),
    monthly_max = max(fc_mean),
    avg_ci_width = mean(fc_upper - fc_lower),
    quarterly_totals = quarterly_summary$x
  ),
  
  monthly_table = forecast_table,
  formatted_table = forecast_table_formatted,
  quarterly_table = quarterly_summary,
  
  forecast_date = Sys.Date(),
  generated_by = "Model Ramalan ARIMAX"
)

assign("pharma_import_forecast_18month", final_forecast, envir = .GlobalEnv)




cat("8. INTERPRETASI RAMALAN\n")
cat(rep("-", 50), "\n")

if (h >= 6) {
  initial_trend <- mean(fc_mean[1:3])
  final_trend <- mean(fc_mean[(h-2):h])
  trend_change <- ((final_trend - initial_trend) / initial_trend) * 100
  
  cat("1. ANALISIS TREND:\n")
  if (abs(trend_change) < 5) {
    cat("   Trend stabil dijangka (±", round(abs(trend_change), 1), "% perubahan)\n", sep = "")
  } else if (trend_change > 0) {
    cat("   Trend menaik dijangka (+", round(trend_change, 1), "% peningkatan)\n", sep = "")
  } else {
    cat("   Trend menurun dijangka (", round(trend_change, 1), "% penurunan)\n", sep = "")
  }
}

avg_ci_percent <- mean((fc_upper - fc_lower) / fc_mean) * 100
cat("\n2. PENILAIAN KETIDAKPASTIAN:\n")
cat(sprintf("   • Purata ketidakpastian: ±%.1f%% sekitar ramalan titik\n", avg_ci_percent/2))
if (avg_ci_percent < 20) {
  cat("   Ramalan yang agak tepat\n")
} else if (avg_ci_percent < 40) {
  cat("   Ketidakpastian ramalan sederhana\n")
} else {
  cat("   Ketidakpastian ramalan tinggi \n")
}

if (h >= 12) {
  monthly_avg <- tapply(fc_mean[1:12], forecast_table$Month[1:12], mean)
  if (length(monthly_avg) > 0) {
    top_months <- names(sort(monthly_avg, decreasing = TRUE))[1:3]
    cat("\n3. CORAK MUSIMAN (Tahun Pertama):\n")
    cat("   Import tertinggi biasanya pada: ", paste(top_months, collapse = ", "), "\n", sep = "")
  }
}

cat("\n4. IMPLIKASI PRAKTIKAL:\n")
cat("   • Perancangan bajet: Gunakan $", format(round(mean(fc_mean)), big.mark = ","), 
    " sebagai penanda aras bulanan\n", sep = "")
cat("   • Pengurusan risiko: Bersedia untuk julat $", 
    format(round(min(fc_lower)), big.mark = ","), 
    " hingga $", 
    format(round(max(fc_upper)), big.mark = ","), "\n", sep = "")
cat("   • Perancangan inventori: Jumlah $", 
    format(round(sum(fc_mean)), big.mark = ","), 
    " dijangka dalam 18 bulan\n", sep = "")

cat("\n", rep("=", 80), "\n", sep = "")
cat("RAMALAN 18-BULAN SELESAI ✓\n")
cat(rep("=", 80), "\n\n")

# Reset parameter plot
par(mfrow = c(1, 1), mar = c(5.1, 4.1, 4.1, 2.1))