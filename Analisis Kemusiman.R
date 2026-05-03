cat("\n", rep("=", 80), "\n", sep = "")
cat("ANALISIS KEMUSIMAN - DATA DI-WINSORIZE\n")
cat(rep("=", 80), "\n\n")

library(forecast)

cat("1. PEMILIHAN DATA\n")
cat(rep("-", 50), "\n\n")

if(exists("ts_winsorized")) {
  analysis_data <- ts_winsorized
  data_type <- "Data Di-Winsorize"
  cat("Menggunakan data DI-WINSORIZE\n\n")
} else {
  analysis_data <- ts_data
  data_type <- "Data Asal"
  cat("Menggunakan data ASAL\n\n")
}


cat("2. ANALISIS KEKUATAN KEMUSIMAN\n")
cat(rep("-", 50), "\n\n")


start_info <- start(analysis_data)
end_info <- end(analysis_data)
freq_info <- frequency(analysis_data)

start_year <- start_info[1]
start_month <- ifelse(length(start_info) > 1, start_info[2], 1)
end_year <- end_info[1]
end_month <- ifelse(length(end_info) > 1, end_info[2], 12)

month_name_start <- month.abb[start_month]
month_name_end <- month.abb[end_month]

cat("Maklumat Siri:\n")
cat("  Tempoh: ", month_name_start, " ", start_year, " hingga ", 
    month_name_end, " ", end_year, "\n", sep = "")
cat("  Kekerapan: ", freq_info, " (", 
    ifelse(freq_info == 12, "Bulanan", 
           ifelse(freq_info == 4, "Suku Tahunan", 
                  ifelse(freq_info == 1, "Tahunan", "Khas"))), ")\n", sep = "")
cat("  Pemerhatian: ", length(analysis_data), "\n", sep = "")


strength <- NA
category <- "TIDAK DIKETAHUI"
recommendation <- "Semak secara manual"
use_sarima <- FALSE


cat("\nMenjalankan penguraian STL...\n")

tryCatch({
  if(freq_info > 1) {
    stl_result <- stl(analysis_data, s.window = "periodic", robust = TRUE)
    
    seasonal <- stl_result$time.series[, "seasonal"]
    remainder <- stl_result$time.series[, "remainder"]
    
    
    var_seasonal <- var(seasonal, na.rm = TRUE)
    var_total <- var(seasonal + remainder, na.rm = TRUE)
    
    if(var_total > 0) {
      strength <- var_seasonal / var_total
      cat("Kekuatan kemusiman dikira: ", round(strength, 4), "\n", sep = "")
    } else {
      strength <- 0
      cat("Varians sifar dalam siri yang dinyahkecenderungan\n")
    }
    
   
    if(strength >= 0.6) {
      category <- "KUAT"
      recommendation <- "PASTI gunakan SARIMA"
      use_sarima <- TRUE
    } else if(strength >= 0.4) {
      category <- "SEDERHANA"
      recommendation <- "Gunakan SARIMA"
      use_sarima <- TRUE
    } else if(strength >= 0.2) {
      category <- "LEMAH"
      recommendation <- "Pertimbangkan SARIMA atau ARIMA"
      use_sarima <- TRUE
    } else if(strength >= 0.05) {
      category <- "SANGAT LEMAH"
      recommendation <- "Kemungkinan gunakan ARIMA"
      use_sarima <- FALSE
    } else {
      category <- "DIABAIKAN"
      recommendation <- "Gunakan ARIMA (tiada kemusiman)"
      use_sarima <- FALSE
    }
    
    
    par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))
    
   
   
    plot(stl_result, 
         main = "Penguraian STL",
         cex.main = 0.85,   # <-- kecilkan tajuk (cuba 0.7 - 0.9)
         col = c("black", "red", "blue", "green"))
    
    plot(seasonal, 
         main = paste("Komponen Musiman\nKekuatan:", round(strength, 3)),
         cex.main = 0.85,   # <-- kecilkan tajuk (cuba 0.7 - 0.9)
         ylab = "Kesan Musiman ($)",
         xlab = "Tahun",
         col = "#059669",
         lwd = 1.5)
    
    
    month_indices <- cycle(analysis_data)
    monthly_avgs <- numeric(12)
    for(m in 1:12) {
      month_values <- analysis_data[month_indices == m]
      monthly_avgs[m] <- mean(month_values, na.rm = TRUE)
    }
    

    
  } else {
    cat(" Kekerapan siri adalah 1 (tidak bermusim)\n")
    strength <- 0
    category <- "TIDAK BERMUSIM"
    recommendation <- "Gunakan ARIMA (tiada kemusiman)"
    use_sarima <- FALSE
    
   
    par(mfrow = c(1, 1))
    plot(analysis_data, 
         main = paste(data_type, "Tidak Bermusim"),
         ylab = "Nilai CIF ($)",
         xlab = "Tahun",
         col = "#1E3A8A",
         lwd = 2)
  }
  
}, error = function(e) {
  cat("Ralat dalam analisis: ", e$message, "\n")
  
  
  par(mfrow = c(1, 1))
  plot(analysis_data, 
       main = paste("Ralat dalam analisis:", e$message),
       ylab = "Nilai CIF ($)",
       xlab = "Tahun",
       col = "red",
       lwd = 1.5)
})


result <- list(
  strength = ifelse(exists("strength"), strength, NA),
  category = ifelse(exists("category"), category, "RALAT"),
  recommendation = ifelse(exists("recommendation"), recommendation, "Semak secara manual"),
  use_sarima = ifelse(exists("use_sarima"), use_sarima, FALSE)
)


cat("\n=== KEPUTUSAN KEKUATAN KEMUSIMAN ===\n")
cat("Kekuatan Kemusiman: ", round(result$strength, 4), "\n", sep = "")
cat("Kategori: ", result$category, "\n", sep = "")
cat("Varians Diterangkan: ", round(result$strength * 100, 1), "%\n", sep = "")
cat("\nCadangan: ", result$recommendation, "\n", sep = "")


cat("\n3. KEPUTUSAN PEMODELAN\n")
cat(rep("-", 50), "\n\n")

cat("PENILAIAN KEMUSIMAN:\n")
cat("  Kekuatan: ", round(result$strength, 3), "\n", sep = "")
cat("  Kategori: ", result$category, "\n", sep = "")

if(result$use_sarima) {
  cat("\n EPUTUSAN AKHIR: GUNA MODEL SARIMA\n")
  cat("   Sebab: ", result$recommendation, "\n", sep = "")
} else {
  cat("\n KEPUTUSAN AKHIR: GUNA MODEL ARIMA STANDARD\n")
  cat("   Sebab: ", result$recommendation, "\n", sep = "")
}


par(mfrow = c(1, 1), mar = c(5.1, 4.1, 4.1, 2.1))

seasonality_decision <- result

cat("\n4. KEPUTUSAN DISIMPAN\n")
cat(rep("-", 50), "\n\n")


cat("\n", rep("=", 80), "\n", sep = "")
cat("ANALISIS KEMUSIMAN SELESAI ✓\n")
cat(rep("=", 80), "\n\n")