cat("\n", rep("=", 80), "\n", sep = "")
cat("UJIAN KEPEGUNAN - ADF & KPSS\n")
cat(rep("=", 80), "\n\n")

library(forecast)
library(tseries)


cat("1. PENYEDIAAN DATA\n")
cat(rep("-", 50), "\n")


if (exists("dummy_info")) {
  ts_to_test <- dummy_info$ts_data
  data_type <- dummy_info$data_type
  cat("Menggunakan data", data_type, "dari dummy_info\n")
} else if (exists("ts_winsorized")) {
  ts_to_test <- ts_winsorized
  data_type <- "Di-Winsorize"
  cat("Menggunakan data WINSORISASI\n")
} else {
  ts_to_test <- ts_data
  data_type <- "Asal"
  cat("Menggunakan data asal (data WINSORISASI tidak ditemui)\n")
}

cat("  Pemerhatian:", length(ts_to_test), "\n")
cat("  Kekerapan:", frequency(ts_to_test), "\n")
cat("  Tempoh:", start(ts_to_test)[1], "hingga", end(ts_to_test)[1], "\n\n")


# ======================================
cat("UJIAN DICKEY-FULLER DITAMBAH (ADF)\n")
cat(rep("-", 50), "\n")

cat("Hipotesis Nol (H0): Siri mempunyai punca unit (tidak PEGUN)\n")
cat("Alternatif (H1): Siri adalah PEGUN\n")
cat("Aras keertian: α = 0.05\n\n")

tryCatch({
  adf_result <- adf.test(ts_to_test, alternative = "PEGUN")
  
  cat("Keputusan Ujian ADF:\n")
  cat("  Statistik ujian (Dickey-Fuller):", round(adf_result$statistic, 3), "\n")
  cat("  p-nilai:", format.pval(adf_result$p.value, digits = 3), "\n")
  cat("  Tertib lengah:", adf_result$parameter, "\n")
  cat("  Saiz sampel:", length(ts_to_test), "\n\n")
  

  if (adf_result$p.value < 0.05) {
    cat("KEPUTUSAN: TOLAK H0\n")
    cat("   Bukti: p-nilai < 0.05\n")
    cat("   Kesimpulan: Siri adalah PEGUN (tiada punca unit)\n")
    cat("   → Pembezaan mungkin TIDAK diperlukan (d = 0)\n")
    adf_stationary <- TRUE
  } else {
    cat("KEPUTUSAN: GAGAL TOLAK H0\n")
    cat("   Bukti: p-nilai ≥ 0.05\n")
    cat("   Kesimpulan: Siri adalah TIDAK PEGUN (mempunyai punca unit)\n")
    cat("   → Pembezaan DIPERLUKAN (d > 0)\n")
    adf_stationary <- FALSE
  }
  
}, error = function(e) {
  cat("Ujian ADF Gagal:", e$message, "\n")
  adf_stationary <- NA
})

cat("\n")

cat("3. UJIAN KPSS\n")
cat(rep("-", 50), "\n")

cat("Hipotesis Nol (H0): Siri adalah PEGUN\n")
cat("Alternatif (H1): Siri tidak PEGUN\n")
cat("Aras keertian: α = 0.05\n\n")

tryCatch({
  kpss_result <- kpss.test(ts_to_test, null = "Level")
  
  cat("Keputusan Ujian KPSS:\n")
  cat("  Statistik ujian (KPSS):", round(kpss_result$statistic, 3), "\n")
  cat("  p-nilai:", format.pval(kpss_result$p.value, digits = 3), "\n")
  cat("  Parameter lengah potongan:", kpss_result$parameter, "\n\n")
  
 
  if (kpss_result$p.value > 0.05) {
    cat("KEPUTUSAN: GAGAL TOLAK H0\n")
    cat("   Bukti: p-nilai > 0.05\n")
    cat("   Kesimpulan: Siri adalah PEGUN\n")
    cat("   → Tiada keperluan pembezaan dari perspektif KPSS\n")
    kpss_stationary <- TRUE
  } else {
    cat("KEPUTUSAN: TOLAK H0\n")
    cat("   Bukti: p-nilai ≤ 0.05\n")
    cat("   Kesimpulan: Siri TIDAK PEGUN\n")
    cat("   → Pembezaan mungkin diperlukan\n")
    kpss_stationary <- FALSE
  }
  
}, error = function(e) {
  cat("Ujian KPSS Gagal:", e$message, "\n")
  kpss_stationary <- NA
})

cat("\n")


# ============================================
cat("4. PENILAIAN KEPEGUNAN AKHIR\n")
cat(rep("-", 50), "\n")


comparison <- data.frame(
  Ujian = c("ADF", "KPSS"),
  H0 = c("Punca unit (tidak PEGUN)", "Stasioner aras"),
  Keputusan = c(ifelse(adf_stationary, "Tolak H0", "Gagal tolak H0"),
                ifelse(kpss_stationary, "Gagal tolak H0", "Tolak H0")),
  Kesimpulan = c(ifelse(adf_stationary, "PEGUN", "Tidak PEGUN"),
                 ifelse(kpss_stationary, "PEGUN", "Tidak PEGUN")),
  Pembezaan = c(ifelse(adf_stationary, "Tidak perlu", "Perlu"),
                ifelse(kpss_stationary, "Tidak perlu", "Perlu"))
)

cat("Perbandingan Ujian:\n")
print(comparison, row.names = FALSE)
cat("\n")


if (!is.na(adf_stationary) && !is.na(kpss_stationary)) {
  if (adf_stationary && kpss_stationary) {
    cat("KEDUA-DUA UJIAN BERSETUJU: SIRI ADALAH PEGUN\n")
    cat("   Cadangan: Gunakan d = 0 (tiada pembezaan)\n")
    final_d <- 0
  }  else {
    cat("UJIAN TIDAK BERSETUJU: Keputusan tidak konklusif\n")
    cat("   ADF berkata:", ifelse(adf_stationary, "Stasioner", "Tidak stasioner"), "\n")
    cat("   KPSS berkata:", ifelse(kpss_stationary, "Stasioner", "Tidak stasioner"), "\n")
    cat("   Cadangan: Semak plot ACF dan gunakan d = 1 sebagai langkah berjaga-jaga\n")
    cat("   Atau uji siri dibezakan\n")
    final_d <- 1  # Pilihan konservatif
  }
} 
cat("\n")


if (final_d > 0) {
  cat("5. MENGUJI SIRI DIBEZAKAN (d = 1)\n")
  cat(rep("-", 50), "\n")
  
  ts_diff <- diff(ts_to_test)
  
  cat("Siri pembezaan pertama dicipta\n")
  cat("  Panjang asal:", length(ts_to_test), "\n")
  cat("  Panjang dibezakan:", length(ts_diff), "\n")
  cat("  Min perbezaan:", round(mean(ts_diff, na.rm = TRUE), 2), "\n\n")
  
  
  tryCatch({
    adf_diff <- adf.test(ts_diff, alternative = "stationary")
    cat("Ujian ADF pada siri dibezakan:\n")
    cat("  p-nilai:", format.pval(adf_diff$p.value, digits = 3), "\n")
    
    if (adf_diff$p.value < 0.05) {
      cat("   Siri dibezakan adalah stasioner\n")
      cat("   Mengesahkan d = 1 adalah sesuai\n")
    } else {
      cat("   Siri dibezakan mungkin masih tidak stasioner\n")
      cat("   Pertimbangkan d = 2 atau semak isu lain\n")
    }
  }, error = function(e) {
    cat("  Ujian ADF pada siri dibezakan gagal\n")
  })
}



cat("\n6. MENYIMPAN KEPUTUSAN\n")
cat(rep("-", 50), "\n")

stationarity_results <- list(
  data_used = data_type,
  ts_original = ts_to_test,
  adf_result = if(exists("adf_result")) adf_result else NULL,
  kpss_result = if(exists("kpss_result")) kpss_result else NULL,
  adf_stationary = if(exists("adf_stationary")) adf_stationary else NA,
  kpss_stationary = if(exists("kpss_stationary")) kpss_stationary else NA,
  recommended_d = final_d,
  ts_differenced = if(exists("ts_diff")) ts_diff else NULL,
  test_date = Sys.Date()
)

