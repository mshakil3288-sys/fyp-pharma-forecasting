library(strucchange)

cat("\n", rep("=", 80), "\n", sep = "")
cat("UJIAN PEMATAHAN STRUKTUR BAI-PERRON\n")
cat("Nilai CIF Import Farmaseutikal (DATA DI-WINSORIZE)\n")
cat(rep("=", 80), "\n\n")   

if(!exists("ts_winsorized")) {
  cat(" ts_winsorized tidak ditemui!\n")
  cat(" Menggunakan data asal ts_data sebagai gantian.\n")
  cat(" Jalankan rawatan pencilan dahulu untuk keputusan lebih baik.\n\n")
  analysis_series <- ts_data
  data_type <- "Data Asal"
} else {
  analysis_series <- ts_winsorized
  data_type <- "Data Di-Winsorize"
}


y <- as.numeric(analysis_series)
x <- time(analysis_series)
n <- length(y)

decimal_to_date <- function(decimal_year) {
  year <- floor(decimal_year)
  month <- round((decimal_year - year) * 12) + 1
  month <- ifelse(month > 12, 12, ifelse(month < 1, 1, month))
  month_names <- c("Jan", "Feb", "Mac", "Apr", "Mei", "Jun",
                   "Jul", "Ogo", "Sep", "Okt", "Nov", "Dis")
  return(paste(month_names[month], year))
}


cat("RINGKASAN DATA (", data_type, "):\n", sep = "")
cat(rep("-", 60), "\n")
cat("Tempoh:", decimal_to_date(min(x)), "hingga", decimal_to_date(max(x)), "\n")
cat("Bulan:", n, "\n")
cat("Min: $", format(round(mean(y)), big.mark = ","), "\n", sep = "")
cat("SP: $", format(round(sd(y)), big.mark = ","), "\n", sep = "")
cat("Median: $", format(round(median(y)), big.mark = ","), "\n\n", sep = "")

cat("MENJALANKAN UJIAN BAI-PERRON...\n")
cat(rep("-", 60), "\n")

bp_result <- breakpoints(y ~ x, h = 0.15, breaks = 5)

calculate_bic <- function(n_breaks) {
  if (n_breaks == 0) {
    mod <- lm(y ~ x)
    rss <- sum(residuals(mod)^2)
    k <- 2
  } else {
    bp_temp <- breakpoints(y ~ x, breaks = n_breaks, h = 0.15)
    if (all(is.na(bp_temp$breakpoints))) {
      return(NA)
    }
    segment_factor <- factor(cut(x, breaks = c(min(x)-0.1, x[bp_temp$breakpoints], max(x)+0.1)))
    mod <- lm(y ~ x * segment_factor)
    rss <- sum(residuals(mod)^2)
    k <- 2 + 2 * n_breaks
  }
  bic_value <- n * log(rss/n) + k * log(n)
  return(bic_value)
}

bic_values <- sapply(0:5, calculate_bic)
optimal_breaks <- which.min(bic_values) - 1

if (optimal_breaks > 0) {
  bp_optimal <- breakpoints(y ~ x, breaks = optimal_breaks, h = 0.15)
  break_indices <- bp_optimal$breakpoints
  break_dates <- x[break_indices]
  
  ci <- confint(bp_optimal, level = 0.95)
  ci_lower <- x[ci$confint[, 1]]
  ci_upper <- x[ci$confint[, 3]]
  
} else {
  break_indices <- NULL
  break_dates <- NULL
  ci_lower <- NULL
  ci_upper <- NULL
}


cat("\nKEPUTUSAN:\n")
cat(rep("-", 60), "\n")
cat("Nilai BIC:\n")
for (i in 0:5) {
  cat(sprintf("  %d pematahan: BIC = %.1f", i, bic_values[i+1]))
  if (i == optimal_breaks) cat("  ← OPTIMUM")
  cat("\n")
}
cat("\n")

if (optimal_breaks > 0) {
  cat("Pematahan struktur dikesan:", optimal_breaks, "\n\n")
  
  for (i in 1:optimal_breaks) {
    cat(sprintf("Pematahan %d: %s (Bulan %d dari %d)\n",
                i, decimal_to_date(break_dates[i]), break_indices[i], n))
    cat(sprintf("  Selang Keyakinan 95%%: %s hingga %s\n",
                decimal_to_date(ci_lower[i]), decimal_to_date(ci_upper[i])))
  }
  
  
  segment_boundaries <- c(1, break_indices, n)
  n_segments <- length(segment_boundaries) - 1
  
  cat("\nSTATISTIK SEGMEN:\n")
  cat(rep("-", 60), "\n")
  
  for (seg in 1:n_segments) {
    idx <- segment_boundaries[seg]:segment_boundaries[seg+1]
    seg_mean <- mean(y[idx])
    seg_sd <- sd(y[idx])
    seg_length <- length(idx)
    
    cat(sprintf("Segmen %d: %s hingga %s\n",
                seg,
                decimal_to_date(x[segment_boundaries[seg]]),
                decimal_to_date(x[segment_boundaries[seg+1]])))
    cat(sprintf("  Tempoh: %d bulan (%.1f tahun)\n", seg_length, seg_length/12))
    cat(sprintf("  Min: $%s\n", format(round(seg_mean), big.mark = ",")))
    cat(sprintf("  SP: $%s\n", format(round(seg_sd), big.mark = ",")))
    cat(sprintf("  CV: %.1f%%\n\n", (seg_sd/seg_mean) * 100))
  }
  
} else {
  cat(" Tiada pematahan struktur dikesan\n")
}

cat("\n", rep("=", 80), "\n", sep = "")
cat("MENCIPTA VISUALISASI...\n")
cat(rep("=", 80), "\n\n")


par(mar = c(5, 8.5, 3.5, 2) + 0.1,   
    mgp = c(2.8, 1.2, 0),            
    las = 1,                         
    cex.main = 0.9,
    font.main = 2,
    cex.lab = 1.0,
    cex.axis = 0.9)


col_main <- "#1E3A8A"
col_break <- "#DC2626"
col_ci <- "#FECACA"
col_grid <- "#F3F4F6"




par(
  mar = c(5, 11, 3.5, 2) + 0.1,   
  mgp = c(2.8, 1.2, 0),           
  las = 1,                        
  cex.main = 0.9,
  font.main = 2,
  cex.lab = 1.0,
  cex.axis = 0.9
)

plot(analysis_series,
     main = "Nilai CIF Import Farmaseutikal (Bulanan)",
     ylab = "",                    
     xlab = "Tahun",
     col  = col_main,
     lwd  = 2.0,
     frame.plot = FALSE,
     ylim = c(min(y) * 0.9, max(y) * 1.1),
     panel.first = {
       grid(col = col_grid, lty = 1, lwd = 0.5)
       abline(h = axTicks(2), col = col_grid, lty = 1, lwd = 0.5)
     })


op <- par(las = 0)                
mtext("Nilai CIF (USD)", side = 2, line = 8, cex = 1.0)
par(op)


if (optimal_breaks > 0) {
  
  for (i in 1:optimal_breaks) {
    rect(ci_lower[i], min(y) * 0.9,
         ci_upper[i], max(y) * 1.1,
         col = col_ci, border = NA)
    
    abline(v = break_dates[i], col = col_break, lty = 2, lwd = 2)
    
    y_at_break <- y[which.min(abs(x - break_dates[i]))]
    points(break_dates[i], y_at_break,
           pch = 21, bg = col_break, col = "white", cex = 1.5, lwd = 1.5)
  }
  
  
  
  par(xpd = FALSE)
  
 
  legend("topleft",
         legend = c("Nilai CIF", "Titik Pematahan", "95% CI"),
         col = c(col_main, col_break, NA),
         lwd = c(2.5, 2, NA),
         lty = c(1, 2, NA),
         pch = c(NA, 21, 15),
         pt.bg = c(NA, col_break, col_ci),
         pt.cex = c(NA, 1.2, 1.5),
         bg = "white",
         box.col = "gray80",
         cex = 0.8,
         inset = 0.02,
         ncol = 1)
  
} else {
  legend("topleft",
         legend = paste("Nilai CIF\n", data_type, sep = ""),
         col = col_main,
         lwd = 2.5,
         bg = "white",
         box.col = "gray80",
         cex = 0.85)
}


col_bic <- "#059669"
col_opt <- "#DC2626"
col_grid <- "#F3F4F6"


bic_df <- data.frame(
  Breaks = 0:5,
  BIC = bic_values
)


plot(bic_df$Breaks, bic_df$BIC,
     type = "b",
     pch = 21,
     bg = ifelse(bic_df$Breaks == optimal_breaks, col_opt, "white"),
     col = col_bic,
     main = "Nilai BIC untuk\nPemilihan Pematahan Struktur",
     xlab = "Bilangan Pematahan Struktur",
     ylab = "Nilai BIC",
     lwd = 2.5,
     cex = 1.3,
     frame.plot = FALSE,
     xlim = c(-0.2, 5.2),
     ylim = c(min(bic_values, na.rm = TRUE) * 0.995,
              max(bic_values, na.rm = TRUE) * 1.005),
     panel.first = {
       grid(col = col_grid, lty = 1, lwd = 0.5)
       abline(h = axTicks(2), col = col_grid, lty = 1, lwd = 0.5)
     })


if (optimal_breaks >= 0) {
  points(optimal_breaks, bic_values[optimal_breaks + 1],
         col = col_opt, pch = 1, cex = 2.2, lwd = 2)
}


for (i in 0:5) {
  if (!is.na(bic_values[i+1])) {
    pos_val <- ifelse(i %in% c(0, 1, 2), 3, 1)
    if (i == optimal_breaks) pos_val <- 3
    
    text(i, bic_values[i+1],
         labels = round(bic_values[i+1], 1),
         pos = pos_val,
         cex = 0.75,
         col = ifelse(i == optimal_breaks, col_opt, "darkgreen"),
         font = ifelse(i == optimal_breaks, 2, 1))
  }
}


if (optimal_breaks >= 0) {
  text(optimal_breaks, bic_values[optimal_breaks + 1],
       labels = paste("Optimum:", optimal_breaks,
                      ifelse(optimal_breaks == 1, "pematahan", "pematahan")),
       pos = ifelse(optimal_breaks < 3, 4, 2),
       col = col_opt,
       cex = 0.75,
       font = 2)
}


legend("topright",
       legend = c("Nilai BIC", "Model Optimum"),
       pch = c(21, 21),
       pt.bg = c("white", col_opt),
       col = c(col_bic, col_opt),
       pt.cex = c(1.3, 1.3),
       pt.lwd = c(1, 1),
       bg = "white",
       box.col = "gray80",
       cex = 0.8,
       inset = 0.02)


par(mfrow = c(1, 1), mar = c(5.1, 4.1, 4.1, 2.1))


if (optimal_breaks > 0) {
  break_info <- list(
    n_breaks = optimal_breaks,
    break_dates = break_dates,
    break_indices = break_indices,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    decimal_to_date = decimal_to_date,
    data_used = data_type,
    analysis_series = analysis_series
  )
  cat("\nMaklumat pematahan disimpan untuk mencipta pembolehubah dummy.\n")
  cat("Langkah seterusnya: Cipta pembolehubah dummy langkah untuk pemodelan ARIMAX.\n")
}

rm(y, x, n, bp_result, calculate_bic, bic_values, bic_df, analysis_series)

