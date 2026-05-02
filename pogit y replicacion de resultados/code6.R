# code 6=========================
# MAE (SoW y SioW) — sin IDs, sin correlaciones
# =========================
library(readxl)
library(dplyr)
library(tibble)

# ---------- helpers ----------
read_num <- function(path, sheet, col, transform = identity) {
  x <- read_excel(path, sheet = sheet)[[col]]
  x <- suppressWarnings(as.numeric(x))
  transform(x)
}

stop_if_length_mismatch <- function(...) {
  xs <- list(...)
  lens <- vapply(xs, length, integer(1))
  if (length(unique(lens)) != 1L) {
    stop("Length mismatch across vectors. Lengths: ",
         paste(lens, collapse = ", "),
         call. = FALSE)
  }
  invisible(TRUE)
}

mae <- function(y, yhat) mean(abs(y - yhat), na.rm = TRUE)

nmae_range <- function(y, yhat) {
  r <- diff(range(y, na.rm = TRUE))
  if (!is.finite(r) || r <= 0) return(NA_real_)
  mae(y, yhat) / r
}

# =========================
# Importar vectores
# =========================
# Empíricos (empirical_sow.xlsx)
emp_sow  <- read_num("empirical_sow.xlsx", sheet = 2, col = "sow")
emp_siow <- read_num("empirical_sow.xlsx", sheet = 3, col = "siow",
                     transform = function(z) round(z, 0))  # misma regla del paper


# Predichos (Report1.xlsx)
pred_sow1  <- read_excel("matrices/outputs_empogit1/Report1.xlsx", sheet ="theta_test_pred")
pred_siow1 <- read_excel("matrices/outputs_empogit1/Report1.xlsx", sheet ="nhat_test_pred")

# Predichos (Report2.xlsx)
pred_sow2  <- read_excel("matrices/outputs_empogit2/Report2.xlsx", sheet ="theta_test_pred")
pred_siow2 <- read_excel("matrices/outputs_empogit2/Report2.xlsx", sheet ="nhat_test_pred")


# =========================
# Métricas 1 (MAE y NMAE por rango)
# =========================
pred_sow<-pred_sow1$estimate
pred_siow<-round(pred_siow1$estimate,0)
stop_if_length_mismatch(emp_sow, pred_sow, emp_siow, pred_siow)
ok_sow  <- is.finite(emp_sow)  & is.finite(pred_sow)
ok_siow <- is.finite(emp_siow) & is.finite(pred_siow)
cat("Valid pairs SoW :", sum(ok_sow),  "/", length(emp_sow),  "\n")
cat("Valid pairs SioW:", sum(ok_siow), "/", length(emp_siow), "\n")

# Metricas
summary_tbl <- tibble(
  metric     = c("SoW", "SioW"),
  n          = c(sum(ok_sow), sum(ok_siow)),
  MAE        = c(mae(emp_sow[ok_sow],   pred_sow[ok_sow]),
                 mae(emp_siow[ok_siow], pred_siow[ok_siow])),
  NMAE_range = c(nmae_range(emp_sow[ok_sow],   pred_sow[ok_sow]),
                 nmae_range(emp_siow[ok_siow], pred_siow[ok_siow]))
)
print(summary_tbl)

# Diagnóstico mínimo (solo para entender el “gap” SioW empírico vs predicho)
# Útil si te preocupa que emp_siow tenga cola muy larga y pred_siow esté “comprimido”
cat("\nRanges:\n")
cat("SoW  empirical:", range(emp_sow[ok_sow]),  " predicted:", range(pred_sow[ok_sow]),  "\n")
cat("SioW empirical:", range(emp_siow[ok_siow]), " predicted:", range(pred_siow[ok_siow]), "\n")

cat("\nQuantiles (SioW):\n")
print(rbind(
  empirical = quantile(emp_siow[ok_siow], probs = c(.5,.75,.9,.95,.99), na.rm = TRUE),
  predicted = quantile(pred_siow[ok_siow], probs = c(.5,.75,.9,.95,.99), na.rm = TRUE)
))

# Pares comparables: SioW (barras) y SoW (hist)
# --- 1) Truncar SioW a 40 (40+) ---
cap_siow <- function(x, cap = 40L) {
  x2 <- x
  x2[x2 > cap] <- cap
  x2
}

emp_siow_cap  <- cap_siow(emp_siow[ok_siow], 40L)
pred_siow_cap <- cap_siow(pred_siow[ok_siow], 40L)

# Convertir a factor con niveles 0,1,2,...,39,"40+"
levels_siow <- c(as.character(0:39), "40+")
to_siow_factor <- function(xcap) {
  lab <- ifelse(xcap >= 40, "40+", as.character(xcap))
  factor(lab, levels = levels_siow)
}

emp_siow_f  <- to_siow_factor(emp_siow_cap)
pred_siow_f <- to_siow_factor(pred_siow_cap)

# Tablas completas (incluye ceros en niveles ausentes)
tab_emp_siow  <- table(emp_siow_f)
tab_pred_siow <- table(pred_siow_f)

# Eje Y común para SioW
ylim_siow_bar <- c(0, max(tab_emp_siow, tab_pred_siow))

# --- 2) SoW: histogramas comparables (mismo rango X, mismos breaks, mismo Y) ---
sow_emp  <- emp_sow[ok_sow]
sow_pred <- pred_sow[ok_sow]

breaks_sow <- seq(0, 1, by = 0.05)

h_emp  <- hist(sow_emp,  breaks = breaks_sow, plot = FALSE)
h_pred <- hist(sow_pred, breaks = breaks_sow, plot = FALSE)
ylim_sow_hist <- c(0, max(h_emp$counts, h_pred$counts))

# --- 3) Plot (2x2) ---
par(mfrow = c(2,2), mar = c(4,4,3,1))

# SioW (barras)
barplot(tab_emp_siow,
        main = "Empirical",
        xlab = "SioW",
        ylab = "Frequency",
        ylim = ylim_siow_bar,
        las  = 2, cex.names = 0.7)

barplot(tab_pred_siow,
        main = "Estimated",
        xlab = "SioW",
        ylab = "Frequency",
        ylim = ylim_siow_bar,
        las  = 2, cex.names = 0.7)

# SoW (histogramas)
hist(sow_emp,
     breaks = breaks_sow,
     main   = "Empirical",
     xlab   = "SoW",
     ylab   = "Frequency",
     xlim   = c(0,1),
     ylim   = ylim_sow_hist)

hist(sow_pred,
     breaks = breaks_sow,
     main   = "Estimated",
     xlab   = "SoW",
     ylab   = "Frequency",
     xlim   = c(0,1),
     ylim   = ylim_sow_hist)

par(mfrow = c(1,1))


# =========================
# Métricas 2 (MAE y NMAE por rango)
# =========================
pred_sow<-pred_sow2$estimate
pred_siow<-round(pred_siow2$estimate,0)
stop_if_length_mismatch(emp_sow, pred_sow, emp_siow, pred_siow)
ok_sow  <- is.finite(emp_sow)  & is.finite(pred_sow)
ok_siow <- is.finite(emp_siow) & is.finite(pred_siow)
cat("Valid pairs SoW :", sum(ok_sow),  "/", length(emp_sow),  "\n")
cat("Valid pairs SioW:", sum(ok_siow), "/", length(emp_siow), "\n")

# Metricas
summary_tbl <- tibble(
  metric     = c("SoW", "SioW"),
  n          = c(sum(ok_sow), sum(ok_siow)),
  MAE        = c(mae(emp_sow[ok_sow],   pred_sow[ok_sow]),
                 mae(emp_siow[ok_siow], pred_siow[ok_siow])),
  NMAE_range = c(nmae_range(emp_sow[ok_sow],   pred_sow[ok_sow]),
                 nmae_range(emp_siow[ok_siow], pred_siow[ok_siow]))
)
print(summary_tbl)

# Diagnóstico mínimo (solo para entender el “gap” SioW empírico vs predicho)
# Útil si te preocupa que emp_siow tenga cola muy larga y pred_siow esté “comprimido”
cat("\nRanges:\n")
cat("SoW  empirical:", range(emp_sow[ok_sow]),  " predicted:", range(pred_sow[ok_sow]),  "\n")
cat("SioW empirical:", range(emp_siow[ok_siow]), " predicted:", range(pred_siow[ok_siow]), "\n")

cat("\nQuantiles (SioW):\n")
print(rbind(
  empirical = quantile(emp_siow[ok_siow], probs = c(.5,.75,.9,.95,.99), na.rm = TRUE),
  predicted = quantile(pred_siow[ok_siow], probs = c(.5,.75,.9,.95,.99), na.rm = TRUE)
))

# Pares comparables: SioW (barras) y SoW (hist)
# --- 1) Truncar SioW a 40 (40+) ---
cap_siow <- function(x, cap = 40L) {
  x2 <- x
  x2[x2 > cap] <- cap
  x2
}

emp_siow_cap  <- cap_siow(emp_siow[ok_siow], 40L)
pred_siow_cap <- cap_siow(pred_siow[ok_siow], 40L)

# Convertir a factor con niveles 0,1,2,...,39,"40+"
levels_siow <- c(as.character(0:39), "40+")
to_siow_factor <- function(xcap) {
  lab <- ifelse(xcap >= 40, "40+", as.character(xcap))
  factor(lab, levels = levels_siow)
}

emp_siow_f  <- to_siow_factor(emp_siow_cap)
pred_siow_f <- to_siow_factor(pred_siow_cap)

# Tablas completas (incluye ceros en niveles ausentes)
tab_emp_siow  <- table(emp_siow_f)
tab_pred_siow <- table(pred_siow_f)

# Eje Y común para SioW
ylim_siow_bar <- c(0, max(tab_emp_siow, tab_pred_siow))

# --- 2) SoW: histogramas comparables (mismo rango X, mismos breaks, mismo Y) ---
sow_emp  <- emp_sow[ok_sow]
sow_pred <- pred_sow[ok_sow]

breaks_sow <- seq(0, 1, by = 0.05)

h_emp  <- hist(sow_emp,  breaks = breaks_sow, plot = FALSE)
h_pred <- hist(sow_pred, breaks = breaks_sow, plot = FALSE)
ylim_sow_hist <- c(0, max(h_emp$counts, h_pred$counts))

# --- 3) Plot (2x2) ---
par(mfrow = c(2,2), mar = c(4,4,3,1))

# SioW (barras)
barplot(tab_emp_siow,
        main = "Empirical",
        xlab = "SioW",
        ylab = "Frequency",
        ylim = ylim_siow_bar,
        las  = 2, cex.names = 0.7)

barplot(tab_pred_siow,
        main = "Estimated",
        xlab = "SioW",
        ylab = "Frequency",
        ylim = ylim_siow_bar,
        las  = 2, cex.names = 0.7)

# SoW (histogramas)
hist(sow_emp,
     breaks = breaks_sow,
     main   = "Empirical",
     xlab   = "SoW",
     ylab   = "Frequency",
     xlim   = c(0,1),
     ylim   = ylim_sow_hist)

hist(sow_pred,
     breaks = breaks_sow,
     main   = "Estimated",
     xlab   = "SoW",
     ylab   = "Frequency",
     xlim   = c(0,1),
     ylim   = ylim_sow_hist)

par(mfrow = c(1,1))




