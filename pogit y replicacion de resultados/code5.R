# Code 5 =========================
# Paquetes
# =========================
library(readr)
library(dplyr)

# =========================
# Betas de X1 y X2 completas
# =========================
bh1 <- read_csv("matrices/outputs_empogit1/summarize_bhat_train.csv")
print(bh1, n = Inf, width = Inf)
knitr::kable(bh1, format = "latex", booktabs = TRUE, longtable = TRUE)

# =========================
# Betas de X1 y X2 después de eliminar variables
# =========================

bh2 <- read_csv("matrices/outputs_empogit2/summarize_bhat_train.csv")
print(bh2, n = Inf, width = Inf)
knitr::kable(bh2, format = "latex", booktabs = TRUE, longtable = TRUE)

