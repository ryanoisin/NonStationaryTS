# jonashaslbeck@protonmail.com; July 9th, 2023

# ---------------------------------------------------
# ---------- What is happening here? ----------------
# ---------------------------------------------------

# Showing the results of applying detrending and differenceing
# to data with deterministic and stochastic trends

# ---------------------------------------------------
# ---------- Load packages & source -----------------
# ---------------------------------------------------

source("aux_functions.R")

# ---------------------------------------------------
# ---------- Simulate -------------------------------
# ---------------------------------------------------

# Detrending vs Differencing; sampling distribution of phi (2x2)

# ---------------------------- Generating Data ----------------------------

# Storage
nIter <- 10000
m_phi_r <- matrix(NA, nIter, 2) # residualized (detrend)
m_phi_d <- matrix(NA, nIter, 2) # differenced
m_phi_d2 <- matrix(NA, nIter, 2) # differenced (alternative)
m_phi_a <- matrix(NA, nIter, 2) # differenced (alternative)

N <- 100

set.seed(1)

for(k in 1:nIter) {
  
  # ------ 1) Deterministic Trend & de-trend -----
  x1 <- genAR(n = N, phi = .5, init = 0, ltrend = .1, noise = 1)
  # detrend
  x1r <- detrend(x1)
  # fit model to residuals
  out <- lm(x1r[-1] ~ x1r[-N])
  m_phi_r[k,1] <- out$coefficients[2]
  
  # ------ 2) Stochastic Trend with drift & detrend -----
  x2 <- genAR(c = 0.75, n=N, phi = 1, init = 0, noise = 1.5)
  # detrend
  x2r <- detrend(x2)
  # fit model to residuals
  out <- lm(x2r[-1] ~ x2r[-N])
  m_phi_r[k,2] <- out$coefficients[2]
  
  # ------ 3) Deterministic Trend & difference -----
  # difference
  x1d <- diff(x1)
  out <- lm(x1d[-1] ~ x1d[-(N-1)])
  m_phi_d[k,1] <- out$coefficients[2]
  
  # ------ 2) Stochastic Trend with drift & difference -----
  # difference
  x2d <- diff(x2)
  out <- lm(x2d[-1] ~ x2d[-(N-1)])
  m_phi_d[k,2] <- out$coefficients[2]
  
  # ------------------------------------------------------------------------
  # ----- Alternative differencing - only outcome variable -----------------
  # ------------------------------------------------------------------------
  
  # ------  Deterministic Trend & difference 2  -----
  # difference
  out <- lm(x1d ~ x1[-N])
  m_phi_d2[k,1] <- out$coefficients[2]
  
  # ------ Stochastic Trend with drift & difference 2-----
  out <- lm(x2d ~ x2[-N])
  m_phi_d2[k,2] <- out$coefficients[2]
  
  # ------------------------------------------------------------------------
  # ---------------- Arima model function differencing ---------------------
  # ------------------------------------------------------------------------
  # ------  Deterministic Trend & difference 2  -----
  # difference
  out <- forecast::Arima(x1, order = c(1,1,0), include.drift = TRUE)
  m_phi_a[k,1] <- out$coef[1]
  
  # ------ Stochastic Trend with drift & difference 2-----
  out <-  forecast::Arima(x2, order = c(1,1,0), include.drift = TRUE)
  m_phi_a[k,2] <- out$coef[1]
  
  
  print(k)
} # end for

# ---------------------------------------------------
# ---------- Save -----------------------------------
# ---------------------------------------------------

results_detdiff <- list("m_phi_r" = m_phi_r,
                        "m_phi_d" = m_phi_d,
                        "m_phi_d2" = m_phi_d2,
                        "m_phi_a" = m_phi_a)

saveRDS(results_detdiff, file = "files/detdiff_results.RDS")

