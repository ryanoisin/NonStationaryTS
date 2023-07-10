# jonashaslbeck@protonmail.com; July 9th, 2023

# ---------------------------------------------------
# ---------- What is happening here? ----------------
# ---------------------------------------------------

# Evaluate bias on phi-parameter for case of weak stationarity
# satisfied, random walk, and linear det trend of time

# ---------------------------------------------------
# ---------- Load packages & source -----------------
# ---------------------------------------------------

source("aux_functions.R")


# ---------------------------------------------------
# ---------- Simulate -------------------------------
# ---------------------------------------------------

# Sampling Distribution of phi; no trend, det trend, stoch trend

# Storage
nIter <- 10000
m_phi <- matrix(NA, nIter, 3)
N <- 100

set.seed(1)

# generate different conditions, estimate AR(1)
for(k in 1:nIter) {
  
  # ------ 1) No Trend -----
  X1 <- genAR(n=N, phi = .5, init = rnorm(1,0,sqrt(2)), noise = 1)
  # recover
  out <- lm(X1[-1] ~ X1[-N])
  m_phi[k,1] <- out$coefficients[2]
  
  # ------ 2) Stochastic Trend -----
  X2 <- genAR(n=N, phi = 1, init = 0, noise = 1.5)
  # recover
  out <- lm(X2[-1] ~ X2[-N])
  m_phi[k,2] <- out$coefficients[2]
  
  # ------ 3) Deterministic Trend -----
  X3 <- genAR(n = N, phi = .5, init = 0, ltrend = .1, noise = 1)
  out <- lm(X3[-1] ~ X3[-N])
  m_phi[k,3] <- out$coefficients[2]
  
  print(k)
} # end for


# ---------------------------------------------------
# ---------- Save -----------------------------------
# ---------------------------------------------------

saveRDS(m_phi, file = "files/bias_results.RDS")

