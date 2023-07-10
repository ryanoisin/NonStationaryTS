# jonashaslbeck@protonmail.com; July 9th, 2023

# ---------------------------------------------------
# ---------- What is happening here? ----------------
# ---------------------------------------------------

# We would like to see whether TVVAR ks can recover the
# four AR(1) models from Figure 1 in the paper

# ---------------------------------------------------
# ---------- Load packages & source -----------------
# ---------------------------------------------------

library(RColorBrewer)
library(scales)
library(mgm)

# library(devtools)
# install_github("LauraBringmann/tvvarGAM")
library(tvvarGAM)

source("aux_functions.R")


# ---------------------------------------------------
# ---------- Create Storage -------------------------
# ---------------------------------------------------

l_data <- list()
l_AR <- list()
l_ints <- list()

l_AR_gam <- list()
l_ints_gam <- list()


# ---------------------------------------------------
# ---------- General Estimation Settings ------------
# ---------------------------------------------------

bandwidth <- 0.1
n_estpoints <- 50

# ---------------------------------------------------
# ---------- Case 1: Stationary ---------------------
# ---------------------------------------------------

# ------ Generate Data ------
N <- 1000 # number of time points
set.seed(1)
X1 <- genAR(n=N, phi = .5, init = rnorm(1,0,sqrt(2)), noise = 1)
# add two noise dimensions (this is to work around the fact that some estimation functions requre p>2 variables)
data_1 <- cbind(X1, rnorm(N), rnorm(N))

# ------ Estimate TVVAR ------
out_1 <- tvmvar(data = data_1,
                type = rep("g", 3),
                level = rep(1, 3),
                bandwidth = bandwidth,
                estpoints = seq(0, 1, length = n_estpoints),
                lags = 1,
                lambdaSeq=0, lambdaSel="EBIC")

# Save parameters
l_AR[[1]] <- out_1$wadj[1,1 , 1, ]
m_ints <- do.call(rbind, out_1$intercepts)
v_ints <- unlist(m_ints[,1])
l_ints[[1]] <- v_ints

# ------ Estimate TVVAR (tvvarGAM) ------
colnames(data_1) <- c("X1", "X2", "X3")
out_1_gam <- tvvarGAM(data = data_1)

# Save parameters
ints <- out_1_gam$Results_GAM$Estimate[1, 1, ]
AR <- out_1_gam$Results_GAM$Estimate[2, 1, ]
downsample <- round(seq(1, N, length=n_estpoints))
l_ints_gam[[1]] <- ints[downsample]
l_AR_gam[[1]] <- AR[downsample]

# ---------------------------------------------------
# ---------- Case 2: Random Walk ---------------------
# ---------------------------------------------------

# ------ Generate Data ------
N <- 1000
set.seed(1)
X2 <- genAR(n=N, phi = 1, init = 0, noise = 1.5)
# add two noise dimensions
data_2 <- cbind(X2, rnorm(N), rnorm(N))
# Look at data
# plot(data_2[, 1], type="l")
l_data[[2]] <- data_2[, 1]

# ------ Estimate TVVAR ------

out_2 <- tvmvar(data = data_2,
                type = rep("g", 3),
                level = rep(1, 3),
                bandwidth = bandwidth,
                estpoints = seq(0, 1, length = n_estpoints),
                lags = 1,
                lambdaSeq=0, lambdaSel="EBIC")

# Save parameters
l_AR[[2]] <- out_2$wadj[1,1 , 1, ]
m_ints <- do.call(rbind, out_2$intercepts)
v_ints <- unlist(m_ints[,1])
l_ints[[2]] <- v_ints

# ------ Estimate TVVAR (tvvarGAM) ------
colnames(data_2) <- c("X1", "X2", "X3")
out_2_gam <- tvvarGAM(data = data_2)

# Save parameters
ints <- out_2_gam$Results_GAM$Estimate[1, 1, ]
AR <- out_2_gam$Results_GAM$Estimate[2, 1, ]
downsample <- round(seq(1, N, length=n_estpoints))
l_ints_gam[[2]] <- ints[downsample]
l_AR_gam[[2]] <- AR[downsample]


# ---------------------------------------------------
# ---------- Case 3: Det. Trend ---------------------
# ---------------------------------------------------

# ------ Generate Data ------
N <- 1000
set.seed(1)
X3 <- genAR(n = N, phi = .5, init = 0, ltrend = .1, noise = 1)
# add two noise dimensions
data_3 <- cbind(X3, rnorm(N), rnorm(N))

# ------ Estimate TVVAR (mgm) ------

out_3 <- tvmvar(data = data_3,
                type = rep("g", 3),
                level = rep(1, 3),
                bandwidth = bandwidth,
                estpoints = seq(0, 1, length = n_estpoints),
                lags = 1,
                lambdaSeq=0, lambdaSel="EBIC")

# Save parameters
l_AR[[3]] <- out_3$wadj[1,1 , 1, ]
m_ints <- do.call(rbind, out_3$intercepts)
v_ints <- unlist(m_ints[,1])
l_ints[[3]] <- v_ints

# ------ Estimate TVVAR (tvvarGAM) ------
colnames(data_3) <- c("X1", "X2", "X3")
out_3_gam <- tvvarGAM(data = data_3)

# Save parameters
ints <- out_3_gam$Results_GAM$Estimate[1, 1, ]
AR <- out_3_gam$Results_GAM$Estimate[2, 1, ]
downsample <- round(seq(1, N, length=n_estpoints))
l_ints_gam[[3]] <- ints[downsample]
l_AR_gam[[3]] <- AR[downsample]


# ---------------------------------------------------
# ---------- Case 4: Random Walk + Drift ------------
# ---------------------------------------------------

# ------ Generate Data ------
N <- 1000
set.seed(1)
X4 <- genAR(n = N, phi = 1, init = 0, c = .75, noise = 1.5)
# add two noise dimensions
data_4 <- cbind(X4, rnorm(N), rnorm(N))
# Look at data
# plot(data_4[, 1], type="l")
l_data[[4]] <- data_4[, 1]

# ------ Estimate TVVAR ------

out_4 <- tvmvar(data = data_4,
                type = rep("g", 3),
                level = rep(1, 3),
                bandwidth = bandwidth,
                estpoints = seq(0, 1, length = n_estpoints),
                lags = 1,
                lambdaSeq=0, lambdaSel="EBIC")


# Save parameters
l_AR[[4]] <- out_4$wadj[1,1 , 1, ]
m_ints <- do.call(rbind, out_4$intercepts)
v_ints <- unlist(m_ints[,1])
l_ints[[4]] <- v_ints

# ------ Estimate TVVAR (tvvarGAM) ------
colnames(data_4) <- c("X1", "X2", "X3")
out_4_gam <- tvvarGAM(data = data_4)



# Save parameters
ints <- out_4_gam$Results_GAM$Estimate[1, 1, ]
AR <- out_4_gam$Results_GAM$Estimate[2, 1, ]
downsample <- round(seq(1, N, length=n_estpoints))
l_ints_gam[[4]] <- ints[downsample]
l_AR_gam[[4]] <- AR[downsample]

l_data <- list(data_1, data_2, data_3, data_4)


# ---------------------------------------------------
# ---------- Save Results ---------------------------
# ---------------------------------------------------

l_tvvar_results <- list("data" = l_data,
                        "l_AR" = l_AR, 
                        "l_ints" = l_ints,
                        "l_AR_gam" = l_AR_gam,
                        "l_ints_gam" = l_ints_gam)

saveRDS(l_tvvar_results, file="files/tvvar_results.RDs")

