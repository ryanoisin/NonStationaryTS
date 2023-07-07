# oisinryan1@protonmail.com
# code to simulate performance multilevel AR(1) model estimation
# under different detrending conditions

library(lme4)
source("aux_functions.R")

# ------------------------------------------------------------------------
# --------------------------- Simulation ---------------------------------
# ------------------------------------------------------------------------

mphi <- 0.3
sdphi <- .1
mtrend <- .2
sdtrend <- 1
covtp <- 0

n_p <- 100
n_t <- 100
iter <- 1000

# storage for parameter estimates
ests <- matrix(NA, iter, 4)
colnames(ests) <- c("raw","center_pp","detrend_m", "detrend_pp")


set.seed(123)

for(i in 1:iter){
  # generate multilevel time series parameters
pars <- MASS::mvrnorm(n = n_p, mu = c(mphi, mtrend),
              Sigma = matrix(c(sdphi^2, covtp,
                               covtp, sdtrend^2),
                                2,2, byrow = T))
# check no stationary trend
max(abs(pars[,1])) < 1
if(!(max(abs(pars[,1])) < 1)){
  print("setting non-stationary phis to 0")
  probs <- which(abs(pars[,1]) > 1)
  pars[probs,1] <- 0

}

# generate data
data <- apply(pars,1, function(row){
  genAR(c = 0, ltrend =row[2], init = 0 , n=n_t , qtrend = 0, phi = row[1], noise = 1)
  })

dataf <- cbind(as.vector(sapply(1:n_p, rep, n_t)), as.vector(data))
colnames(dataf) <- c("id","Y")
dataf <- as.data.frame(dataf)

# Method 1: Naive Estimate
data_raw <- as.data.frame(lagdataf(dataf))
lmobjr <- lmer(Y ~  Ylag + (1+Ylag |id), data = data_raw)
sobjr <- summary(lmobjr)

ests[i,"raw" ] <- c(sobjr$coefficients[2,1])

# Method 2: Center per person
data_cpp <- as.data.frame(lagdataf(dataf, preprocess = "center_pp"))
lmobj2 <- lmer(Y ~ 0 + Ylag + (0 + Ylag |id), data = data_cpp)
sobj2 <- summary(lmobj2)
ests[i,"center_pp"] <- sobj2$coefficients[1,1]

# Method 3: Detrend overall
timeb <- rep(0:(n_t -1), n_p)
dtmod <- lm(Y ~ timeb, data = dataf)$coefficients
trendlist <- list(int = dtmod[1], slope = dtmod[2])
data_mdt <-  as.data.frame(lagdataf(dataf, 
                                    preprocess = "detrend_fixed",
                                    trendlist = trendlist))

lmobj3 <- lmer(Y ~ 0 + Ylag + (0 + Ylag |id), data = data_mdt)
sobj3 <- summary(lmobj3)
ests[i,"detrend_m"] <- sobj3$coefficients[1,1]


# Method 4: Detrend per person
data_ppdt <-  as.data.frame(lagdataf(dataf, 
                                    preprocess = "detrend_pp"))
lmobj4 <- lmer(Y ~ 0 +  Ylag + (0 + Ylag |id), data = data_ppdt)
sobj4 <- summary(lmobj4)
ests[i,"detrend_pp"] <- sobj4$coefficients[1,1]

print(i)
}

# save simulation results
saveRDS(ests,"files/simresults_mldetrend.RDS")

 
