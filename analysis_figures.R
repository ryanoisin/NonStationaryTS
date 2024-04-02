# oisinryan1@protonmail.com

# load packages
library(RColorBrewer)
library(scales)
library(fpp3)
 # dependencies of fpp3
library(tidyr); library(dplyr); library(tsibble)
library(lubridate)

library(forecast)

source("aux_functions.R")

# --------------------------------------------------------------------------- #
# --------------------------------- Figure 1 -------------------------------- #
# --------------------------------------------------------------------------- #
# jonashaslbeck@protonmail.com

# Examples of different (non-) stationary processes; 2x2

p <- 7 # number of time series
n <- 20 # number of time points
noise <- 1

l_data <- list()

# Stationary (phi=0.5, beta=0)
set.seed(2)
l_data[[1]] <-  sapply(c(1:p), function(s){
  genAR(n=n, phi = .5, init = rnorm(1,0,sqrt(2)), noise = noise)
})

# Mean stationary, not cov-stationary (phi=1, beta=0)
set.seed(17)
l_data[[2]] <-  sapply(c(1:p), function(s){
  genAR(n=n, phi = 1, init = 0, noise = 1.5)
})

# Mean trend, cov-stationary (phi=0.5, beta=0.1)
set.seed(3)
l_data[[3]] <-  sapply(c(1:p), function(s){
  genAR(n = n, phi = .5, init = 0, ltrend = .1, noise = noise)
})

# Mean trend, cov-stationary (phi=1, c=0.75)
set.seed(15)
l_data[[4]] <-  sapply(c(1:p), function(s){
  genAR(n = n, phi = 1, init = 0, c = .75, noise = 1.5)
})

# ---------- Plotting--------------------------

# Get nice colors
cols <- brewer.pal(p, "Set1")

# PDF
pdf("figures/Figure1_Illustration_Stationarity.pdf", width=8, height=7)

par(mfrow=c(2,2), mar=c(4,3.5,2.5,1.5))

lwd <- 1.5

# --- Plot 1: Stationary ---
# Layout
plot.new()
plot.window(xlim=c(1, 20), ylim=c(-4,4))
axis(1, c(1, 5, 10, 15, 20))
axis(2, las=2)
title(xlab="Time", line=2.4)
title(main="(a) Weakly Stationary", font.main=1)
# Mean/var lines
abline(h=c(1*sqrt(2), -1*sqrt(2)), col = "gray", lty = 2, lwd=2)
abline(h = 0, col = 'gray', lwd=2)
# Data
for(i in 1:p) lines(l_data[[1]][, i], col=cols[i], lwd=lwd)

# --- Plot 2: Mean stationarity / not var-stationary ---
# Layout
plot.new()
plot.window(xlim=c(1, 20), ylim=c(-20,20))
axis(1, c(1, 5, 10, 15, 20))
axis(2, las=2)
title(xlab="Time", line=2.4)
title(main="(b) Variance depends on Time", font.main=1)
# Mean/var lines
time <- 1:20
lines(time, time*1.1, col = "gray", lty = 2, lwd = 2)
lines(time, -time*1.1, col = "gray", lty = 2, lwd = 2)
abline(h = 0, col = 'gray', lwd=2)
# Data
for(i in 1:p) lines(l_data[[2]][, i], col=cols[i], lwd=lwd)

# --- Plot 3: Mean trend / var-stationary ---
# Layout
plot.new()
plot.window(xlim=c(1, 20), ylim=c(-3,6))
axis(1, c(1, 5, 10, 15, 20))
axis(2, las=2, seq(-3, 6, length=7))
title(xlab="Time", line=2.4)
title(main="(c) Mean depends on Time", font.main=1)
# Mean/var lines
time <- 1:20
lines(time, .19*time, col = "grey", lwd=2)
lines(time, .19*time + sqrt(2), col = "grey", lty = 2, lwd=2)
lines(time, .19*time - sqrt(2), col = "grey", lty = 2, lwd=2)
# Data
for(i in 1:p) lines(l_data[[3]][, i], col=cols[i], lwd=lwd)


# --- Plot 4: Mean trend / not var-stationary ---
# Layout
plot.new()
plot.window(xlim=c(1, 20), ylim=c(-10,30))
axis(1, c(1, 5, 10, 15, 20))
axis(2, las=2)
title(xlab="Time", line=2.4)
title(main="(d) Mean & Variance depend on Time", font.main=1)
# Mean/var lines
lines(time, .75*time, col = "grey", lwd=2)
lines(time, .75*time + time*sqrt(1.5), col = "grey", lty = 2, lwd=2)
lines(time, .75*time - time*sqrt(1.5), col = "grey", lty = 2, lwd=2)
# Data
for(i in 1:p) lines(l_data[[4]][, i], col=cols[i], lwd=lwd)

dev.off()



# --------------------------------------------------------------------------- #
# --------------------------------- Figure 2 -------------------------------- #
# --------------------------------------------------------------------------- #
# jonashaslbeck@protonmail.com


# Consequences of violating stationarity on cumulative mean; 1x3

# generate data
N <- 1000
set.seed(9)

# ------ 1) No Trend -----
X1 <- genAR(n=N, phi = .5, init = rnorm(1,0,sqrt(2)), noise = 1)
cumm_X1 <- cumsum(X1) / seq_along(X1)

# ------ 2) Stochastic Trend -----
X2 <- genAR(n=N, phi = 1, init = 0, noise = 1.5)
cumm_X2 <- cumsum(X2) / seq_along(X2)

# ------ 3) Deterministic Trend -----
X3 <- genAR(n = N, phi = .5, init = 0, ltrend = .01, noise = 1)
cumm_X3 <- cumsum(X3) / seq_along(X3)

sc <- 1.1
pdf("figures/Figure2_nonConv.pdf", width = 7*sc, height=2.7*sc)

par(mfrow=c(1,3), mar=c(4,3.5,3,0.5))

lwd.data <- 1.3
alpha.data <-  0.75
lwd.cm <- 2
line.xlab <- 2.6
line.main <- 1.5

# ------ 1) No Trend -----
plot.new()
plot.window(xlim=c(1,N), ylim=c(-4,4))
axis(1)
axis(2, las=2)
lines(X1, col=alpha("black", alpha=alpha.data), lwd=lwd.data)
lines(cumm_X1, col="tomato", lwd=lwd.cm)
title("(a) Weak Stationarity Satisfied", font.main=1, line=line.main)
title(xlab="Time", line=line.xlab)
text(700, 3.5, "Cumulative Mean", col="tomato", cex=1.3)

# ------ 2) Stochastic Trend -----
plot.new()
plot.window(xlim=c(1,N), ylim=c(-20,60))
axis(1)
axis(2, las=2)
lines(X2, col=alpha("black", alpha=alpha.data), lwd=lwd.data)
lines(cumm_X2, col="tomato", lwd=lwd.cm)
title("(b) Variance Depends on Time", font.main=1, line=line.main)
title(xlab="Time", line=line.xlab)

# ------ 3) Deterministic Trend -----
plot.new()
plot.window(xlim=c(1,N), ylim=c(0,20))
axis(1)
axis(2, las=2)
lines(X3, col=alpha("black", alpha=alpha.data), lwd=lwd.data)
lines(cumm_X3, col="tomato", lwd=lwd.cm)
title("(c) Mean depends on Time", font.main=1, line=line.main)
title(xlab="Time", line=line.xlab)

dev.off()

# --------------------------------------------------------------------------- #
# --------------------------------- Figure 2b -------------------------------- #
# --------------------------------------------------------------------------- #
# jonashaslbeck@protonmail.com

# This is for Reviewer 3 who wanted to see the time-varying AR for all four cases

# Function to get time-varying AR
getTvAR <- function(X) {
  m_lag <- cbind(X[-1], X[-N])
  tv_cor <- sapply(3:(N-1), function(x) { 
    cor(m_lag[1:x, ])[1,2]
  })
} # eoF


# ------ 1) No Trend -----
tvAR_X1 <- getTvAR(X1)

# ------ 2) Stochastic Trend -----
tvAR_X2 <- getTvAR(X2)

# ------ 3) Deterministic Trend -----
tvAR_X3 <- getTvAR(X3)


sc <- 1.1
pdf("figures/Figure2b_tvAR.pdf", width = 7*sc, height=2.7*sc)

par(mfrow=c(1,3), mar=c(4,3.5,3,0.5))

lwd.data <- 1.3
alpha.data <-  0.75
lwd.cm <- 2
line.xlab <- 2.6
line.main <- 1.5

# ------ 1) No Trend -----
plot.new()
plot.window(xlim=c(1,N), ylim=c(-1,1))
axis(1)
axis(2, las=2)
abline(h=seq(-1, 1, length=5), lty=2, col="grey")
abline(h=seq(-1, 1, length=5), lty=2, col="grey")
lines(tvAR_X1, col="tomato", lwd=lwd.cm)
title("(a) Weak Stationarity Satisfied", font.main=1, line=line.main)
title(xlab="Time", line=line.xlab)
text(700, -.25, "Time-varying AR", col="tomato", cex=1.3)

# ------ 2) Stochastic Trend -----
plot.new()
plot.window(xlim=c(1,N), ylim=c(-1,1))
axis(1)
axis(2, las=2)
abline(h=seq(-1, 1, length=5), lty=2, col="grey")
lines(tvAR_X2, col="tomato", lwd=lwd.cm)
title("(b) Variance Depends on Time", font.main=1, line=line.main)
title(xlab="Time", line=line.xlab)

# ------ 3) Deterministic Trend -----
plot.new()
plot.window(xlim=c(1,N), ylim=c(-1,1))
axis(1)
axis(2, las=2)
abline(h=seq(-1, 1, length=5), lty=2, col="grey")
lines(tvAR_X3, col="tomato", lwd=lwd.cm)
title("(c) Mean depends on Time", font.main=1, line=line.main)
title(xlab="Time", line=line.xlab)

dev.off()




# --------------------------------------------------------------------------- #
# --------------------------------- Figure 3 -------------------------------- #
# --------------------------------------------------------------------------- #
# jonashaslbeck@protonmail.com

# Load output from "analysis_biases.R"

m_phi <- readRDS(file = "files/bias_results.RDS")

# ---------- Plotting -------------------------------

sc <- 1.1
pdf("figures/Figure3_hists_bias.pdf", width = 7*sc, height=2.7*sc)

n_breaks <- 50
cex.axis <- 1
line.main <- 1.5

par(mfrow=c(1,3), mar=c(4,3.5,3,0.5))
# No trend
hist(m_phi[, 1], breaks=seq(0, 1, length=n_breaks), freq = FALSE,
     axes=FALSE, xlab="", main="", ylim=c(0,6), ylab="")
axis(1, cex.axis=cex.axis)
axis(2, las=2)
abline(v=0.5, lwd=1.5, col="orange")
abline(v=mean(m_phi[, 1]), lwd=1.5, lty=2, col="black")
title("(a) Weak Stationarity Satisfied", font.main=1, line=line.main)
title(xlab=expression(hat(phi)))
title(ylab="Density", line=2.2)

# Legend
legend <- c(expression(phi),
            expression(paste("Mean ", hat(phi))))
legend("topright", legend=legend,
       lwd=c(1.5, 1.5), lty=1:2, col=c("orange", "black"),
       bty="n",)


# Stochastic Trend
hist(m_phi[, 2], breaks=seq(0.6, 1.2, length=n_breaks), freq = FALSE,
     axes=FALSE, xlab="", main="", ylim=c(0,15), ylab="")
axis(1, cex.axis=cex.axis)
axis(2, las=2)
abline(v=1, lwd=1.5, col="orange")
abline(v=mean(m_phi[, 2]), lwd=1.5, lty=2, col="black")
title("(b) Variance Depends on Time", font.main=1, line=line.main)
title(xlab=expression(hat(phi)))

# Det trend
hist(m_phi[, 3], breaks=seq(0.4, 1.1, length=n_breaks), freq = FALSE,
     axes=FALSE, xlab="", main="", ylim=c(0,70), ylab="")
axis(1, seq(0.4, 1.1, length=8), cex.axis=cex.axis)
axis(2, las=2)
abline(v=0.5, lwd=1.5, col="orange")
abline(v=mean(m_phi[, 3]), lwd=1.5, lty=2, col="black", line=line.main)
title("(c) Mean depends on Time", font.main=1)
title(xlab=expression(hat(phi)))

dev.off()


# --------------------------------------------------------------------------- #
# --------------------------------- Figure 4 -------------------------------- #
# --------------------------------------------------------------------------- #
# jonashaslbeck@protonmail.com

# Load results from "analysis_detdiff.R"
results_detdiff <- readRDS(file = "files/detdiff_results.RDS")

# --------------------------------- Plotting -----------------------------

# Plot text function
alpha <- 0.2
sc <- 0.9
pdf("figures/Figure4_detrend_bias.pdf", width = 7*sc, height=6.5*sc)

# Make Layout
lmat <- rbind(c(0, 1, 2),
              c(3, 5, 6),
              c(4, 7, 8))

lo <- layout(mat = lmat, widths = c(.1, 1, 1), heights = c(.1, 1, 1))

# other plotting parameters
cex <- 1.4
xpos1 <- .55
xpos2 <- .3
ypos1 <- .4
ypos2 <- .55
plotLabel("Detrending", xpos=xpos1, cex=cex, ypos=ypos1)
plotLabel("Differencing", xpos=xpos1, cex=cex, ypos=ypos1)
plotLabel("Deterministic Trend", srt=90, xpos=xpos2, ypos=ypos2, cex=cex)
plotLabel("Stochastic Trend", srt=90, xpos=xpos2, ypos=ypos2, cex=cex)


### Plot data
n_breaks <- 50
cex.axis <- 0.9

par(mar=c(4,3.5,.5,0.5))
# Deterministic Trend, De-trend
plot.new()
plot.window(xlim=c(0,1), ylim=c(0, 6))
# rect(0, 0, 1, 6, col=alpha("darkgreen", alpha=alpha), border=FALSE)
hist(results_detdiff$m_phi_r[, 1], breaks=seq(0, 1, length=n_breaks), freq = FALSE,
     axes=FALSE, xlab="", main="", ylim=c(0,6), ylab="", add=TRUE)

axis(1, cex.axis=cex.axis)
axis(2, las=2)
abline(v=0.5, lwd=1.5, col="orange")
abline(v=mean(results_detdiff$m_phi_r[, 1]), lwd=1.5, lty=2, col="black")
title(xlab=expression(hat(phi)))
title(ylab="Density", line=2.2)

# Legend
legend <- c(expression(paste("True ", phi)),
            expression(paste("Mean ", hat(phi))))
legend(.65, 6, legend=legend,
       lwd=c(1.5, 1.5), lty=1:2, col=c("orange", "black"),
       bty="n",)


# Stochastic Trend, de-trend
plot.new()
plot.window(xlim=c(.5,1.1), ylim=c(0, 10))
# rect(0, 0, 1.1, 10, col=alpha("red", alpha=alpha), border=FALSE)
hist(results_detdiff$m_phi_r[, 2],breaks=seq(0.5, 1.1, length=n_breaks), freq = FALSE,
     axes=FALSE, xlab="", main="", ylim=c(0,10), ylab="", add=TRUE)
axis(1, cex.axis=cex.axis)
axis(2, las=2)
abline(v=1, lwd=1.5, col="orange")
abline(v=mean(results_detdiff$m_phi_r[, 2]), lwd=1.5, lty=2, col="black")
title(xlab=expression(hat(phi)))


# Deterministic Trend, Differenced
plot.new()
plot.window(xlim=c(.4,1.2), ylim=c(0, 6))
# rect(0, 0, 1.2, 6, col=alpha("red", alpha=alpha), border=FALSE)
hist(results_detdiff$m_phi_d[, 1] + 1, breaks=seq(0.4, 1.2, length=n_breaks), freq = FALSE,
     axes=FALSE, xlab="", main="", ylim=c(0,6), ylab="", add=TRUE)
axis(1, cex.axis=cex.axis)
axis(2, las=2)
abline(v=0.5, lwd=1.5, col="orange")
abline(v=mean(results_detdiff$m_phi_d[, 1]+1), lwd=1.5, lty=2, col="black")
title(xlab=expression(hat(phi) + 1))
title(ylab="Density", line=2.2)


# Stochastic Trend, Differenced
plot.new()
plot.window(xlim=c(.6,1.4), ylim=c(0, 6))
# rect(0, 0, 1.4, 6, col=alpha("darkgreen", alpha=alpha), border=FALSE)
hist(results_detdiff$m_phi_d[, 2]+1,breaks=seq(0.6, 1.4, length=n_breaks), freq = FALSE,
     axes=FALSE, xlab="", main="", ylim=c(0,6), ylab="", add=TRUE)
axis(1, cex.axis=cex.axis)
axis(2, las=2)
abline(v=1, lwd=1.5, col="orange")
abline(v=mean(results_detdiff$m_phi_d[, 2]+1), lwd=1.5, lty=2, col="black")
title(xlab=expression(hat(phi) + 1))
title(ylab="Density", line=2.2)

dev.off()



# --------------------------------------------------------------------------- #
# --------------------------------- Figure 6 -------------------------------- #
# --------------------------------------------------------------------------- #
# Note: Figure 5 made in illustrator
# Performance of DF test and DF flowchart

# load simulation results (analysis_adfsim.R)
simres <- readRDS("files/simresults_adftest.RDS")
tpoints_vec <- as.numeric(rownames(simres[[1]]))
cols <- brewer.pal(5, "Dark2")

sc <- 4.25

# Create main panels
pdf("figures/Figure6_URsim_plus_leg.pdf", height = (1.1)*sc, width = 2*sc)

lmat <- rbind(c(1,2), c(3,3))
lo <- layout(mat = lmat, heights = c(1, .2), widths = c(1,1))

lwd.data <- 1.8

# Data
par(mar=c(4, 4.2, 3, 2))
plot.new()
plot.window(xlim = c(0,500), ylim = c(0,1))
axis(1); axis(2, las = 2)
title("(a) ADF Model Known", font.main = 1, line=1.5)
title(xlab = "# Time Points", line = 2.5)
title(ylab = "Proportion Correct", line = 2.5)
for(i in 1:5){
  if(i == 4) jit <- 5; if(i == 5) jit <- -5 else jit <- 0
  lines(x = tpoints_vec +jit, y = simres[[i]][,1], col = alpha(cols[i],.75), type = "l", pch = 21, lwd = lwd.data)
  # points(x = tpoints_vec + jit, y = simres[[i]][,1], bg = alpha(cols[i],.75), pch = 21, cex = 1.5)
}
abline(h = .95, col = "grey", lty = 2)

# 2nd panel; with model selection
plot.new()
plot.window(xlim = c(0,500), ylim = c(0,1))
axis(1); axis(2, las = 2)
title("(b) ADF Model Selection", font.main = 1, line=1.5)
title(xlab = "# Time Points", line = 2.5)
title(ylab = "Proportion Correct", line = 2.5)
for(i in 1:5){
  lines(x = tpoints_vec, y = simres[[i]][,2], col = alpha(cols[i],.75), type = "l", pch = 21, lwd = lwd.data)
  # points(x = tpoints_vec, y = simres[[i]][,2], bg = alpha(cols[i],.75), pch = 21, cex = 1.5)
}
abline(h = .95, col = "grey", lty = 2)

# Legend
par(mar=c(0, 4, 0, 2))
plot.new()
legend("center", xpd = "n", bty = "n",
       legend = c("Stationary\nAR(1)",
                  "Deterministic\nLinear Trend",
                  "Random Walk",
                  "Random Walk\nWith Drift",
                  "Random Walk\nLinear Trend"),
       pt.bg = cols, ncol = 5, lwd = lwd.data, col = cols, border = "black",
       # pch = 21,
       lty = 1,
       text.width = .17)

dev.off()

# # pdf("../ursim_unknown_lowalpha.pdf", height = 7, width = 9)
# plot.new()
# plot.window(xlim = c(0,500), ylim = c(0,1))
# axis(1); axis(2)
# title(main = "Correct Unit-Root Decision, Model Unknown" , xlab = "Time Points", ylab = "Proportion of correct decisions")
# for(i in 1:5){
#   lines(x = jitter(tpoints_vec), y = simres[[i]][,2], col = cols[i], type = "b", pch = 20)
# }
# abline(h = .95, col = "grey", lty = 2)
# legend("bottomright", legend = c("Stationary",
#                                  "Det. Trend",
#                                  "Random Walk",
#                                  "Random Walk Drift",
#                                  "Random Walk with Det. Trend"), col = cols, pch = 20)
# dev.off()

# --------------------------------------------------------------------------- #
# --------------------------------- Figure 7 -------------------------------- #
# --------------------------------------------------------------------------- #
# Forecasting; determinstic vs stochastic

# ---- Generate time series -----

# Generate trend
time <- 0:20
trend <- 120 -2*time

# add noise (here, pre-specified for illustrative purposes)
noise <- c(-10, -9.5, -9.2, -5.16, -2.45, 1.59, 3.77, 6.45, 6.4, 8.1, 5.6,
           5.7, 5.9, 4.5, 1.7, -2.5, -1, -4, -2, -2, -1)
x <- trend + noise

# ---- Determinstic Trend model ----
fit_growth <- lm(x ~ time)

# Forecast future values form this model
newdata <- data.frame(
  time = seq(0:30)
)
predg <- predict(
  object = fit_growth,
  newdata = newdata,
  interval = "prediction"
)

# --- Stochastic Trend Model ----
# get data into format for fpp3 package
dat <- data.frame(x = c(x, rep(NA, 10)), time = c(0:30),
                  prepost = c(rep("pre", length(x)),
                              rep("post", length(21:30)))
)
dat_ts <- as_tsibble(dat, index = time)

# fit ARIMA model with fpp3, allow it to select best model using defaults
fit_arima <- dat_ts  |>
  filter(prepost == "pre") |> model(timeseries = ARIMA(x))
# best fitting model is random walk with drift

# Generate forecasts
fcasts <- fit_arima |> forecast(new_data = dat_ts |> filter(prepost == "post"))
fcasts

# manually extract forecast distribution from fcasts object
dist_man <- rbind(c(77.5,5.3),
                  c(75.9, 11),
                  c(74.4, 16),
                  c(72.8,21),
                  c(71.3,27),
                  c(69.7,32),
                  c(68.2,37),
                  c(66.6,43),
                  c(65.1,48),
                  c(63.5,53))

# mean of forecast distribution is the point forecast
meansw <- dist_man[,1]
# get 2.5 and 97.5 percent quantiles of forecast distributions
quants <- apply(dist_man, 1,
                function(row) qnorm(c(.025,.975), row[1], row[2]))

sc <- 1.1
pdf("figures/Figure7_forecast.pdf", width = 7*sc, height=3.5*sc)
par(mfrow = c(1,2))
# plot deterministic 
plot.new()
plot.window(xlim=c(0,30), ylim=c(-50,200))
axis(1)
axis(2, las=2, at = c(-50, 0, 50, 100, 150,200))
title("(a) Deterministic Trend", font.main=1)
title(xlab="Time", line=2.2)

# prediciton intervals as shaded regions
lines(x = 20:30, y = predg[21:31,"fit"], col="tomato", lwd=2)
lines(x = 20:30, y = c(x[21],predg[22:31,"lwr"]), col = "tomato",
      lty = 2, lwd = 2)
yl <- c(x[21],predg[22:31,"lwr"])
yu <- c(x[21],predg[22:31,"upr"])
lines(x = 20:30, y = c(x[21],predg[22:31,"upr"]), col = "tomato",
      lty = 2, lwd = 2)
polygon(x = c(20:30, rev(20:30)),
        y = c(yu, rev(yl)),
        col = alpha("tomato", .5), lty = 0
)
lines(x = 0:20, y = x, col="black", lwd = 2)

# plot random walk with drift 
plot.new()
plot.window(xlim=c(0,30), ylim=c(-50,200))
axis(1)
axis(2, las=2, at  = c(-50, 0, 50, 100, 150, 200))

lines(x = 20:30, y = c(x[20],meansw),
      col="tomato", lwd=2)
yl <- c(x[21], quants[1,])
yu <- c(x[21], quants[2,])
lines(x = 20:30, y = yl, col = "tomato",
      lty = 2, lwd = 2)
lines(x = 20:30, y = yu, col = "tomato",
      lty = 2, lwd = 2)

polygon(x = c(20:30, rev(20:30)),
        y = c(yu, rev(yl)),
        col = alpha("tomato", .5), lty = 0
)

lines(x = 0:20, y = x, col="black", lwd = 2)
title("(b) Stochastic Trend", font.main=1)
title(xlab="Time", line=2.2)

dev.off()

# --------------------------------------------------------------------------- #
# --------------------------------- Figure 8 -------------------------------- #
# --------------------------------------------------------------------------- #
# Fixed autoregressive effects in multilevel setting

# load simualtion results; `analysis_mlar.R`
ests <- readRDS("files/simresults_mldetrend.RDS")
iter <- nrow(ests)
mphi <- 0.3 # true mean phi parameter used in data generation

estlong <- data.frame(estimates = c(ests[,"raw"], ests[,"center_pp"], 
                                    ests[,"detrend_m"], ests[,"detrend_pp"]),
                      method = c(rep("raw", iter), rep("center_pp", iter),
                                 rep("detrend_m", iter), rep("detrend_pp", iter))
)

estlong$method <- factor(estlong$method ,
                         levels=c("raw","center_pp", "detrend_m", 
                                  "detrend_pp"))
pdf("figures/Figure8_mldetrend.pdf", 12, 7)
boxplot(estimates ~ method, data = estlong,
        names = c("Raw", "Within-Unit Mean Centered",
                  "Detrended with Mean Trend",
                  "Detrended Per Unit"),
        frame = FALSE,
        xlab = "Data Pre-Processing",
        ylab = "Autoregressive Fixed Effect Estimate", 
        cex.axis = 1.1, 
        cex.lab = 1.1)
abline(h = mphi, lty = 2, col = "gray")
dev.off()

# --------------------------------------------------------------------------- #
# --------------------------------- Figure 9 -------------------------------- #
# --------------------------------------------------------------------------- #
# TVVAR; jonashaslbeck@protonmail.com

# Load results
l_tvvar <- readRDS(file="files/tvvar_results.RDs")

N <- 1000

sc <- 1.1
pdf("figures/Figure9_TVVAR.pdf", width = 12*sc, height=10*sc)

# ---- Setup Layout ----
m1 <- matrix(1:20, ncol=5, byrow = TRUE)
m2 <- cbind(c(21:24), m1)
m3 <- rbind(c(25:30), m2)
lo <- layout(mat = m3, widths = c(.2, 1, 1, 1, 1, 1), heights = c(.2, 1, 1, 1, 1))
# layout.show(lo)

# Define ylims for data
m_ylim <- rbind(c(-4, 4),
                c(-35,35),
                c(0, 200),
                c(0, 750))

# Define ylims for parameters (kernel)
m_ylim_phi <- rbind(c(0.35, 0.65),
                    c(0.95, 1),
                    c(0.95, 1.05),
                    c(0.95, 1.05))
v_phi_correct <- c(.5, 1, .5, 1)

m_ylim_ints <- rbind(c(-.1, .1),
                     c(-.1, .1),
                     c(-.1, .1),
                     c(-.1, .1))
v_ints_correct <- c(0, 0, 0, 0.75)

N_est <- 50

#  Define ylims for parameters (GAM)
m_ylim_phi_GAM <- rbind(c(0.35, 0.65),
                        c(0.90, 1),
                        c(0.4, .5),
                        c(.9, 1))
m_ylim_ints_GAM <- rbind(c(-.1, .1),
                         c(-2, 2),
                         c(0, 120),
                         c(0, 60))

# ----Loop trough 4 cases ----

par(mar=c(4, 4, 1, 1))
line <- 3
cex.tit <- 1.3

for(i in 1:4) {
  
  # Data
  plot.new()
  plot.window(xlim=c(1, N), ylim=m_ylim[i, ])
  axis(1)
  axis(2, las=2)
  title(xlab="Time", ylab="X", line=line)
  lines(l_tvvar$data[[i]][,1])
  
  # --- Kernel (mgm) ---
  # Phi Parameter (mgm)
  plot.new()
  plot.window(xlim=c(0, N_est), ylim=m_ylim_phi[i, ])
  x_ticks <- seq(0, N, length=6)
  axis(1, labels=x_ticks, at=x_ticks/20)
  axis(2, las=2)
  title(xlab="Time", ylab="Estimate", line=line)
  lines(l_tvvar$l_AR[[i]])
  abline(h=v_phi_correct[i], lty=2)
  
  # Intercept (mgm)
  plot.new()
  plot.window(xlim=c(1, N_est), ylim=m_ylim_ints[i, ])
  x_ticks <- seq(0, N, length=6)
  axis(1, labels=x_ticks, at=x_ticks/20)
  axis(2, las=2)
  title(xlab="Time", ylab="Estimate", line=line)
  lines(l_tvvar$l_ints[[i]])
  abline(h=v_ints_correct[i], lty=2)
  
  # --- GAM (tvvarGAM) ---
  # Phi Parameter (tvvarGam)
  plot.new()
  plot.window(xlim=c(0, N_est), ylim=m_ylim_phi_GAM[i, ])
  x_ticks <- seq(0, N, length=6)
  axis(1, labels=x_ticks, at=x_ticks/20)
  axis(2, las=2)
  title(xlab="Time", ylab="Estimate", line=line)
  lines(l_tvvar$l_AR_gam[[i]], col="orange")
  abline(h=v_phi_correct[i], lty=2)
  
  # Intercept (tvvarGam)
  plot.new()
  plot.window(xlim=c(1, N_est), ylim=m_ylim_ints_GAM[i, ])
  x_ticks <- seq(0, N, length=6)
  axis(1, labels=x_ticks, at=x_ticks/20)
  axis(2, las=2)
  title(xlab="Time", ylab="Estimate", line=line)
  lines(l_tvvar$l_ints_gam[[i]], col="orange")
  abline(h=v_ints_correct[i], lty=2)
  
} # end for: 4 cases


# ---- Plot row&column labels ----

cex.main <- 1.8

# Row labels
plotLabel("Stationary", srt=90, cex=cex.main)
plotLabel("Random Walk", srt=90, cex=cex.main)
plotLabel("Det. Trend", srt=90, cex=cex.main)
plotLabel("Random Walk + Drift", srt=90, cex=cex.main)

# Col labels
plotLabel("", srt=0)
plotLabel("Data", srt=0, cex=cex.main)
plotLabel("Phi (kernel)", srt=0, cex=cex.main)
plotLabel("Intercept (kernel)", srt=0, cex=cex.main)
plotLabel("Phi (GAM)", srt=0, col="orange", cex=cex.main)
plotLabel("Intercept (GAM)", srt=0, col="orange", cex=cex.main)

dev.off()















