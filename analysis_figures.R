# oisinryan1@protonmail.com

# load packages
library(RColorBrewer)
library(scales)

# --------------------------------------------------------------------------- #
# --------------------------------- Figure 1 -------------------------------- #
# --------------------------------------------------------------------------- #

# Examples of different (non-) stationary processes; 2x2

# --------------------------------------------------------------------------- #
# --------------------------------- Figure 2 -------------------------------- #
# --------------------------------------------------------------------------- #

# Consequences of violating stationarity; 1x3

# --------------------------------------------------------------------------- #
# --------------------------------- Figure 3 -------------------------------- #
# --------------------------------------------------------------------------- #

# Sampling Distribution of phi; no trend, det trend, stoch trend

# --------------------------------------------------------------------------- #
# --------------------------------- Figure 4 -------------------------------- #
# --------------------------------------------------------------------------- #

# Detrending vs Differencing; sampling distribution of phi (2x2)

# --------------------------------------------------------------------------- #
# --------------------------------- Figure 6 -------------------------------- #
# --------------------------------------------------------------------------- #
# Note: Figure 5 made in illustrator

# Performance of DF test and DF flowchart
simres <- readRDS("files/simresults_adftest.RDS")

cols <- brewer.pal(5, "Dark2")

sc <- .3

# pdf("../simur_2panel.pdf", height = 9*2*sc, width = 9*4*sc)
par(mfrow = c(1,2))
# here: custom layout with panel for legend along the bottom

plot.new()
plot.window(xlim = c(0,500), ylim = c(0,1))
axis(1); axis(2, las = 2)
title("(a) ADF Model Known", font.main = 1)
title(xlab = "# Time Points", line = 2.5)
title(ylab = "Proportion Correct", line = 2.5)
for(i in 1:5){
  if(i == 4) jit <- 5; if(i == 5) jit <- -5 else jit <- 0
  lines(x = tpoints_vec +jit, y = simres[[i]][,1], col = alpha(cols[i],.75), type = "b", pch = 21, cex = 1.5)
  points(x = tpoints_vec + jit, y = simres[[i]][,1], bg = alpha(cols[i],.75), pch = 21, cex = 1.5)
}
abline(h = .95, col = "grey", lty = 2)

plot.new()
plot.window(xlim = c(0,500), ylim = c(0,1))
axis(1); axis(2, las = 2)
title("(b) ADF Model Selection", font.main = 1)
title(xlab = "# Time Points", line = 2.5)
title(ylab = "Proportion Correct", line = 2.5)
for(i in 1:5){
  lines(x = tpoints_vec, y = simres[[i]][,2], col = alpha(cols[i],.75), type = "b", pch = 21, cex = 1.5)
  points(x = tpoints_vec, y = simres[[i]][,2], bg = alpha(cols[i],.75), pch = 21, cex = 1.5)
}
abline(h = .95, col = "grey", lty = 2)


legend("bottom", legend = c("Stationary AR(1)",
                                 "Det Linear Trend",
                                 "Random Walk",
                                 "Random Walk Drift",
                                 "Random Walk with Det Linear Trend"), 
       pt.bg = cols, ncol = 5, pt.cex = 1.5,
       pch = 21,
       lty = 1)
# dev.off()

# pdf("../ursim_unknown_lowalpha.pdf", height = 7, width = 9)
plot.new()
plot.window(xlim = c(0,500), ylim = c(0,1))
axis(1); axis(2)
title(main = "Correct Unit-Root Decision, Model Unknown" , xlab = "Time Points", ylab = "Proportion of correct decisions")
for(i in 1:5){
  lines(x = jitter(tpoints_vec), y = simres[[i]][,2], col = cols[i], type = "b", pch = 20)
}
abline(h = .95, col = "grey", lty = 2)
legend("bottomright", legend = c("Stationary",
                                 "Det. Trend",
                                 "Random Walk",
                                 "Random Walk Drift",
                                 "Random Walk with Det. Trend"), col = cols, pch = 20)
dev.off()

# --------------------------------------------------------------------------- #
# --------------------------------- Figure 7 -------------------------------- #
# --------------------------------------------------------------------------- #
# Forecasting; determinstic vs stochastic

