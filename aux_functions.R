# oisinryan1@protonmail.com; jonashaslbeck@gmail.com;

# ---------------------------------------------------
# ---------- What is happening here? ----------------
# ---------------------------------------------------

# Some auxiliary functions for the detrending paper

# ---------------------------------------------------
# ---------- Simulate data from AR Family -----------
# ---------------------------------------------------

genAR <- function(c = 0, phi = .5, n = 101, init = 1, noise = FALSE, ltrend = 0, qtrend = 0){
  # browser()
  if(isFALSE(noise)) noisedraw <- rep(0, n) else noisedraw <- rnorm(n=n, mean =0, sd = noise)
  # browser()
  data <- rep(NA,n)
  data[1] <- init
  for(i in 2:n){
    data[i] <- c + ltrend*i + qtrend*i^2 + phi*data[i-1] + noisedraw[i-1]
  }
  data
}

# ---------------------------------------------------
# ---------- linear detrend a time series -----------
# --------------------------------------------------

detrend <- function(x){
  time <- seq(from = 0, by = 1, length.out = length(x))
  lmobj <- lm(x ~ time)
  lmobj$residuals
}


# ---------------------------------------------------
# ------- Simulate performance of ADF test ----------
# ---------------------------------------------------
# functions to simulate ADF performance in a set of conditions
sim_adf <- function(tpoints_vec, gmod = list(c = 0, ltrend = 0, init = 0, phi = .5, noise = .5, adfmod = "type1"),
                    alpha = 0.01, modSel = "pval"){

# Create storage matrix
rates <- matrix(NA, ncol = 2, nrow = length(tpoints_vec),
                dimnames = list(tpoints_vec, c("modknown","modsel")))
for(q in 1:length(tpoints_vec)){
  n <- tpoints_vec[q]
  print(n)
  urmat <- matrix(NA, nrow = iter, ncol = 3)
  for(i in 1:iter){
    tmp <- genAR(c = gmod$c, ltrend =gmod$ltrend, init = gmod$init , n= n, qtrend = gmod$qtrend, 
                 phi = gmod$phi, noise = gmod$noise)
    
    # classic method; just choose the right model!
    o1 <- aTSA::adf.test(tmp,  nlag = 1, output = FALSE)
    urmat[i,1] <- ifelse(o1[[gmod$adfmod]][,3] > alpha,1,0)
    
    # enders model selection method
    endout <- adf_flow(tmp, alpha = alpha, diffX = FALSE, modSel = modSel)
    urmat[i,2] <- endout$ur
    
  }
  
  # Deterministic quadratic trend 
  rates[q,1] <- mean(urmat[,1])
  rates[q,2] <- mean(urmat[,2])
}
return(rates)
}


# ---------------------------------------------------
# ---------- Function to plot labels ----------------
# ---------------------------------------------------

plotLabel <- function(x, srt=0, col="black",
                      xpos=.6, ypos=.6, cex=1.4) {
  par(mar=rep(0, 4))
  plot.new()
  plot.window(xlim=c(0,1), ylim=c(0,1))
  text(xpos, ypos, x, srt=srt, cex=cex, col=col)
}


# ---------------------------------------------------
# ---------- Pre-process Multilevel Time-Series -----
# ---------------------------------------------------

# function takes a univariate multilevel time series data frame
# applies pre-processing steps, namely one of
  # centering per person
  # detrending per-person with within-person estimated trend
  # detrending per-person with a pre-specified trend per person
  # detrending per-person with a uniform pre-specified trend
# then creates a lagged version of the outcome variable 

lagdataf <- function(dataf, preprocess = "raw", 
                     trendlist = NULL
                     # mppout = FALSE,
){
  ids <- unique(dataf[,"id"])
  
  lobj <- sapply(ids, function(id){
    tmp <- dataf[dataf[,"id"]==id,]
    pp_mean <- mean(tmp[,"Y"])
    if(preprocess == "center_pp"){
      tmp[,"Y"] <- tmp[,"Y"] - mean(tmp[,"Y"])
    }
    if(preprocess == "detrend_pp"){
      time <- 0:(nrow(tmp)-1)
      tmp[,"Y"] <- lm(tmp[,"Y"] ~ time)$residuals
    }
    if(preprocess == "detrend_pp_true"){
      time <- 0:(nrow(tmp)-1)
      tmp[,"Y"] <- tmp[,"Y"] - (trendlist$slopes[id]*time)
    }
    if(preprocess == "detrend_fixed"){
      time <- 0:(nrow(tmp)-1)
      tmp[,"Y"] <-  tmp[,"Y"] - (trendlist$int + trendlist$slope*time)
    }
    out1 <- cbind(rep(id, nrow(tmp) -1),tmp[-1,"Y"], tmp[-nrow(tmp),"Y"])
    # if(mppout){
    #   out1 <- cbind(out1, pp_mean)
    # }
    return(out1)
  }, simplify = FALSE)
  
  output <- do.call("rbind",lobj)
  colnames(output)[1:3] <- c("id","Y", "Ylag")
  # if(mppout){
  #   colnames(output)[4] <- "meanpp"
  # }
  return(output)
}

