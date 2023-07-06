# helper functions

# function to generate data from an AR model

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
