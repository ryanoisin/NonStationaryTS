

# ---------------------------------------------
# --------- Function -----=--------------------
# ---------------------------------------------


adf_flow <- function(x,
                       alpha = 0.05, 
                       diffX = FALSE, 
                       modSel = "pval"){ # options: pval, AIC, BIC

  y <- diff(x)
  n <- length(y)
  result1 <- result2 <- result3 <- NULL
  
  # data prep
  if(isTRUE(diffX)){
    yt <- y[-1]
    xt1 <- y[-length(y)]
    
  } else if(isFALSE(diffX)){
    yt <- y
    xt1 <- x[-length(x)]
  }
  
  t <- 1:length(yt)
  
  # function to approximate p-value based on critical value tables and sample size
  # from aTSA::adf.test
  pvalue <- function(table, stat) {
    Ncol <- ncol(table)
    Size <- c(25, 50, 100, 250, 500, 1e+05)
    Percnt <- c(0.01, 0.025, 0.05, 0.1, 0.5, 0.9, 0.95, 
                0.975, 0.99)
    intplSize <- numeric(Ncol)
    for (j in 1:Ncol) intplSize[j] <- approx(Size, table[,j], n, rule = 2)$y
    approx(intplSize, Percnt, stat, rule = 2)$y
  }
  
  pvalue_pars <- function(table, stat) {
    Ncol <- ncol(table)
    Size <- c(25, 50, 100, 250, 500, 1e+05)
    Percnt <- c(0.01, 0.025, 0.05, 0.1)
    intplSize <- numeric(Ncol)
    for (j in 1:Ncol) intplSize[j] <- approx(Size, table[,j], n, rule = 2)$y
    approx(intplSize, Percnt, stat, rule = 2)$y
  }
  
  # critical value tables for unit root hypothesis tests across models
  table1 <- rbind(c(-2.65, -2.26, -1.95, -1.6, -0.47, 0.92, 1.33, 1.7, 2.15), 
                  c(-2.62, -2.25, -1.95, -1.61, -0.49, 0.91, 1.31, 1.66, 2.08), 
                  c(-2.6, -2.24, -1.95, -1.61, -0.5, 0.9, 1.29, 1.64, 2.04), 
                  c(-2.58, -2.24, -1.95, -1.62, -0.5, 0.89, 1.28, 1.63, 2.02), 
                  c(-2.58, -2.23, -1.95, -1.62, -0.5, 0.89, 1.28, 1.62, 2.01), 
                  c(-2.58, -2.23, -1.95, -1.62, -0.51, 0.89, 1.28,1.62, 2.01))
  table2 <- rbind(
    c(-3.75,-3.33,-2.99,-2.64,-1.53,-0.37, 0, 0.34, 0.71),
    c(-3.59,-3.23,-2.93,-2.6,-1.55,-0.41,-0.04, 0.28, 0.66),
    c(-3.5,-3.17,-2.9,-2.59,-1.56,-0.42,-0.06, 0.26, 0.63),
    c(-3.45,-3.14,-2.88,-2.58,-1.56,-0.42,-0.07, 0.24,0.62),
    c(-3.44,-3.13,-2.87,-2.57,-1.57,-0.44,-0.07, 0.24, 0.61),
    c(-3.42,-3.12,-2.86,-2.57,-1.57, -0.44, -0.08, 0.23, 0.6))
  
  table3 <- rbind(
    c(-4.38,-3.95,-3.6,-3.24,-2.14,-1.14,-0.81,-0.5,-0.15),
    c(-4.16,-3.8,-3.5,-3.18,-2.16,-1.19,-0.87,-0.58,-0.24),
    c(-4.05,-3.73,-3.45,-3.15,-2.17,-1.22,-0.9,-0.62,-0.28),
    c(-3.98,-3.69,-3.42,-3.13,-2.18,-1.23,-0.92,-0.64,-0.31),
    c(-3.97,-3.67,-3.42,-3.13,-2.18,-1.24,-0.93,-0.65,-0.32),
    c(-3.96,-3.67,-3.41,-3.13,-2.18,-1.25,-0.94,-0.66,-0.32)
  )
  
  # Dickey fuller (1981)
  # cvals for testing time trend in most complicated model
  table_m3t <- rbind(
    c(3.74, 3.25, 2.85, 2.39),
    c(3.60, 3.18, 2.81, 2.38),
    c(3.53, 3.14, 2.79, 2.38),
    c(3.49, 3.12, 2.79, 2.38),
    c(3.48, 3.11, 2.78, 2.38),
    c(3.46, 3.11, 2.78, 2.38)
  )
  
  # cvals for testing constant in model 2
  table_m2c <- rbind(
    c(3.41, 2.97, 2.61, 2.2),
    c(3.28, 2.89, 2.56, 2.18),
    c(3.22, 2.86, 2.54, 2.17),
    c(3.19, 2.84, 2.53, 2.16),
    c(3.18, 2.83, 2.52, 2.16),
    c(3.18, 2.83, 2.52, 2.16)
  )
  
  
  # Fit all models we will need
    m3 <- lm(yt ~ xt1 + t)
    m2 <- lm(yt ~ xt1)
    m1 <- lm(yt ~ xt1 - 1)
    
    l_lm <- list(m1,m2,m3)
    
    
 # Compute criteria for model selection
    if(modSel != "pval") {
      
      ## Compute LL
      m_C <- matrix(NA, 3, 2)
      m_C <- as.data.frame(m_C)
      colnames(m_C) <- c("LL", "IC")
      v_npar <- 2:4
      
      N <- length(yt)
      for(i in 1:3) {
        
        # Compute
        preds_i <- predict(l_lm[[i]])
        preds_i_sd <- summary(l_lm[[i]])$sigma
        LL <- sum(dnorm(x = xt1, mean = preds_i, sd=preds_i_sd, log = TRUE))
        
        if(modSel == "BIC"){
          IC <- -2*LL + log(N-1)*v_npar[i]
        }
        if(modSel == "AIC"){
          IC <- -2*LL + 2*v_npar[i]
        }
        
        # Save
        m_C[i, 1] <- LL
        m_C[i, 2] <- IC
        
      } # end for
      }# end if
      
    
  # Test statistic for unit root: Parameter divided by SE
  
  # Start of flowchart
    
    # -------- Step 1 ---------------------
    STAT <- summary(m3)$coefficients[2, 1]/summary(m3)$coefficients[2, 2]
    PVAL1 <- pvalue(table3, STAT)
    # if we reject the null hypothesis, stop. no unit root
    if(PVAL1 <= alpha){
      out <- list(ur = 0, decision = paste0("step 1. no unit root according to m3, p <=", PVAL1), model = m3)
      return(out)
    }else{ 
      
      if(modSel == "pval"){
      # ---------- Step 2 -----------------------
      # Fail to reject, so now check if model over-specified
      # test significance of trend parameter
      STAT <- abs(summary(m3)$coefficients[3, 1])/summary(m3)$coefficients[3, 2]
      PVAL_trend <- pvalue_pars(table_m3t, STAT)
      
      decision1 <- PVAL_trend <= alpha
      # (alternative /addition: use modified F statistic and Phi_3 parameter to test for model simplification)
      } else{
        # otherwise do model selection
       decision1  <- m_C$IC[3] < m_C$IC[2]
      }
      

      
      # if the trend is significant, then the model is correctly specified. 
      # that means we can retest the unit root using the standard normal distribution test
      if(decision1){
        PVAL2 <- summary(m3)$coefficients[2,4]
        if(PVAL2 <= alpha){
          out <- list(ur = 0, 
                      decision = paste0("step 2. no unit root. since trend significant, regular t-test used for unit root.m3, p =", PVAL2),
                      model = m3)
          return(out)
        }else if(PVAL2 > alpha){
          out <- list(ur = 1, 
                      decision = paste0("step 2. unit root. since trend significant, regular t-test used for unit root.m3, p = ", PVAL2),
                      model = m3)
          return(out)
        }# end step 2 model choices
      }else{ 
        #if the trend is not significant, we might have model misspecification, so move on
        # ------- Step 3------------------------
        # we remove the time trend and move to model 2
        STAT <- summary(m2)$coefficients[2, 1]/summary(m2)$coefficients[2, 2]
        PVAL3 <-  pvalue(table2, STAT)
        if(PVAL3 <= alpha){
          out <- list(ur = 0, 
                      decision = paste0("step 3. no unit root according to m2, p <=", PVAL3), 
                      model = m2)
          return(out)
        }else{
          #if null hypothesis rejected, then test for presence of the drift constant
          
          if(modSel == "pval"){
            STAT <- abs(summary(m2)$coefficients[1, 1])/summary(m2)$coefficients[1, 2]
            PVAL_const <- pvalue_pars(table_m2c, STAT)
            
            decision2 <- PVAL_const <= alpha
          } else{
            # otherwise do model selection
            decision2  <- m_C$IC[2] < m_C$IC[1]
          }
          
          
          if(decision2){ 
            # if the constant is signficant, then the model is correctly specified
            # so re-test the parameter using the standard t-test
            PVAL3b <- summary(m2)$coefficients[2,4]
            if(PVAL3b <= alpha){
              out <- list(ur = 0, 
                          decision = paste0("step 3. no unit root. 
                      since constant significant, regular t-test used for unit root. 
                                            m2, p <=", PVAL3b), model = m2)
              return(out)
            }else if(PVAL3b > alpha){
              out <- list(ur = 1, 
                          decision = paste0("step 3. unit root. 
                      since constant significant, regular t-test used for unit root. 
                                            m2, p =", PVAL3b), model = m2)
              return(out)
            }
            
          }else{ # m2 is over-specified , move to the simplest model m1
            # -------- Step 4 ----------
            PVAL4 <- summary(m1)$coefficients[1,4]
            if(PVAL4 <= alpha){
              out <- list(ur = 0, 
                          decision = paste0("step 4. no unit root according to m1, p <=", PVAL4), 
                          model = m1)
              return(out)
            }else if(PVAL4 > alpha){
              out <- list(ur = 1, 
                          decision = paste0("step 4. unit root according to m1, p =", PVAL4), 
                          model = m1)
              return(out)
            } # end decision step 4 branch
          } #end step 4 branch
        
      } #end step 3 else branch
        }# end step 2 else branch
    }# end step 1  else branch
    
} #eoF



# ------------------------------------------------------------------------
# --------- Extended Function for Model Selection  -----------------------
# ------------------------------------------------------------------------

adf_translate <- function(ur, model){
  # this function takes as input a unit-root decision (0 or 1) and a final model (m1, m2 or m3)
  mvec <- c("yt ~ xt1 - 1","yt ~ xt1","yt ~ xt1 + t")
  mout <- as.character(model$call)[2]
  m_ind <- which(mvec == mout)
  
  # 3 x 2 table of possible outcomes
  if(ur == 0){
    if(m_ind == 1){
      ph <- "Stationary AR(1)"
    }else if(m_ind == 2){
      ph <- "AR(1) with linear deterministic trend"
    }else if(m_ind ==3){
      ph <- "AR(1) with quadratic deterministic trend"
    }
  } # end no unit root
  if(ur == 1){
    if(m_ind == 1){
      ph <- "Random Walk"
    }else if(m_ind == 2){
      ph <- "Random Walk with drift"
    }else if(m_ind ==3){
      ph <- "Random Walk with linear trend"
    }
  }# end unit root
  
  return(ph)
}

