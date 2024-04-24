# code to simulate performance of the ADF test and ADF flowchart method
# oisinryan1@protonmail.com

library(aTSA)
source("adf_flow.R")
source("aux_functions.R")

# ------------------------------------------------------------------------
# --------------------------------- Simulation ---------------------------
# ------------------------------------------------------------------------

iter <- 500

tpoints_vec <- c(25,50,75,100,150,200,250,300,400,500)
rates <- matrix(NA, ncol = 3, nrow = length(tpoints_vec))
alpha = 0.01
modSel = "pval"

# specify data-generating models
  # adfmod specifies what the "correct" ADF model to use is
  # for the aTSA::adf.test function
stat <- list(c = 0, ltrend = 0, init = 0,
             qtrend = 0, phi = .5, noise = .5,
             adfmod = "type1")
stat_trend <- list(c = 0, ltrend = .5, init = 0 ,
                   qtrend = 0, phi = .5, noise = .5,
                   adfmod = "type3")
randwalk <- list(c = 0, ltrend = 0, init = 0 ,
                 qtrend = 0, phi = 1, noise = .5,
                 adfmod = "type1")
randwalk_drift <- list(c = .25, ltrend = 0, init = 0 ,
                       qtrend = 0, phi = 1, noise = .5,
                       adfmod = "type2")
randwalk_drift_trend <- list(c = .25, ltrend = .5, init = 0 ,
                             qtrend = 0, phi = 1, noise = .5,
                             adfmod = "type3")

gmodlist <- list(stat = stat,
                 stat_trend = stat_trend,
                 randwalk = randwalk,
                 randwalk_drift = randwalk_drift,
                 randwalk_drift_trend = randwalk_drift_trend)


# Run simulation
simres <- lapply(gmodlist, function(gmod){
  sim_adf(tpoints_vec, gmod = gmod, alpha = alpha, modSel = modSel)
})

names(simres) <- names(gmodlist)

# sim_adf returns a proportion of Unit root decisions
# recode so that it is proportion "correct" decisions
# i.e., stationary and deterministic trends should be identified as no unit root

simres$stat <- 1 - simres$stat
simres$stat_trend <- 1 - simres$stat_trend

# save object
saveRDS(simres,  "files/simresults_adftest.RDS")

# ------------------------------------------------------------------------
# ------------------- Additional Condition 1: Vary Phi -------------------
# ------------------------------------------------------------------------

# ----- Vary the phi ; .25 and .75
stat_lowp <- list(c = 0, ltrend = 0, init = 0,
             qtrend = 0, phi = .25, noise = .5,
             adfmod = "type1")
stat_trend_lowp <- list(c = 0, ltrend = .5, init = 0 ,
                   qtrend = 0, phi = .25, noise = .5,
                   adfmod = "type3")

stat_highp <- list(c = 0, ltrend = 0, init = 0,
                  qtrend = 0, phi = .75, noise = .5,
                  adfmod = "type1")
stat_trend_highp <- list(c = 0, ltrend = .5, init = 0 ,
                        qtrend = 0, phi = .75, noise = .5,
                        adfmod = "type3")

# ---- Vary noise levels

gmodlist_phi <- list(
                 stat_lowp = stat_lowp,
                 stat_trend_lowp = stat_trend_lowp,
                 stat_highp = stat_highp,
                 stat_trend_highp = stat_trend_highp
)

# ---- 



# Run simulation
simres_phi <- lapply(gmodlist_phi, function(gmod){
  sim_adf(tpoints_vec, gmod = gmod, alpha = alpha, modSel = modSel)
})

names(simres_phi) <- names(gmodlist_phi)

# sim_adf returns a proportion of Unit root decisions
# recode so that it is proportion "correct" decisions
# i.e., stationary and deterministic trends should be identified as no unit root

simres_phi$stat_lowp <- 1 - simres_phi$stat_lowp
simres_phi$stat_highp <- 1 - simres_phi$stat_highp
simres_phi$stat_trend_lowp <- 1 - simres_phi$stat_trend_lowp
simres_phi$stat_trend_highp <- 1 - simres_phi$stat_trend_highp

# save object
saveRDS(simres_phi,  "files/simresults_adftest_phi.RDS")

# ------------------------------------------------------------------------
# ------------------- Additional Condition 2: Vary Noise Lower -----------
# ------------------------------------------------------------------------

# specify data-generating models
# adfmod specifies what the "correct" ADF model to use is
# for the aTSA::adf.test function
stat <- list(c = 0, ltrend = 0, init = 0,
             qtrend = 0, phi = .5, noise = .25,
             adfmod = "type1")
stat_trend <- list(c = 0, ltrend = .5, init = 0 ,
                   qtrend = 0, phi = .5, noise = .25,
                   adfmod = "type3")
randwalk <- list(c = 0, ltrend = 0, init = 0 ,
                 qtrend = 0, phi = 1, noise = .25,
                 adfmod = "type1")
randwalk_drift <- list(c = .25, ltrend = 0, init = 0 ,
                       qtrend = 0, phi = 1, noise = .25,
                       adfmod = "type2")
randwalk_drift_trend <- list(c = .25, ltrend = .5, init = 0 ,
                             qtrend = 0, phi = 1, noise = .25,
                             adfmod = "type3")

gmodlist_lowvar <- list(stat = stat,
                 stat_trend = stat_trend,
                 randwalk = randwalk,
                 randwalk_drift = randwalk_drift,
                 randwalk_drift_trend = randwalk_drift_trend)


# Run simulation
simres_lowvar <- lapply(gmodlist_lowvar, function(gmod){
  sim_adf(tpoints_vec, gmod = gmod, alpha = alpha, modSel = modSel)
})

names(simres_lowvar) <- names(gmodlist_lowvar)

# sim_adf returns a proportion of Unit root decisions
# recode so that it is proportion "correct" decisions
# i.e., stationary and deterministic trends should be identified as no unit root

simres_lowvar$stat <- 1 - simres_lowvar$stat
simres_lowvar$stat_trend <- 1 - simres_lowvar$stat_trend

# save object
saveRDS(simres_lowvar,  "files/simresults_adftest_lowvar.RDS")

# ------------- higher var --------------------------

# specify data-generating models
# adfmod specifies what the "correct" ADF model to use is
# for the aTSA::adf.test function
stat <- list(c = 0, ltrend = 0, init = 0,
             qtrend = 0, phi = .5, noise = .75,
             adfmod = "type1")
stat_trend <- list(c = 0, ltrend = .5, init = 0 ,
                   qtrend = 0, phi = .5, noise = .75,
                   adfmod = "type3")
randwalk <- list(c = 0, ltrend = 0, init = 0 ,
                 qtrend = 0, phi = 1, noise = .75,
                 adfmod = "type1")
randwalk_drift <- list(c = .25, ltrend = 0, init = 0 ,
                       qtrend = 0, phi = 1, noise = .75,
                       adfmod = "type2")
randwalk_drift_trend <- list(c = .25, ltrend = .5, init = 0 ,
                             qtrend = 0, phi = 1, noise = .75,
                             adfmod = "type3")

gmodlist_hivar <- list(stat = stat,
                 stat_trend = stat_trend,
                 randwalk = randwalk,
                 randwalk_drift = randwalk_drift,
                 randwalk_drift_trend = randwalk_drift_trend)


# Run simulation
simres_hivar <- lapply(gmodlist_hivar, function(gmod){
  sim_adf(tpoints_vec, gmod = gmod, alpha = alpha, modSel = modSel)
})

names(simres_hivar) <- names(gmodlist_hivar)

# sim_adf returns a proportion of Unit root decisions
# recode so that it is proportion "correct" decisions
# i.e., stationary and deterministic trends should be identified as no unit root

simres_hivar$stat <- 1 - simres_hivar$stat
simres_hivar$stat_trend <- 1 - simres_hivar$stat_trend

# save object
saveRDS(simres_hivar,  "files/simresults_adftest_hivar.RDS")

