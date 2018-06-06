#----------------------------------------------------------------
# Examining a Most Likely Case for Strong Campaign Effects: 
# Hitler's Speeches and the Rise of the Nazi Party, 1927--1933
# Peter Selb (University of Konstanz)
# Simon Munzert (MZES, University of Mannheim)
# 2016
#----------------------------------------------------------------

## imports
c("county_df_pointdata_long.RData",
  "county_pres_df_pointdata_long.RData",
  "community_df_pointdata_long.RData")

## exports
c("../figures/tab-effect-voteshare-models-countysize-1930.tex",
  "../figures/tab-effect-voteshare-kpd-models-countysize-1930.tex",
  "../figures/tab-effect-turnout-models-countysize-1930.tex",
  "../figures/tab-effect-voteshare-models-countysize-1932-1.tex",
  "../figures/tab-effect-voteshare-kpd-models-countysize-1932-1.tex",
  "../figures/tab-effect-turnout-models-countysize-1932-1.tex",
  "../figures/tab-effect-voteshare-models-countysize-1932-2.tex",
  "../figures/tab-effect-voteshare-kpd-models-countysize-1932-2.tex",
  "../figures/tab-effect-turnout-models-countysize-1932-2.tex",
  "../figures/tab-effect-voteshare-models-countysize-1933.tex",
  "../figures/tab-effect-voteshare-kpd-models-countysize-1933.tex",
  "../figures/tab-effect-turnout-models-countysize-1933.tex",
  "../figures/effects_coefplot_countysize.pdf")


## load packages and functions -------------------
source("packages.r")
source("functions.r")
source("functions-analysis.r")


## import prepared data --------------------------
load("county_df_pointdata_long.RData")
load("county_pres_df_pointdata_long.RData")
load("community_df_pointdata_long.RData")





## county size -----------------------------

# 1. subset data frames by county size
# 2. then match
# 3. then estimate

# overview
county_df_long$wbcut <- cut(county_df_long$wb, breaks = c(0, 20000, 50000, 80000, max(county_df_long$wb, na.rm = TRUE)), include.lowest = TRUE, labels = c("N_elig < 20,000", "20000 <= N_elig < 50000", "50000 <= N_elig < 80000", "N_elig >= 80000"))
table(county_df_long$wbcut)

## only counties with wb <= 20000
county_df_long_wb1 <- filter(county_df_long, wb <= 20000)


# model NSDAP vote share
model_voteshare_wb1_formula_simple <- paste("p_nsdap ~ time + exposure + timeXexposure",sep = "") %>% as.formula
model_voteshare_wb1_fullsample_simple <- list()
model_voteshare_wb1_matched_simple <- list()
for (i in 1:4) {
  # full sample, lm simple model with voting population weights (i.e., only number of voters is considered)
  dat_full <- getDatFull("county_df_long_wb1", election = i+1, treatment = "visit_10km")
  model_voteshare_wb1_fullsample_simple[[i]] <- lm(model_voteshare_wb1_formula_simple, data =  dat_full, weights = dat_full$gs)
  model_voteshare_wb1_fullsample_simple[[i]]$clusterse <- runClusterRobustOLS(model_voteshare_wb1_fullsample_simple[[i]], dat_full$lfnr)
  # 1:1 matching, lm simple model with voting population weights
  dat_raw <- county_df_long_wb1[county_df_long_wb1$election == i+1,]
  dat_match <- performMatch("visit_10km", dat.raw = "dat_raw", electiontype = "county")
  dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "county_df_long_wb1", election = i+1, treatment = "visit_10km")
  model_voteshare_wb1_matched_simple[[i]] <- lm(model_voteshare_wb1_formula_simple, data =  dat_match, weights = dat_match$gs)
  model_voteshare_wb1_matched_simple[[i]]$clusterse <- runClusterRobustOLS(model_voteshare_wb1_matched_simple[[i]], dat_match$lfnr)
}

# model KPD vote share
model_voteshare_kpd_wb1_formula_simple <- paste("p_kpd ~ time + exposure + timeXexposure",sep = "") %>% as.formula
model_voteshare_kpd_wb1_fullsample_simple <- list()
model_voteshare_kpd_wb1_matched_simple <- list()
for (i in 1:4) {
  # full sample, lm simple model with voting population weights (i.e., only number of voters is considered)
  dat_full <- getDatFull("county_df_long_wb1", election = i+1, treatment = "visit_10km")
  model_voteshare_kpd_wb1_fullsample_simple[[i]] <- lm(model_voteshare_kpd_wb1_formula_simple, data =  dat_full, weights = dat_full$gs)
  model_voteshare_kpd_wb1_fullsample_simple[[i]]$clusterse <- runClusterRobustOLS(model_voteshare_kpd_wb1_fullsample_simple[[i]], dat_full$lfnr)
  # 1:1 matching, lm simple model with voting population weights
  dat_raw <- county_df_long_wb1[county_df_long_wb1$election == i+1,]
  dat_match <- performMatch("visit_10km", dat.raw = "dat_raw", electiontype = "county")
  dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "county_df_long_wb1", election = i+1, treatment = "visit_10km")
  model_voteshare_kpd_wb1_matched_simple[[i]] <- lm(model_voteshare_kpd_wb1_formula_simple, data =  dat_match, weights = dat_match$gs)
  model_voteshare_kpd_wb1_matched_simple[[i]]$clusterse <- runClusterRobustOLS(model_voteshare_kpd_wb1_matched_simple[[i]], dat_match$lfnr)
}

# model turnout
model_turnout_wb1_formula_simple <- paste("p_turnout ~ time + exposure + timeXexposure",sep = "") %>% as.formula
model_turnout_wb1_fullsample_simple <- list()
model_turnout_wb1_matched_simple <- list()
for (i in 1:4) {
  # full sample, lm simple model with voting population weights (i.e., only number of voters is considered)
  dat_full <- getDatFull("county_df_long_wb1", election = i+1, treatment = "visit_10km")
  model_turnout_wb1_fullsample_simple[[i]] <- lm(model_turnout_wb1_formula_simple, data =  dat_full, weights = dat_full$gs)
  model_turnout_wb1_fullsample_simple[[i]]$clusterse <- runClusterRobustOLS(model_turnout_wb1_fullsample_simple[[i]], dat_full$lfnr)
  # 1:1 matching, lm simple model with voting population weights
  dat_raw <- county_df_long_wb1[county_df_long_wb1$election == i+1,]
  dat_match <- performMatch("visit_10km", dat.raw = "dat_raw", electiontype = "county")
  dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "county_df_long_wb1", election = i+1, treatment = "visit_10km")
  model_turnout_wb1_matched_simple[[i]] <- lm(model_turnout_wb1_formula_simple, data =  dat_match, weights = dat_match$wbht)
  model_turnout_wb1_matched_simple[[i]]$clusterse <- runClusterRobustOLS(model_turnout_wb1_matched_simple[[i]], dat_match$lfnr)
}


## only counties with wb > 20000 & wb <= 50000
county_df_long_wb2 <- filter(county_df_long, wb > 20000 & wb <= 50000)

# model NSDAP vote share
model_voteshare_wb2_formula_simple <- paste("p_nsdap ~ time + exposure + timeXexposure",sep = "") %>% as.formula
model_voteshare_wb2_fullsample_simple <- list()
model_voteshare_wb2_matched_simple <- list()
for (i in 1:4) {
  # full sample, lm simple model with voting population weights (i.e., only number of voters is considered)
  dat_full <- getDatFull("county_df_long_wb2", election = i+1, treatment = "visit_10km")
  model_voteshare_wb2_fullsample_simple[[i]] <- lm(model_voteshare_wb2_formula_simple, data =  dat_full, weights = dat_full$gs)
  model_voteshare_wb2_fullsample_simple[[i]]$clusterse <- runClusterRobustOLS(model_voteshare_wb2_fullsample_simple[[i]], dat_full$lfnr)
  # 1:1 matching, lm simple model with voting population weights
  dat_raw <- county_df_long_wb2[county_df_long_wb2$election == i+1,]
  dat_match <- performMatch("visit_10km", dat.raw = "dat_raw", electiontype = "county")
  dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "county_df_long_wb2", election = i+1, treatment = "visit_10km")
  model_voteshare_wb2_matched_simple[[i]] <- lm(model_voteshare_wb2_formula_simple, data =  dat_match, weights = dat_match$gs)
  model_voteshare_wb2_matched_simple[[i]]$clusterse <- runClusterRobustOLS(model_voteshare_wb2_matched_simple[[i]], dat_match$lfnr)
}

# model KPD vote share
model_voteshare_kpd_wb2_formula_simple <- paste("p_kpd ~ time + exposure + timeXexposure",sep = "") %>% as.formula
model_voteshare_kpd_wb2_fullsample_simple <- list()
model_voteshare_kpd_wb2_matched_simple <- list()
for (i in 1:4) {
  # full sample, lm simple model with voting population weights (i.e., only number of voters is considered)
  dat_full <- getDatFull("county_df_long_wb2", election = i+1, treatment = "visit_10km")
  model_voteshare_kpd_wb2_fullsample_simple[[i]] <- lm(model_voteshare_kpd_wb2_formula_simple, data =  dat_full, weights = dat_full$gs)
  model_voteshare_kpd_wb2_fullsample_simple[[i]]$clusterse <- runClusterRobustOLS(model_voteshare_kpd_wb2_fullsample_simple[[i]], dat_full$lfnr)
  # 1:1 matching, lm simple model with voting population weights
  dat_raw <- county_df_long_wb2[county_df_long_wb2$election == i+1,]
  dat_match <- performMatch("visit_10km", dat.raw = "dat_raw", electiontype = "county")
  dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "county_df_long_wb2", election = i+1, treatment = "visit_10km")
  model_voteshare_kpd_wb2_matched_simple[[i]] <- lm(model_voteshare_kpd_wb2_formula_simple, data =  dat_match, weights = dat_match$gs)
  model_voteshare_kpd_wb2_matched_simple[[i]]$clusterse <- runClusterRobustOLS(model_voteshare_kpd_wb2_matched_simple[[i]], dat_match$lfnr)
}

# model turnout
model_turnout_wb2_formula_simple <- paste("p_turnout ~ time + exposure + timeXexposure",sep = "") %>% as.formula
model_turnout_wb2_fullsample_simple <- list()
model_turnout_wb2_matched_simple <- list()
for (i in 1:4) {
  # full sample, lm simple model with voting population weights (i.e., only number of voters is considered)
  dat_full <- getDatFull("county_df_long_wb2", election = i+1, treatment = "visit_10km")
  model_turnout_wb2_fullsample_simple[[i]] <- lm(model_turnout_wb2_formula_simple, data =  dat_full, weights = dat_full$gs)
  model_turnout_wb2_fullsample_simple[[i]]$clusterse <- runClusterRobustOLS(model_turnout_wb2_fullsample_simple[[i]], dat_full$lfnr)
  # 1:1 matching, lm simple model with voting population weights
  dat_raw <- county_df_long_wb2[county_df_long_wb2$election == i+1,]
  dat_match <- performMatch("visit_10km", dat.raw = "dat_raw", electiontype = "county")
  dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "county_df_long_wb2", election = i+1, treatment = "visit_10km")
  model_turnout_wb2_matched_simple[[i]] <- lm(model_turnout_wb2_formula_simple, data =  dat_match, weights = dat_match$wbht)
  model_turnout_wb2_matched_simple[[i]]$clusterse <- runClusterRobustOLS(model_turnout_wb2_matched_simple[[i]], dat_match$lfnr)
}



## only counties with wb > 50000 & wb <= 80000
county_df_long_wb3 <- filter(county_df_long, wb > 50000 & wb <= 80000)

# model NSDAP vote share
model_voteshare_wb3_formula_simple <- paste("p_nsdap ~ time + exposure + timeXexposure",sep = "") %>% as.formula
model_voteshare_wb3_fullsample_simple <- list()
model_voteshare_wb3_matched_simple <- list()
for (i in 1:4) {
  # full sample, lm simple model with voting population weights (i.e., only number of voters is considered)
  dat_full <- getDatFull("county_df_long_wb3", election = i+1, treatment = "visit_10km")
  model_voteshare_wb3_fullsample_simple[[i]] <- lm(model_voteshare_wb3_formula_simple, data =  dat_full, weights = dat_full$gs)
  model_voteshare_wb3_fullsample_simple[[i]]$clusterse <- runClusterRobustOLS(model_voteshare_wb3_fullsample_simple[[i]], dat_full$lfnr)
  # 1:1 matching, lm simple model with voting population weights
  dat_raw <- county_df_long_wb3[county_df_long_wb3$election == i+1,]
  dat_match <- performMatch("visit_10km", dat.raw = "dat_raw", electiontype = "county")
  dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "county_df_long_wb3", election = i+1, treatment = "visit_10km")
  model_voteshare_wb3_matched_simple[[i]] <- lm(model_voteshare_wb3_formula_simple, data =  dat_match, weights = dat_match$gs)
  model_voteshare_wb3_matched_simple[[i]]$clusterse <- runClusterRobustOLS(model_voteshare_wb3_matched_simple[[i]], dat_match$lfnr)
}

# model KPD vote share
model_voteshare_kpd_wb3_formula_simple <- paste("p_kpd ~ time + exposure + timeXexposure",sep = "") %>% as.formula
model_voteshare_kpd_wb3_fullsample_simple <- list()
model_voteshare_kpd_wb3_matched_simple <- list()
for (i in 1:4) {
  # full sample, lm simple model with voting population weights (i.e., only number of voters is considered)
  dat_full <- getDatFull("county_df_long_wb3", election = i+1, treatment = "visit_10km")
  model_voteshare_kpd_wb3_fullsample_simple[[i]] <- lm(model_voteshare_kpd_wb3_formula_simple, data =  dat_full, weights = dat_full$gs)
  model_voteshare_kpd_wb3_fullsample_simple[[i]]$clusterse <- runClusterRobustOLS(model_voteshare_kpd_wb3_fullsample_simple[[i]], dat_full$lfnr)
  # 1:1 matching, lm simple model with voting population weights
  dat_raw <- county_df_long_wb3[county_df_long_wb3$election == i+1,]
  dat_match <- performMatch("visit_10km", dat.raw = "dat_raw", electiontype = "county")
  dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "county_df_long_wb3", election = i+1, treatment = "visit_10km")
  model_voteshare_kpd_wb3_matched_simple[[i]] <- lm(model_voteshare_kpd_wb3_formula_simple, data =  dat_match, weights = dat_match$gs)
  model_voteshare_kpd_wb3_matched_simple[[i]]$clusterse <- runClusterRobustOLS(model_voteshare_kpd_wb3_matched_simple[[i]], dat_match$lfnr)
}

# model turnout
model_turnout_wb3_formula_simple <- paste("p_turnout ~ time + exposure + timeXexposure",sep = "") %>% as.formula
model_turnout_wb3_fullsample_simple <- list()
model_turnout_wb3_matched_simple <- list()
for (i in 1:4) {
  # full sample, lm simple model with voting population weights (i.e., only number of voters is considered)
  dat_full <- getDatFull("county_df_long_wb3", election = i+1, treatment = "visit_10km")
  model_turnout_wb3_fullsample_simple[[i]] <- lm(model_turnout_wb3_formula_simple, data =  dat_full, weights = dat_full$gs)
  model_turnout_wb3_fullsample_simple[[i]]$clusterse <- runClusterRobustOLS(model_turnout_wb3_fullsample_simple[[i]], dat_full$lfnr)
  # 1:1 matching, lm simple model with voting population weights
  dat_raw <- county_df_long_wb3[county_df_long_wb3$election == i+1,]
  dat_match <- performMatch("visit_10km", dat.raw = "dat_raw", electiontype = "county")
  dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "county_df_long_wb3", election = i+1, treatment = "visit_10km")
  model_turnout_wb3_matched_simple[[i]] <- lm(model_turnout_wb3_formula_simple, data =  dat_match, weights = dat_match$wbht)
  model_turnout_wb3_matched_simple[[i]]$clusterse <- runClusterRobustOLS(model_turnout_wb3_matched_simple[[i]], dat_match$lfnr)
}


## only counties with wb > 80000
county_df_long_wb4 <- filter(county_df_long, wb > 80000)

# model NSDAP vote share
model_voteshare_wb4_formula_simple <- paste("p_nsdap ~ time + exposure + timeXexposure",sep = "") %>% as.formula
model_voteshare_wb4_fullsample_simple <- list()
model_voteshare_wb4_matched_simple <- list()
for (i in 1:4) {
  # full sample, lm simple model with voting population weights (i.e., only number of voters is considered)
  dat_full <- getDatFull("county_df_long_wb4", election = i+1, treatment = "visit_10km")
  model_voteshare_wb4_fullsample_simple[[i]] <- lm(model_voteshare_wb4_formula_simple, data =  dat_full, weights = dat_full$gs)
  model_voteshare_wb4_fullsample_simple[[i]]$clusterse <- runClusterRobustOLS(model_voteshare_wb4_fullsample_simple[[i]], dat_full$lfnr)
  # 1:1 matching, lm simple model with voting population weights
  dat_raw <- county_df_long_wb4[county_df_long_wb4$election == i+1,]
  dat_match <- performMatch("visit_10km", dat.raw = "dat_raw", electiontype = "county")
  dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "county_df_long_wb4", election = i+1, treatment = "visit_10km")
  model_voteshare_wb4_matched_simple[[i]] <- lm(model_voteshare_wb4_formula_simple, data =  dat_match, weights = dat_match$gs)
  model_voteshare_wb4_matched_simple[[i]]$clusterse <- runClusterRobustOLS(model_voteshare_wb4_matched_simple[[i]], dat_match$lfnr)
}

# model KPD vote share
model_voteshare_kpd_wb4_formula_simple <- paste("p_kpd ~ time + exposure + timeXexposure",sep = "") %>% as.formula
model_voteshare_kpd_wb4_fullsample_simple <- list()
model_voteshare_kpd_wb4_matched_simple <- list()
for (i in 1:4) {
  # full sample, lm simple model with voting population weights (i.e., only number of voters is considered)
  dat_full <- getDatFull("county_df_long_wb4", election = i+1, treatment = "visit_10km")
  model_voteshare_kpd_wb4_fullsample_simple[[i]] <- lm(model_voteshare_kpd_wb4_formula_simple, data =  dat_full, weights = dat_full$gs)
  model_voteshare_kpd_wb4_fullsample_simple[[i]]$clusterse <- runClusterRobustOLS(model_voteshare_kpd_wb4_fullsample_simple[[i]], dat_full$lfnr)
  # 1:1 matching, lm simple model with voting population weights
  dat_raw <- county_df_long_wb4[county_df_long_wb4$election == i+1,]
  dat_match <- performMatch("visit_10km", dat.raw = "dat_raw", electiontype = "county")
  dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "county_df_long_wb4", election = i+1, treatment = "visit_10km")
  model_voteshare_kpd_wb4_matched_simple[[i]] <- lm(model_voteshare_kpd_wb4_formula_simple, data =  dat_match, weights = dat_match$gs)
  model_voteshare_kpd_wb4_matched_simple[[i]]$clusterse <- runClusterRobustOLS(model_voteshare_kpd_wb4_matched_simple[[i]], dat_match$lfnr)
}

# model turnout
model_turnout_wb4_formula_simple <- paste("p_turnout ~ time + exposure + timeXexposure",sep = "") %>% as.formula
model_turnout_wb4_fullsample_simple <- list()
model_turnout_wb4_matched_simple <- list()
for (i in 1:4) {
  # full sample, lm simple model with voting population weights (i.e., only number of voters is considered)
  dat_full <- getDatFull("county_df_long_wb4", election = i+1, treatment = "visit_10km")
  model_turnout_wb4_fullsample_simple[[i]] <- lm(model_turnout_wb4_formula_simple, data =  dat_full, weights = dat_full$gs)
  model_turnout_wb4_fullsample_simple[[i]]$clusterse <- runClusterRobustOLS(model_turnout_wb4_fullsample_simple[[i]], dat_full$lfnr)
  # 1:1 matching, lm simple model with voting population weights
  dat_raw <- county_df_long_wb4[county_df_long_wb4$election == i+1,]
  dat_match <- performMatch("visit_10km", dat.raw = "dat_raw", electiontype = "county")
  dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "county_df_long_wb4", election = i+1, treatment = "visit_10km")
  model_turnout_wb4_matched_simple[[i]] <- lm(model_turnout_wb4_formula_simple, data =  dat_match, weights = dat_match$wbht)
  model_turnout_wb4_matched_simple[[i]]$clusterse <- runClusterRobustOLS(model_turnout_wb4_matched_simple[[i]], dat_match$lfnr)
}



## tables ----------

# NSDAP vote share models, 1930 election, varying county size
model_voteshare_list <- list(model_voteshare_wb1_fullsample_simple[[1]], model_voteshare_wb1_matched_simple[[1]], 
                             model_voteshare_wb2_fullsample_simple[[1]], model_voteshare_wb2_matched_simple[[1]], 
                             model_voteshare_wb3_fullsample_simple[[1]], model_voteshare_wb3_matched_simple[[1]], 
                             model_voteshare_wb4_fullsample_simple[[1]], model_voteshare_wb4_matched_simple[[1]])
model_voteshare_ses <- list(model_voteshare_wb1_fullsample_simple[[1]]$clusterse[,2], model_voteshare_wb1_matched_simple[[1]]$clusterse[,2],
                            model_voteshare_wb2_fullsample_simple[[1]]$clusterse[,2], model_voteshare_wb2_matched_simple[[1]]$clusterse[,2],
                            model_voteshare_wb3_fullsample_simple[[1]]$clusterse[,2], model_voteshare_wb3_matched_simple[[1]]$clusterse[,2],
                            model_voteshare_wb4_fullsample_simple[[1]]$clusterse[,2], model_voteshare_wb4_matched_simple[[1]]$clusterse[,2])
obs <- c(length(model_voteshare_wb1_fullsample_simple[[1]]$residuals)/2,
         length(model_voteshare_wb1_matched_simple[[1]]$residuals)/2,
         length(model_voteshare_wb2_fullsample_simple[[1]]$residuals)/2,
         length(model_voteshare_wb2_matched_simple[[1]]$residuals)/2,
         length(model_voteshare_wb3_fullsample_simple[[1]]$residuals)/2,
         length(model_voteshare_wb3_matched_simple[[1]]$residuals)/2,
         length(model_voteshare_wb4_fullsample_simple[[1]]$residuals)/2,
         length(model_voteshare_wb4_matched_simple[[1]]$residuals)/2)
varnames_long <- c("\\cmidrule(r){2-3}\\cmidrule(r){4-5}\\cmidrule(r){6-7}\\cmidrule(r){8-9}Exposure, county size of...", "(Intercept)")
election_names <- c("... less than 20k ", "... between 20k and 50k", "... between 50k and 80k", "... more than 80k")
effect_models_tex <- stargazer(model_voteshare_list, 
                               dep.var.caption = "", #dep.var.caption = "NSDAP/Hitler vote share", 
                               omit.table.layout = "d",
                               covariate.labels = varnames_long, 
                               model.numbers = FALSE, 
                               keep=c("Constant", "^timeXexposure"),
                               omit.stat = c("rsq", "res.dev", "ser", "n", "f"),
                               no.space = TRUE,
                               df = FALSE, 
                               se = model_voteshare_ses,
                               add.lines = list(c("Sample", rep(c("full", "matched"), 4)),
                                                c("Observations", obs)),
                               notes        = "DID models with number of actual voters as pop weights. Clustered SEs shown. $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01", 
                               notes.append = FALSE,
                               column.labels = election_names, 
                               column.separate = c(2, 2, 2, 2), 
                               font.size = "scriptsize",
                               title = paste0("Diff-in-diff estimates of exposure effects on NSDAP vote share at the 1930 national parliamentary election with varying county size specifications.\\vspace{-.25cm}"), 
                               label = paste0("tab:nsdap-voteshare-countysize-dd-1930"),
                               out = paste0("../figures/tab-effect-voteshare-models-countysize-1930.tex"))

# KPD vote share models, 1930 election, varying county size
model_voteshare_kpd_list <- list(model_voteshare_kpd_wb1_fullsample_simple[[1]], model_voteshare_kpd_wb1_matched_simple[[1]], 
                                 model_voteshare_kpd_wb2_fullsample_simple[[1]], model_voteshare_kpd_wb2_matched_simple[[1]], 
                                 model_voteshare_kpd_wb3_fullsample_simple[[1]], model_voteshare_kpd_wb3_matched_simple[[1]], 
                                 model_voteshare_kpd_wb4_fullsample_simple[[1]], model_voteshare_kpd_wb4_matched_simple[[1]])
model_voteshare_kpd_ses <- list(model_voteshare_kpd_wb1_fullsample_simple[[1]]$clusterse[,2], model_voteshare_kpd_wb1_matched_simple[[1]]$clusterse[,2],
                                model_voteshare_kpd_wb2_fullsample_simple[[1]]$clusterse[,2], model_voteshare_kpd_wb2_matched_simple[[1]]$clusterse[,2],
                                model_voteshare_kpd_wb3_fullsample_simple[[1]]$clusterse[,2], model_voteshare_kpd_wb3_matched_simple[[1]]$clusterse[,2],
                                model_voteshare_kpd_wb4_fullsample_simple[[1]]$clusterse[,2], model_voteshare_kpd_wb4_matched_simple[[1]]$clusterse[,2])
obs <- c(length(model_voteshare_kpd_wb1_fullsample_simple[[1]]$residuals)/2,
         length(model_voteshare_kpd_wb1_matched_simple[[1]]$residuals)/2,
         length(model_voteshare_kpd_wb2_fullsample_simple[[1]]$residuals)/2,
         length(model_voteshare_kpd_wb2_matched_simple[[1]]$residuals)/2,
         length(model_voteshare_kpd_wb3_fullsample_simple[[1]]$residuals)/2,
         length(model_voteshare_kpd_wb3_matched_simple[[1]]$residuals)/2,
         length(model_voteshare_kpd_wb4_fullsample_simple[[1]]$residuals)/2,
         length(model_voteshare_kpd_wb4_matched_simple[[1]]$residuals)/2)
varnames_long <- c("\\cmidrule(r){2-3}\\cmidrule(r){4-5}\\cmidrule(r){6-7}\\cmidrule(r){8-9}Exposure, county size of...", "(Intercept)")
election_names <- c("... less than 20k ", "... between 20k and 50k", "... between 50k and 80k", "... more than 80k")
effect_models_tex <- stargazer(model_voteshare_kpd_list, 
                               dep.var.caption = "", #dep.var.caption = "NSDAP/Hitler vote share", 
                               omit.table.layout = "d",
                               covariate.labels = varnames_long, 
                               model.numbers = FALSE, 
                               keep=c("Constant", "^timeXexposure"),
                               omit.stat = c("rsq", "res.dev", "ser", "n", "f"),
                               no.space = TRUE,
                               df = FALSE, 
                               se = model_voteshare_kpd_ses,
                               add.lines = list(c("Sample", rep(c("full", "matched"), 4)),
                                                c("Observations", obs)),
                               notes        = "DID models with number of actual voters as pop weights. Clustered SEs shown. $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01", 
                               notes.append = FALSE,
                               column.labels = election_names, 
                               column.separate = c(2, 2, 2, 2), 
                               font.size = "scriptsize",
                               title = paste0("Diff-in-diff estimates of exposure effects on KPD vote share at the 1930 national parliamentary election with varying county size specifications.\\vspace{-.25cm}"), 
                               label = paste0("tab:nsdap-voteshare-kpd-countysize-dd-1930"),
                               out = paste0("../figures/tab-effect-voteshare-kpd-models-countysize-1930.tex"))

# turnout models, 1930 election, varying county size
model_turnout_list <- list(model_turnout_wb1_fullsample_simple[[1]], model_turnout_wb1_matched_simple[[1]], 
                           model_turnout_wb2_fullsample_simple[[1]], model_turnout_wb2_matched_simple[[1]], 
                           model_turnout_wb3_fullsample_simple[[1]], model_turnout_wb3_matched_simple[[1]], 
                           model_turnout_wb4_fullsample_simple[[1]], model_turnout_wb4_matched_simple[[1]])
model_turnout_ses <- list(model_turnout_wb1_fullsample_simple[[1]]$clusterse[,2], model_turnout_wb1_matched_simple[[1]]$clusterse[,2],
                          model_turnout_wb2_fullsample_simple[[1]]$clusterse[,2], model_turnout_wb2_matched_simple[[1]]$clusterse[,2],
                          model_turnout_wb3_fullsample_simple[[1]]$clusterse[,2], model_turnout_wb3_matched_simple[[1]]$clusterse[,2],
                          model_turnout_wb4_fullsample_simple[[1]]$clusterse[,2], model_turnout_wb4_matched_simple[[1]]$clusterse[,2])
obs <- c(length(model_turnout_wb1_fullsample_simple[[1]]$residuals)/2,
         length(model_turnout_wb1_matched_simple[[1]]$residuals)/2,
         length(model_turnout_wb2_fullsample_simple[[1]]$residuals)/2,
         length(model_turnout_wb2_matched_simple[[1]]$residuals)/2,
         length(model_turnout_wb3_fullsample_simple[[1]]$residuals)/2,
         length(model_turnout_wb3_matched_simple[[1]]$residuals)/2,
         length(model_turnout_wb4_fullsample_simple[[1]]$residuals)/2,
         length(model_turnout_wb4_matched_simple[[1]]$residuals)/2)
varnames_long <- c("\\cmidrule(r){2-3}\\cmidrule(r){4-5}\\cmidrule(r){6-7}\\cmidrule(r){8-9}Exposure, county size of...", "(Intercept)")
election_names <- c("... less than 20k ", "... between 20k and 50k", "... between 50k and 80k", "... more than 80k")
effect_models_tex <- stargazer(model_turnout_list, 
                               dep.var.caption = "", #dep.var.caption = "NSDAP/Hitler vote share", 
                               omit.table.layout = "d",
                               covariate.labels = varnames_long, 
                               model.numbers = FALSE, 
                               keep=c("Constant", "^timeXexposure"),
                               omit.stat = c("rsq", "res.dev", "ser", "n", "f"),
                               no.space = TRUE,
                               df = FALSE, 
                               se = model_turnout_ses,
                               add.lines = list(c("Sample", rep(c("full", "matched"), 4)),
                                                c("Observations", obs)),
                               notes        = "DID models with number of actual voters as pop weights. Clustered SEs shown. $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01", 
                               notes.append = FALSE,
                               column.labels = election_names, 
                               column.separate = c(2, 2, 2, 2), 
                               font.size = "scriptsize",
                               title = paste0("Diff-in-diff estimates of exposure effects on turnout at the 1930 national parliamentary election with varying county size specifications.\\vspace{-.25cm}"), 
                               label = paste0("tab:nsdap-turnout-countysize-dd-1930"),
                               out = paste0("../figures/tab-effect-turnout-models-countysize-1930.tex"))

# NSDAP vote share models, 1932-1 election, varying county size
model_voteshare_list <- list(model_voteshare_wb1_fullsample_simple[[2]], model_voteshare_wb1_matched_simple[[2]], 
                             model_voteshare_wb2_fullsample_simple[[2]], model_voteshare_wb2_matched_simple[[2]], 
                             model_voteshare_wb3_fullsample_simple[[2]], model_voteshare_wb3_matched_simple[[2]], 
                             model_voteshare_wb4_fullsample_simple[[2]], model_voteshare_wb4_matched_simple[[2]])
model_voteshare_ses <- list(model_voteshare_wb1_fullsample_simple[[2]]$clusterse[,2], model_voteshare_wb1_matched_simple[[2]]$clusterse[,2],
                            model_voteshare_wb2_fullsample_simple[[2]]$clusterse[,2], model_voteshare_wb2_matched_simple[[2]]$clusterse[,2],
                            model_voteshare_wb3_fullsample_simple[[2]]$clusterse[,2], model_voteshare_wb3_matched_simple[[2]]$clusterse[,2],
                            model_voteshare_wb4_fullsample_simple[[2]]$clusterse[,2], model_voteshare_wb4_matched_simple[[2]]$clusterse[,2])
obs <- c(length(model_voteshare_wb1_fullsample_simple[[2]]$residuals)/2,
         length(model_voteshare_wb1_matched_simple[[2]]$residuals)/2,
         length(model_voteshare_wb2_fullsample_simple[[2]]$residuals)/2,
         length(model_voteshare_wb2_matched_simple[[2]]$residuals)/2,
         length(model_voteshare_wb3_fullsample_simple[[2]]$residuals)/2,
         length(model_voteshare_wb3_matched_simple[[2]]$residuals)/2,
         length(model_voteshare_wb4_fullsample_simple[[2]]$residuals)/2,
         length(model_voteshare_wb4_matched_simple[[2]]$residuals)/2)
varnames_long <- c("\\cmidrule(r){2-3}\\cmidrule(r){4-5}\\cmidrule(r){6-7}\\cmidrule(r){8-9}Exposure, county size of...", "(Intercept)")
election_names <- c("... less than 20k ", "... between 20k and 50k", "... between 50k and 80k", "... more than 80k")
effect_models_tex <- stargazer(model_voteshare_list, 
                               dep.var.caption = "", #dep.var.caption = "NSDAP/Hitler vote share", 
                               omit.table.layout = "d",
                               covariate.labels = varnames_long, 
                               model.numbers = FALSE, 
                               keep=c("Constant", "^timeXexposure"),
                               omit.stat = c("rsq", "res.dev", "ser", "n", "f"),
                               no.space = TRUE,
                               df = FALSE, 
                               se = model_voteshare_ses,
                               add.lines = list(c("Sample", rep(c("full", "matched"), 4)),
                                                c("Observations", obs)),
                               notes        = "DID models with number of actual voters as pop weights. Clustered SEs shown. $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01", 
                               notes.append = FALSE,
                               column.labels = election_names, 
                               column.separate = c(2, 2, 2, 2), 
                               font.size = "scriptsize",
                               title = paste0("Diff-in-diff estimates of exposure effects on NSDAP vote share at the Jul 1932 national parliamentary election with varying county size specifications.\\vspace{-.25cm}"), 
                               label = paste0("tab:nsdap-voteshare-countysize-dd-1932-1"),
                               out = paste0("../figures/tab-effect-voteshare-models-countysize-1932-1.tex"))

# KPD vote share models, 1932-1 election, varying county size
model_voteshare_kpd_list <- list(model_voteshare_kpd_wb1_fullsample_simple[[2]], model_voteshare_kpd_wb1_matched_simple[[2]], 
                                 model_voteshare_kpd_wb2_fullsample_simple[[2]], model_voteshare_kpd_wb2_matched_simple[[2]], 
                                 model_voteshare_kpd_wb3_fullsample_simple[[2]], model_voteshare_kpd_wb3_matched_simple[[2]], 
                                 model_voteshare_kpd_wb4_fullsample_simple[[2]], model_voteshare_kpd_wb4_matched_simple[[2]])
model_voteshare_kpd_ses <- list(model_voteshare_kpd_wb1_fullsample_simple[[2]]$clusterse[,2], model_voteshare_kpd_wb1_matched_simple[[2]]$clusterse[,2],
                                model_voteshare_kpd_wb2_fullsample_simple[[2]]$clusterse[,2], model_voteshare_kpd_wb2_matched_simple[[2]]$clusterse[,2],
                                model_voteshare_kpd_wb3_fullsample_simple[[2]]$clusterse[,2], model_voteshare_kpd_wb3_matched_simple[[2]]$clusterse[,2],
                                model_voteshare_kpd_wb4_fullsample_simple[[2]]$clusterse[,2], model_voteshare_kpd_wb4_matched_simple[[2]]$clusterse[,2])
obs <- c(length(model_voteshare_kpd_wb1_fullsample_simple[[2]]$residuals)/2,
         length(model_voteshare_kpd_wb1_matched_simple[[2]]$residuals)/2,
         length(model_voteshare_kpd_wb2_fullsample_simple[[2]]$residuals)/2,
         length(model_voteshare_kpd_wb2_matched_simple[[2]]$residuals)/2,
         length(model_voteshare_kpd_wb3_fullsample_simple[[2]]$residuals)/2,
         length(model_voteshare_kpd_wb3_matched_simple[[2]]$residuals)/2,
         length(model_voteshare_kpd_wb4_fullsample_simple[[2]]$residuals)/2,
         length(model_voteshare_kpd_wb4_matched_simple[[2]]$residuals)/2)
varnames_long <- c("\\cmidrule(r){2-3}\\cmidrule(r){4-5}\\cmidrule(r){6-7}\\cmidrule(r){8-9}Exposure, county size of...", "(Intercept)")
election_names <- c("... less than 20k ", "... between 20k and 50k", "... between 50k and 80k", "... more than 80k")
effect_models_tex <- stargazer(model_voteshare_kpd_list, 
                               dep.var.caption = "", #dep.var.caption = "NSDAP/Hitler vote share", 
                               omit.table.layout = "d",
                               covariate.labels = varnames_long, 
                               model.numbers = FALSE, 
                               keep=c("Constant", "^timeXexposure"),
                               omit.stat = c("rsq", "res.dev", "ser", "n", "f"),
                               no.space = TRUE,
                               df = FALSE, 
                               se = model_voteshare_kpd_ses,
                               add.lines = list(c("Sample", rep(c("full", "matched"), 4)),
                                                c("Observations", obs)),
                               notes        = "DID models with number of actual voters as pop weights. Clustered SEs shown. $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01", 
                               notes.append = FALSE,
                               column.labels = election_names, 
                               column.separate = c(2, 2, 2, 2), 
                               font.size = "scriptsize",
                               title = paste0("Diff-in-diff estimates of exposure effects on KPD vote share at the Jul 1932 national parliamentary election with varying county size specifications.\\vspace{-.25cm}"), 
                               label = paste0("tab:nsdap-voteshare-kpd-countysize-dd-1932-1"),
                               out = paste0("../figures/tab-effect-voteshare-kpd-models-countysize-1932-1.tex"))

# turnout models, 1932-1 election, varying county size
model_turnout_list <- list(model_turnout_wb1_fullsample_simple[[2]], model_turnout_wb1_matched_simple[[2]], 
                           model_turnout_wb2_fullsample_simple[[2]], model_turnout_wb2_matched_simple[[2]], 
                           model_turnout_wb3_fullsample_simple[[2]], model_turnout_wb3_matched_simple[[2]], 
                           model_turnout_wb4_fullsample_simple[[2]], model_turnout_wb4_matched_simple[[2]])
model_turnout_ses <- list(model_turnout_wb1_fullsample_simple[[2]]$clusterse[,2], model_turnout_wb1_matched_simple[[2]]$clusterse[,2],
                          model_turnout_wb2_fullsample_simple[[2]]$clusterse[,2], model_turnout_wb2_matched_simple[[2]]$clusterse[,2],
                          model_turnout_wb3_fullsample_simple[[2]]$clusterse[,2], model_turnout_wb3_matched_simple[[2]]$clusterse[,2],
                          model_turnout_wb4_fullsample_simple[[2]]$clusterse[,2], model_turnout_wb4_matched_simple[[2]]$clusterse[,2])
obs <- c(length(model_turnout_wb1_fullsample_simple[[2]]$residuals)/2,
         length(model_turnout_wb1_matched_simple[[2]]$residuals)/2,
         length(model_turnout_wb2_fullsample_simple[[2]]$residuals)/2,
         length(model_turnout_wb2_matched_simple[[2]]$residuals)/2,
         length(model_turnout_wb3_fullsample_simple[[2]]$residuals)/2,
         length(model_turnout_wb3_matched_simple[[2]]$residuals)/2,
         length(model_turnout_wb4_fullsample_simple[[2]]$residuals)/2,
         length(model_turnout_wb4_matched_simple[[2]]$residuals)/2)
varnames_long <- c("\\cmidrule(r){2-3}\\cmidrule(r){4-5}\\cmidrule(r){6-7}\\cmidrule(r){8-9}Exposure, county size of...", "(Intercept)")
election_names <- c("... less than 20k ", "... between 20k and 50k", "... between 50k and 80k", "... more than 80k")
effect_models_tex <- stargazer(model_turnout_list, 
                               dep.var.caption = "", #dep.var.caption = "NSDAP/Hitler vote share", 
                               omit.table.layout = "d",
                               covariate.labels = varnames_long, 
                               model.numbers = FALSE, 
                               keep=c("Constant", "^timeXexposure"),
                               omit.stat = c("rsq", "res.dev", "ser", "n", "f"),
                               no.space = TRUE,
                               df = FALSE, 
                               se = model_turnout_ses,
                               add.lines = list(c("Sample", rep(c("full", "matched"), 4)),
                                                c("Observations", obs)),
                               notes        = "DID models with number of actual voters as pop weights. Clustered SEs shown. $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01", 
                               notes.append = FALSE,
                               column.labels = election_names, 
                               column.separate = c(2, 2, 2, 2), 
                               font.size = "scriptsize",
                               title = paste0("Diff-in-diff estimates of exposure effects on turnout at the Jul 1932 national parliamentary election with varying county size specifications.\\vspace{-.25cm}"), 
                               label = paste0("tab:turnout-countysize-dd-1932-1"),
                               out = paste0("../figures/tab-effect-turnout-models-countysize-1932-1.tex"))


# NSDAP vote share models, 1932-2 election, varying county size
model_voteshare_list <- list(model_voteshare_wb1_fullsample_simple[[3]], model_voteshare_wb1_matched_simple[[3]], 
                             model_voteshare_wb2_fullsample_simple[[3]], model_voteshare_wb2_matched_simple[[3]], 
                             model_voteshare_wb3_fullsample_simple[[3]], model_voteshare_wb3_matched_simple[[3]], 
                             model_voteshare_wb4_fullsample_simple[[3]], model_voteshare_wb4_matched_simple[[3]])
model_voteshare_ses <- list(model_voteshare_wb1_fullsample_simple[[3]]$clusterse[,2], model_voteshare_wb1_matched_simple[[3]]$clusterse[,2],
                            model_voteshare_wb2_fullsample_simple[[3]]$clusterse[,2], model_voteshare_wb2_matched_simple[[3]]$clusterse[,2],
                            model_voteshare_wb3_fullsample_simple[[3]]$clusterse[,2], model_voteshare_wb3_matched_simple[[3]]$clusterse[,2],
                            model_voteshare_wb4_fullsample_simple[[3]]$clusterse[,2], model_voteshare_wb4_matched_simple[[3]]$clusterse[,2])
obs <- c(length(model_voteshare_wb1_fullsample_simple[[3]]$residuals)/2,
         length(model_voteshare_wb1_matched_simple[[3]]$residuals)/2,
         length(model_voteshare_wb2_fullsample_simple[[3]]$residuals)/2,
         length(model_voteshare_wb2_matched_simple[[3]]$residuals)/2,
         length(model_voteshare_wb3_fullsample_simple[[3]]$residuals)/2,
         length(model_voteshare_wb3_matched_simple[[3]]$residuals)/2,
         length(model_voteshare_wb4_fullsample_simple[[3]]$residuals)/2,
         length(model_voteshare_wb4_matched_simple[[3]]$residuals)/2)
varnames_long <- c("\\cmidrule(r){2-3}\\cmidrule(r){4-5}\\cmidrule(r){6-7}\\cmidrule(r){8-9}Exposure, county size of...", "(Intercept)")
election_names <- c("... less than 20k ", "... between 20k and 50k", "... between 50k and 80k", "... more than 80k")
effect_models_tex <- stargazer(model_voteshare_list, 
                               dep.var.caption = "", #dep.var.caption = "NSDAP/Hitler vote share", 
                               omit.table.layout = "d",
                               covariate.labels = varnames_long, 
                               model.numbers = FALSE, 
                               keep=c("Constant", "^timeXexposure"),
                               omit.stat = c("rsq", "res.dev", "ser", "n", "f"),
                               no.space = TRUE,
                               df = FALSE, 
                               se = model_voteshare_ses,
                               add.lines = list(c("Sample", rep(c("full", "matched"), 4)),
                                                c("Observations", obs)),
                               notes        = "DID models with number of actual voters as pop weights. Clustered SEs shown. $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01", 
                               notes.append = FALSE,
                               column.labels = election_names, 
                               column.separate = c(2, 2, 2, 2), 
                               font.size = "scriptsize",
                               title = paste0("Diff-in-diff estimates of exposure effects on NSDAP vote share at the Nov 1932 national parliamentary election with varying county size specifications.\\vspace{-.25cm}"), 
                               label = paste0("tab:nsdap-voteshare-countysize-dd-1932-2"),
                               out = paste0("../figures/tab-effect-voteshare-models-countysize-1932-2.tex"))

# KPD vote share models, 1932-2 election, varying county size
model_voteshare_kpd_list <- list(model_voteshare_kpd_wb1_fullsample_simple[[3]], model_voteshare_kpd_wb1_matched_simple[[3]], 
                                 model_voteshare_kpd_wb2_fullsample_simple[[3]], model_voteshare_kpd_wb2_matched_simple[[3]], 
                                 model_voteshare_kpd_wb3_fullsample_simple[[3]], model_voteshare_kpd_wb3_matched_simple[[3]], 
                                 model_voteshare_kpd_wb4_fullsample_simple[[3]], model_voteshare_kpd_wb4_matched_simple[[3]])
model_voteshare_kpd_ses <- list(model_voteshare_kpd_wb1_fullsample_simple[[3]]$clusterse[,2], model_voteshare_kpd_wb1_matched_simple[[3]]$clusterse[,2],
                                model_voteshare_kpd_wb2_fullsample_simple[[3]]$clusterse[,2], model_voteshare_kpd_wb2_matched_simple[[3]]$clusterse[,2],
                                model_voteshare_kpd_wb3_fullsample_simple[[3]]$clusterse[,2], model_voteshare_kpd_wb3_matched_simple[[3]]$clusterse[,2],
                                model_voteshare_kpd_wb4_fullsample_simple[[3]]$clusterse[,2], model_voteshare_kpd_wb4_matched_simple[[3]]$clusterse[,2])
obs <- c(length(model_voteshare_kpd_wb1_fullsample_simple[[3]]$residuals)/2,
         length(model_voteshare_kpd_wb1_matched_simple[[3]]$residuals)/2,
         length(model_voteshare_kpd_wb2_fullsample_simple[[3]]$residuals)/2,
         length(model_voteshare_kpd_wb2_matched_simple[[3]]$residuals)/2,
         length(model_voteshare_kpd_wb3_fullsample_simple[[3]]$residuals)/2,
         length(model_voteshare_kpd_wb3_matched_simple[[3]]$residuals)/2,
         length(model_voteshare_kpd_wb4_fullsample_simple[[3]]$residuals)/2,
         length(model_voteshare_kpd_wb4_matched_simple[[3]]$residuals)/2)
varnames_long <- c("\\cmidrule(r){2-3}\\cmidrule(r){4-5}\\cmidrule(r){6-7}\\cmidrule(r){8-9}Exposure, county size of...", "(Intercept)")
election_names <- c("... less than 20k ", "... between 20k and 50k", "... between 50k and 80k", "... more than 80k")
effect_models_tex <- stargazer(model_voteshare_kpd_list, 
                               dep.var.caption = "", #dep.var.caption = "NSDAP/Hitler vote share", 
                               omit.table.layout = "d",
                               covariate.labels = varnames_long, 
                               model.numbers = FALSE, 
                               keep=c("Constant", "^timeXexposure"),
                               omit.stat = c("rsq", "res.dev", "ser", "n", "f"),
                               no.space = TRUE,
                               df = FALSE, 
                               se = model_voteshare_kpd_ses,
                               add.lines = list(c("Sample", rep(c("full", "matched"), 4)),
                                                c("Observations", obs)),
                               notes        = "DID models with number of actual voters as pop weights. Clustered SEs shown. $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01", 
                               notes.append = FALSE,
                               column.labels = election_names, 
                               column.separate = c(2, 2, 2, 2), 
                               font.size = "scriptsize",
                               title = paste0("Diff-in-diff estimates of exposure effects on KPD vote share at the Nov 1932 national parliamentary election with varying county size specifications.\\vspace{-.25cm}"), 
                               label = paste0("tab:nsdap-voteshare-kpd-countysize-dd-1932-2"),
                               out = paste0("../figures/tab-effect-voteshare-kpd-models-countysize-1932-2.tex"))

# turnout models, 1932-2 election, varying county size
model_turnout_list <- list(model_turnout_wb1_fullsample_simple[[3]], model_turnout_wb1_matched_simple[[3]], 
                           model_turnout_wb2_fullsample_simple[[3]], model_turnout_wb2_matched_simple[[3]], 
                           model_turnout_wb3_fullsample_simple[[3]], model_turnout_wb3_matched_simple[[3]], 
                           model_turnout_wb4_fullsample_simple[[3]], model_turnout_wb4_matched_simple[[3]])
model_turnout_ses <- list(model_turnout_wb1_fullsample_simple[[3]]$clusterse[,2], model_turnout_wb1_matched_simple[[3]]$clusterse[,2],
                          model_turnout_wb2_fullsample_simple[[3]]$clusterse[,2], model_turnout_wb2_matched_simple[[3]]$clusterse[,2],
                          model_turnout_wb3_fullsample_simple[[3]]$clusterse[,2], model_turnout_wb3_matched_simple[[3]]$clusterse[,2],
                          model_turnout_wb4_fullsample_simple[[3]]$clusterse[,2], model_turnout_wb4_matched_simple[[3]]$clusterse[,2])
obs <- c(length(model_turnout_wb1_fullsample_simple[[3]]$residuals)/2,
         length(model_turnout_wb1_matched_simple[[3]]$residuals)/2,
         length(model_turnout_wb2_fullsample_simple[[3]]$residuals)/2,
         length(model_turnout_wb2_matched_simple[[3]]$residuals)/2,
         length(model_turnout_wb3_fullsample_simple[[3]]$residuals)/2,
         length(model_turnout_wb3_matched_simple[[3]]$residuals)/2,
         length(model_turnout_wb4_fullsample_simple[[3]]$residuals)/2,
         length(model_turnout_wb4_matched_simple[[3]]$residuals)/2)
varnames_long <- c("\\cmidrule(r){2-3}\\cmidrule(r){4-5}\\cmidrule(r){6-7}\\cmidrule(r){8-9}Exposure, county size of...", "(Intercept)")
election_names <- c("... less than 20k ", "... between 20k and 50k", "... between 50k and 80k", "... more than 80k")
effect_models_tex <- stargazer(model_turnout_list, 
                               dep.var.caption = "", #dep.var.caption = "NSDAP/Hitler vote share", 
                               omit.table.layout = "d",
                               covariate.labels = varnames_long, 
                               model.numbers = FALSE, 
                               keep=c("Constant", "^timeXexposure"),
                               omit.stat = c("rsq", "res.dev", "ser", "n", "f"),
                               no.space = TRUE,
                               df = FALSE, 
                               se = model_turnout_ses,
                               add.lines = list(c("Sample", rep(c("full", "matched"), 4)),
                                                c("Observations", obs)),
                               notes        = "DID models with number of actual voters as pop weights. Clustered SEs shown. $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01", 
                               notes.append = FALSE,
                               column.labels = election_names, 
                               column.separate = c(2, 2, 2, 2), 
                               font.size = "scriptsize",
                               title = paste0("Diff-in-diff estimates of exposure effects on turnout at the Nov 1932 national parliamentary election with varying county size specifications.\\vspace{-.25cm}"), 
                               label = paste0("tab:turnout-countysize-dd-1932-2"),
                               out = paste0("../figures/tab-effect-turnout-models-countysize-1932-2.tex"))


# NSDAP vote share models, 1933 election, varying county size
model_voteshare_list <- list(model_voteshare_wb1_fullsample_simple[[4]], model_voteshare_wb1_matched_simple[[4]], 
                             model_voteshare_wb2_fullsample_simple[[4]], model_voteshare_wb2_matched_simple[[4]], 
                             model_voteshare_wb3_fullsample_simple[[4]], model_voteshare_wb3_matched_simple[[4]], 
                             model_voteshare_wb4_fullsample_simple[[4]], model_voteshare_wb4_matched_simple[[4]])
model_voteshare_ses <- list(model_voteshare_wb1_fullsample_simple[[4]]$clusterse[,2], model_voteshare_wb1_matched_simple[[4]]$clusterse[,2],
                            model_voteshare_wb2_fullsample_simple[[4]]$clusterse[,2], model_voteshare_wb2_matched_simple[[4]]$clusterse[,2],
                            model_voteshare_wb3_fullsample_simple[[4]]$clusterse[,2], model_voteshare_wb3_matched_simple[[4]]$clusterse[,2],
                            model_voteshare_wb4_fullsample_simple[[4]]$clusterse[,2], model_voteshare_wb4_matched_simple[[4]]$clusterse[,2])
obs <- c(length(model_voteshare_wb1_fullsample_simple[[4]]$residuals)/2,
         length(model_voteshare_wb1_matched_simple[[4]]$residuals)/2,
         length(model_voteshare_wb2_fullsample_simple[[4]]$residuals)/2,
         length(model_voteshare_wb2_matched_simple[[4]]$residuals)/2,
         length(model_voteshare_wb3_fullsample_simple[[4]]$residuals)/2,
         length(model_voteshare_wb3_matched_simple[[4]]$residuals)/2,
         length(model_voteshare_wb4_fullsample_simple[[4]]$residuals)/2,
         length(model_voteshare_wb4_matched_simple[[4]]$residuals)/2)
varnames_long <- c("\\cmidrule(r){2-3}\\cmidrule(r){4-5}\\cmidrule(r){6-7}\\cmidrule(r){8-9}Exposure, county size of...", "(Intercept)")
election_names <- c("... less than 20k ", "... between 20k and 50k", "... between 50k and 80k", "... more than 80k")
effect_models_tex <- stargazer(model_voteshare_list, 
                               dep.var.caption = "", #dep.var.caption = "NSDAP/Hitler vote share", 
                               omit.table.layout = "d",
                               covariate.labels = varnames_long, 
                               model.numbers = FALSE, 
                               keep=c("Constant", "^timeXexposure"),
                               omit.stat = c("rsq", "res.dev", "ser", "n", "f"),
                               no.space = TRUE,
                               df = FALSE, 
                               se = model_voteshare_ses,
                               add.lines = list(c("Sample", rep(c("full", "matched"), 4)),
                                                c("Observations", obs)),
                               notes        = "DID models with number of actual voters as pop weights. Clustered SEs shown. $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01", 
                               notes.append = FALSE,
                               column.labels = election_names, 
                               column.separate = c(2, 2, 2, 2), 
                               font.size = "scriptsize",
                               title = paste0("Diff-in-diff estimates of exposure effects on NSDAP vote share at the 1933 national parliamentary election with varying county size specifications.\\vspace{-.25cm}"), 
                               label = paste0("tab:nsdap-voteshare-countysize-dd-1933"),
                               out = paste0("../figures/tab-effect-voteshare-models-countysize-1933.tex"))

# KPD vote share models, 1933 election, varying county size
model_voteshare_kpd_list <- list(model_voteshare_kpd_wb1_fullsample_simple[[4]], model_voteshare_kpd_wb1_matched_simple[[4]], 
                                 model_voteshare_kpd_wb2_fullsample_simple[[4]], model_voteshare_kpd_wb2_matched_simple[[4]], 
                                 model_voteshare_kpd_wb3_fullsample_simple[[4]], model_voteshare_kpd_wb3_matched_simple[[4]], 
                                 model_voteshare_kpd_wb4_fullsample_simple[[4]], model_voteshare_kpd_wb4_matched_simple[[4]])
model_voteshare_kpd_ses <- list(model_voteshare_kpd_wb1_fullsample_simple[[4]]$clusterse[,2], model_voteshare_kpd_wb1_matched_simple[[4]]$clusterse[,2],
                                model_voteshare_kpd_wb2_fullsample_simple[[4]]$clusterse[,2], model_voteshare_kpd_wb2_matched_simple[[4]]$clusterse[,2],
                                model_voteshare_kpd_wb3_fullsample_simple[[4]]$clusterse[,2], model_voteshare_kpd_wb3_matched_simple[[4]]$clusterse[,2],
                                model_voteshare_kpd_wb4_fullsample_simple[[4]]$clusterse[,2], model_voteshare_kpd_wb4_matched_simple[[4]]$clusterse[,2])
obs <- c(length(model_voteshare_kpd_wb1_fullsample_simple[[4]]$residuals)/2,
         length(model_voteshare_kpd_wb1_matched_simple[[4]]$residuals)/2,
         length(model_voteshare_kpd_wb2_fullsample_simple[[4]]$residuals)/2,
         length(model_voteshare_kpd_wb2_matched_simple[[4]]$residuals)/2,
         length(model_voteshare_kpd_wb3_fullsample_simple[[4]]$residuals)/2,
         length(model_voteshare_kpd_wb3_matched_simple[[4]]$residuals)/2,
         length(model_voteshare_kpd_wb4_fullsample_simple[[4]]$residuals)/2,
         length(model_voteshare_kpd_wb4_matched_simple[[4]]$residuals)/2)
varnames_long <- c("\\cmidrule(r){2-3}\\cmidrule(r){4-5}\\cmidrule(r){6-7}\\cmidrule(r){8-9}Exposure, county size of...", "(Intercept)")
election_names <- c("... less than 20k ", "... between 20k and 50k", "... between 50k and 80k", "... more than 80k")
effect_models_tex <- stargazer(model_voteshare_kpd_list, 
                               dep.var.caption = "", #dep.var.caption = "NSDAP/Hitler vote share", 
                               omit.table.layout = "d",
                               covariate.labels = varnames_long, 
                               model.numbers = FALSE, 
                               keep=c("Constant", "^timeXexposure"),
                               omit.stat = c("rsq", "res.dev", "ser", "n", "f"),
                               no.space = TRUE,
                               df = FALSE, 
                               se = model_voteshare_kpd_ses,
                               add.lines = list(c("Sample", rep(c("full", "matched"), 4)),
                                                c("Observations", obs)),
                               notes        = "DID models with number of actual voters as pop weights. Clustered SEs shown. $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01", 
                               notes.append = FALSE,
                               column.labels = election_names, 
                               column.separate = c(2, 2, 2, 2), 
                               font.size = "scriptsize",
                               title = paste0("Diff-in-diff estimates of exposure effects on KPD vote share at the 1933 national parliamentary election with varying county size specifications.\\vspace{-.25cm}"), 
                               label = paste0("tab:nsdap-voteshare-kpd-countysize-dd-1933"),
                               out = paste0("../figures/tab-effect-voteshare-kpd-models-countysize-1933.tex"))

# turnout models, 1933 election, varying county size
model_turnout_list <- list(model_turnout_wb1_fullsample_simple[[4]], model_turnout_wb1_matched_simple[[4]], 
                           model_turnout_wb2_fullsample_simple[[4]], model_turnout_wb2_matched_simple[[4]], 
                           model_turnout_wb3_fullsample_simple[[4]], model_turnout_wb3_matched_simple[[4]], 
                           model_turnout_wb4_fullsample_simple[[4]], model_turnout_wb4_matched_simple[[4]])
model_turnout_ses <- list(model_turnout_wb1_fullsample_simple[[4]]$clusterse[,2], model_turnout_wb1_matched_simple[[4]]$clusterse[,2],
                          model_turnout_wb2_fullsample_simple[[4]]$clusterse[,2], model_turnout_wb2_matched_simple[[4]]$clusterse[,2],
                          model_turnout_wb3_fullsample_simple[[4]]$clusterse[,2], model_turnout_wb3_matched_simple[[4]]$clusterse[,2],
                          model_turnout_wb4_fullsample_simple[[4]]$clusterse[,2], model_turnout_wb4_matched_simple[[4]]$clusterse[,2])
obs <- c(length(model_turnout_wb1_fullsample_simple[[4]]$residuals)/2,
         length(model_turnout_wb1_matched_simple[[4]]$residuals)/2,
         length(model_turnout_wb2_fullsample_simple[[4]]$residuals)/2,
         length(model_turnout_wb2_matched_simple[[4]]$residuals)/2,
         length(model_turnout_wb3_fullsample_simple[[4]]$residuals)/2,
         length(model_turnout_wb3_matched_simple[[4]]$residuals)/2,
         length(model_turnout_wb4_fullsample_simple[[4]]$residuals)/2,
         length(model_turnout_wb4_matched_simple[[4]]$residuals)/2)
varnames_long <- c("\\cmidrule(r){2-3}\\cmidrule(r){4-5}\\cmidrule(r){6-7}\\cmidrule(r){8-9}Exposure, county size of...", "(Intercept)")
election_names <- c("... less than 20k ", "... between 20k and 50k", "... between 50k and 80k", "... more than 80k")
effect_models_tex <- stargazer(model_turnout_list, 
                               dep.var.caption = "", #dep.var.caption = "NSDAP/Hitler vote share", 
                               omit.table.layout = "d",
                               covariate.labels = varnames_long, 
                               model.numbers = FALSE, 
                               keep=c("Constant", "^timeXexposure"),
                               omit.stat = c("rsq", "res.dev", "ser", "n", "f"),
                               no.space = TRUE,
                               df = FALSE, 
                               se = model_turnout_ses,
                               add.lines = list(c("Sample", rep(c("full", "matched"), 4)),
                                                c("Observations", obs)),
                               notes        = "DID models with number of actual voters as pop weights. Clustered SEs shown. $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01", 
                               notes.append = FALSE,
                               column.labels = election_names, 
                               column.separate = c(2, 2, 2, 2), 
                               font.size = "scriptsize",
                               title = paste0("Diff-in-diff estimates of exposure effects on turnout at the 1933 national parliamentary election with varying county size specifications.\\vspace{-.25cm}"), 
                               label = paste0("tab:turnout-countysize-dd-1933"),
                               out = paste0("../figures/tab-effect-turnout-models-countysize-1933.tex"))



## plot ----
pdf(file="../figures/effects_coefplot_countysize.pdf", height=3.5, width=8, family="Times")
par(oma=c(.5,0,.5,0))
par(mar=c(3,7,3,1))
layout(matrix(1:3, 1, byrow = TRUE), widths=c(1.3, 1, 1), heights=c(2, 2, 2))
ylabel_names <- rev(c("Sep 1930", "Jul 1932", "Nov 1932", "Mar 1933"))
colors <- brewer.pal(9,"Greys")
colors <- c(colors[4], colors[5], colors[7], colors[9])
## voteshare effect plot
plot(0, 0, xlim = c(-0.07, .07), ylim = c(-.3, 3.3), pch = 20, col = "white", xlab = "", ylab = "", yaxt = "n", xaxt = "n")
axis(2, at = 0:3, labels = F, tick = F)
text(y = 0:3, par("usr")[1], labels = ylabel_names, srt = 0, pos = 2, xpd = TRUE)
axis(1, at = seq(-.06, .06, .03), labels = seq(-.06, .06, .03), tick = T)
mtext("NSDAP vote share", side = 3, line = 1.2, outer = FALSE)
abline(v = 0, lty = 2)
abline(h = c(.5, 1.5, 2.5, 3.5), lty = 1, col = "darkgrey")
# Sep 1930
y_par <- 3
mod_input <- model_voteshare_wb1_matched_simple[[1]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.25, x1 = mod$coef_hardhi, y1 = y_par+.25, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.25, x1 = mod$coef_weakhi, y1 = y_par+.25, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.25, pch = 20, col = colors[1])
mod_input <- model_voteshare_wb2_matched_simple[[1]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.083, x1 = mod$coef_hardhi, y1 = y_par+.083, length = 0, col = colors[2])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.083, x1 = mod$coef_weakhi, y1 = y_par+.083, length = 0, lwd = 2, col = colors[2])
points(mod$coef_point, y_par+.083, pch = 20, col = colors[2])
mod_input <- model_voteshare_wb3_matched_simple[[1]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.083, x1 = mod$coef_hardhi, y1 = y_par-.083, length = 0, col = colors[3])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.083, x1 = mod$coef_weakhi, y1 = y_par-.083, length = 0, lwd = 2, col = colors[3])
points(mod$coef_point, y_par-.083, pch = 20, col = colors[3])
mod_input <- model_voteshare_wb4_matched_simple[[1]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.25, x1 = mod$coef_hardhi, y1 = y_par-.25, length = 0, col = colors[4])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.25, x1 = mod$coef_weakhi, y1 = y_par-.25, length = 0, lwd = 2, col = colors[4])
points(mod$coef_point, y_par-.25, pch = 20, col = colors[4])
# Jul 1932
y_par <- 2
mod_input <- model_voteshare_wb1_matched_simple[[2]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.25, x1 = mod$coef_hardhi, y1 = y_par+.25, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.25, x1 = mod$coef_weakhi, y1 = y_par+.25, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.25, pch = 20, col = colors[1])
mod_input <- model_voteshare_wb2_matched_simple[[2]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.083, x1 = mod$coef_hardhi, y1 = y_par+.083, length = 0, col = colors[2])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.083, x1 = mod$coef_weakhi, y1 = y_par+.083, length = 0, lwd = 2, col = colors[2])
points(mod$coef_point, y_par+.083, pch = 20, col = colors[2])
mod_input <- model_voteshare_wb3_matched_simple[[2]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.083, x1 = mod$coef_hardhi, y1 = y_par-.083, length = 0, col = colors[3])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.083, x1 = mod$coef_weakhi, y1 = y_par-.083, length = 0, lwd = 2, col = colors[3])
points(mod$coef_point, y_par-.083, pch = 20, col = colors[3])
mod_input <- model_voteshare_wb4_matched_simple[[2]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.25, x1 = mod$coef_hardhi, y1 = y_par-.25, length = 0, col = colors[4])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.25, x1 = mod$coef_weakhi, y1 = y_par-.25, length = 0, lwd = 2, col = colors[4])
points(mod$coef_point, y_par-.25, pch = 20, col = colors[4])
# Nov 1932
y_par <- 1
mod_input <- model_voteshare_wb1_matched_simple[[3]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.25, x1 = mod$coef_hardhi, y1 = y_par+.25, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.25, x1 = mod$coef_weakhi, y1 = y_par+.25, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.25, pch = 20, col = colors[1])
mod_input <- model_voteshare_wb2_matched_simple[[3]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.083, x1 = mod$coef_hardhi, y1 = y_par+.083, length = 0, col = colors[2])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.083, x1 = mod$coef_weakhi, y1 = y_par+.083, length = 0, lwd = 2, col = colors[2])
points(mod$coef_point, y_par+.083, pch = 20, col = colors[2])
mod_input <- model_voteshare_wb3_matched_simple[[3]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.083, x1 = mod$coef_hardhi, y1 = y_par-.083, length = 0, col = colors[3])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.083, x1 = mod$coef_weakhi, y1 = y_par-.083, length = 0, lwd = 2, col = colors[3])
points(mod$coef_point, y_par-.083, pch = 20, col = colors[3])
mod_input <- model_voteshare_wb4_matched_simple[[3]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.25, x1 = mod$coef_hardhi, y1 = y_par-.25, length = 0, col = colors[4])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.25, x1 = mod$coef_weakhi, y1 = y_par-.25, length = 0, lwd = 2, col = colors[4])
points(mod$coef_point, y_par-.25, pch = 20, col = colors[4])
# Mar 1933
y_par <- 0
mod_input <- model_voteshare_wb1_matched_simple[[4]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.25, x1 = mod$coef_hardhi, y1 = y_par+.25, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.25, x1 = mod$coef_weakhi, y1 = y_par+.25, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.25, pch = 20, col = colors[1])
mod_input <- model_voteshare_wb2_matched_simple[[4]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.083, x1 = mod$coef_hardhi, y1 = y_par+.083, length = 0, col = colors[2])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.083, x1 = mod$coef_weakhi, y1 = y_par+.083, length = 0, lwd = 2, col = colors[2])
points(mod$coef_point, y_par+.083, pch = 20, col = colors[2])
mod_input <- model_voteshare_wb3_matched_simple[[4]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.083, x1 = mod$coef_hardhi, y1 = y_par-.083, length = 0, col = colors[3])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.083, x1 = mod$coef_weakhi, y1 = y_par-.083, length = 0, lwd = 2, col = colors[3])
points(mod$coef_point, y_par-.083, pch = 20, col = colors[3])
mod_input <- model_voteshare_wb4_matched_simple[[4]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.25, x1 = mod$coef_hardhi, y1 = y_par-.25, length = 0, col = colors[4])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.25, x1 = mod$coef_weakhi, y1 = y_par-.25, length = 0, lwd = 2, col = colors[4])
points(mod$coef_point, y_par-.25, pch = 20, col = colors[4])

## kpd voteshare effect plot
par(mar=c(3,0,3,1))
plot(0, 0, xlim = c(-0.07, .07), ylim = c(-.3, 3.3), pch = 20, col = "white", xlab = "", ylab = "", yaxt = "n", xaxt = "n")
axis(1, at = seq(-.06, .06, .03), labels = seq(-.06, .06, .03), tick = T)
mtext("KPD vote share", side = 3, line = 1.2, outer = FALSE)
abline(v = 0, lty = 2)
abline(h = c(.5, 1.5, 2.5), lty = 1, col = "darkgrey")
# Sep 1930
y_par <- 3
mod_input <- model_voteshare_kpd_wb1_matched_simple[[1]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.25, x1 = mod$coef_hardhi, y1 = y_par+.25, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.25, x1 = mod$coef_weakhi, y1 = y_par+.25, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.25, pch = 20, col = colors[1])
mod_input <- model_voteshare_kpd_wb2_matched_simple[[1]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.083, x1 = mod$coef_hardhi, y1 = y_par+.083, length = 0, col = colors[2])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.083, x1 = mod$coef_weakhi, y1 = y_par+.083, length = 0, lwd = 2, col = colors[2])
points(mod$coef_point, y_par+.083, pch = 20, col = colors[2])
mod_input <- model_voteshare_kpd_wb3_matched_simple[[1]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.083, x1 = mod$coef_hardhi, y1 = y_par-.083, length = 0, col = colors[3])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.083, x1 = mod$coef_weakhi, y1 = y_par-.083, length = 0, lwd = 2, col = colors[3])
points(mod$coef_point, y_par-.083, pch = 20, col = colors[3])
mod_input <- model_voteshare_kpd_wb4_matched_simple[[1]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.25, x1 = mod$coef_hardhi, y1 = y_par-.25, length = 0, col = colors[4])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.25, x1 = mod$coef_weakhi, y1 = y_par-.25, length = 0, lwd = 2, col = colors[4])
points(mod$coef_point, y_par-.25, pch = 20, col = colors[4])
# Jul 1932
y_par <- 2
mod_input <- model_voteshare_kpd_wb1_matched_simple[[2]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.25, x1 = mod$coef_hardhi, y1 = y_par+.25, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.25, x1 = mod$coef_weakhi, y1 = y_par+.25, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.25, pch = 20, col = colors[1])
mod_input <- model_voteshare_kpd_wb2_matched_simple[[2]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.083, x1 = mod$coef_hardhi, y1 = y_par+.083, length = 0, col = colors[2])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.083, x1 = mod$coef_weakhi, y1 = y_par+.083, length = 0, lwd = 2, col = colors[2])
points(mod$coef_point, y_par+.083, pch = 20, col = colors[2])
mod_input <- model_voteshare_kpd_wb3_matched_simple[[2]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.083, x1 = mod$coef_hardhi, y1 = y_par-.083, length = 0, col = colors[3])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.083, x1 = mod$coef_weakhi, y1 = y_par-.083, length = 0, lwd = 2, col = colors[3])
points(mod$coef_point, y_par-.083, pch = 20, col = colors[3])
mod_input <- model_voteshare_kpd_wb4_matched_simple[[2]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.25, x1 = mod$coef_hardhi, y1 = y_par-.25, length = 0, col = colors[4])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.25, x1 = mod$coef_weakhi, y1 = y_par-.25, length = 0, lwd = 2, col = colors[4])
points(mod$coef_point, y_par-.25, pch = 20, col = colors[4])
# Nov 1932
y_par <- 1
mod_input <- model_voteshare_kpd_wb1_matched_simple[[3]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.25, x1 = mod$coef_hardhi, y1 = y_par+.25, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.25, x1 = mod$coef_weakhi, y1 = y_par+.25, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.25, pch = 20, col = colors[1])
mod_input <- model_voteshare_kpd_wb2_matched_simple[[3]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.083, x1 = mod$coef_hardhi, y1 = y_par+.083, length = 0, col = colors[2])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.083, x1 = mod$coef_weakhi, y1 = y_par+.083, length = 0, lwd = 2, col = colors[2])
points(mod$coef_point, y_par+.083, pch = 20, col = colors[2])
mod_input <- model_voteshare_kpd_wb3_matched_simple[[3]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.083, x1 = mod$coef_hardhi, y1 = y_par-.083, length = 0, col = colors[3])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.083, x1 = mod$coef_weakhi, y1 = y_par-.083, length = 0, lwd = 2, col = colors[3])
points(mod$coef_point, y_par-.083, pch = 20, col = colors[3])
mod_input <- model_voteshare_kpd_wb4_matched_simple[[3]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.25, x1 = mod$coef_hardhi, y1 = y_par-.25, length = 0, col = colors[4])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.25, x1 = mod$coef_weakhi, y1 = y_par-.25, length = 0, lwd = 2, col = colors[4])
points(mod$coef_point, y_par-.25, pch = 20, col = colors[4])
# Mar 1933
y_par <- 0
mod_input <- model_voteshare_kpd_wb1_matched_simple[[4]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.25, x1 = mod$coef_hardhi, y1 = y_par+.25, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.25, x1 = mod$coef_weakhi, y1 = y_par+.25, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.25, pch = 20, col = colors[1])
mod_input <- model_voteshare_kpd_wb2_matched_simple[[4]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.083, x1 = mod$coef_hardhi, y1 = y_par+.083, length = 0, col = colors[2])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.083, x1 = mod$coef_weakhi, y1 = y_par+.083, length = 0, lwd = 2, col = colors[2])
points(mod$coef_point, y_par+.083, pch = 20, col = colors[2])
mod_input <- model_voteshare_kpd_wb3_matched_simple[[4]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.083, x1 = mod$coef_hardhi, y1 = y_par-.083, length = 0, col = colors[3])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.083, x1 = mod$coef_weakhi, y1 = y_par-.083, length = 0, lwd = 2, col = colors[3])
points(mod$coef_point, y_par-.083, pch = 20, col = colors[3])
mod_input <- model_voteshare_kpd_wb4_matched_simple[[4]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.25, x1 = mod$coef_hardhi, y1 = y_par-.25, length = 0, col = colors[4])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.25, x1 = mod$coef_weakhi, y1 = y_par-.25, length = 0, lwd = 2, col = colors[4])
points(mod$coef_point, y_par-.25, pch = 20, col = colors[4])
text(.07, y_par+.25, labels = expression(N[elig] < 20), col = colors[1], pos = 2)
text(.07, y_par+.083, labels = expression({20 <= N[elig]} < 50), col = colors[2], pos = 2)
text(.07, y_par-.083, labels = expression({50 <= N[elig]} < 80), col = colors[3], pos = 2)
text(.07, y_par-.25, labels = expression(N[elig] >= 80), col = colors[4], pos = 2)

## turnout effect plot
par(mar=c(3,0,3,1))
plot(0, 0, xlim = c(-0.07, .07), ylim = c(-.3, 3.3), pch = 20, col = "white", xlab = "", ylab = "", yaxt = "n", xaxt = "n")
axis(1, at = seq(-.06, .06, .03), labels = seq(-.06, .06, .03), tick = T)
mtext("Turnout", side = 3, line = 1.2, outer = FALSE)
abline(v = 0, lty = 2)
abline(h = c(.5, 1.5, 2.5), lty = 1, col = "darkgrey")
# Sep 1930
y_par <- 3
mod_input <- model_turnout_wb1_matched_simple[[1]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.25, x1 = mod$coef_hardhi, y1 = y_par+.25, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.25, x1 = mod$coef_weakhi, y1 = y_par+.25, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.25, pch = 20, col = colors[1])
mod_input <- model_turnout_wb2_matched_simple[[1]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.083, x1 = mod$coef_hardhi, y1 = y_par+.083, length = 0, col = colors[2])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.083, x1 = mod$coef_weakhi, y1 = y_par+.083, length = 0, lwd = 2, col = colors[2])
points(mod$coef_point, y_par+.083, pch = 20, col = colors[2])
mod_input <- model_turnout_wb3_matched_simple[[1]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.083, x1 = mod$coef_hardhi, y1 = y_par-.083, length = 0, col = colors[3])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.083, x1 = mod$coef_weakhi, y1 = y_par-.083, length = 0, lwd = 2, col = colors[3])
points(mod$coef_point, y_par-.083, pch = 20, col = colors[3])
mod_input <- model_turnout_wb4_matched_simple[[1]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.25, x1 = mod$coef_hardhi, y1 = y_par-.25, length = 0, col = colors[4])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.25, x1 = mod$coef_weakhi, y1 = y_par-.25, length = 0, lwd = 2, col = colors[4])
points(mod$coef_point, y_par-.25, pch = 20, col = colors[4])
# Jul 1932
y_par <- 2
mod_input <- model_turnout_wb1_matched_simple[[2]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.25, x1 = mod$coef_hardhi, y1 = y_par+.25, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.25, x1 = mod$coef_weakhi, y1 = y_par+.25, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.25, pch = 20, col = colors[1])
mod_input <- model_turnout_wb2_matched_simple[[2]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.083, x1 = mod$coef_hardhi, y1 = y_par+.083, length = 0, col = colors[2])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.083, x1 = mod$coef_weakhi, y1 = y_par+.083, length = 0, lwd = 2, col = colors[2])
points(mod$coef_point, y_par+.083, pch = 20, col = colors[2])
mod_input <- model_turnout_wb3_matched_simple[[2]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.083, x1 = mod$coef_hardhi, y1 = y_par-.083, length = 0, col = colors[3])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.083, x1 = mod$coef_weakhi, y1 = y_par-.083, length = 0, lwd = 2, col = colors[3])
points(mod$coef_point, y_par-.083, pch = 20, col = colors[3])
mod_input <- model_turnout_wb4_matched_simple[[2]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.25, x1 = mod$coef_hardhi, y1 = y_par-.25, length = 0, col = colors[4])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.25, x1 = mod$coef_weakhi, y1 = y_par-.25, length = 0, lwd = 2, col = colors[4])
points(mod$coef_point, y_par-.25, pch = 20, col = colors[4])
# Nov 1932
y_par <- 1
mod_input <- model_turnout_wb1_matched_simple[[3]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.25, x1 = mod$coef_hardhi, y1 = y_par+.25, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.25, x1 = mod$coef_weakhi, y1 = y_par+.25, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.25, pch = 20, col = colors[1])
mod_input <- model_turnout_wb2_matched_simple[[3]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.083, x1 = mod$coef_hardhi, y1 = y_par+.083, length = 0, col = colors[2])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.083, x1 = mod$coef_weakhi, y1 = y_par+.083, length = 0, lwd = 2, col = colors[2])
points(mod$coef_point, y_par+.083, pch = 20, col = colors[2])
mod_input <- model_turnout_wb3_matched_simple[[3]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.083, x1 = mod$coef_hardhi, y1 = y_par-.083, length = 0, col = colors[3])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.083, x1 = mod$coef_weakhi, y1 = y_par-.083, length = 0, lwd = 2, col = colors[3])
points(mod$coef_point, y_par-.083, pch = 20, col = colors[3])
mod_input <- model_turnout_wb4_matched_simple[[3]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.25, x1 = mod$coef_hardhi, y1 = y_par-.25, length = 0, col = colors[4])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.25, x1 = mod$coef_weakhi, y1 = y_par-.25, length = 0, lwd = 2, col = colors[4])
points(mod$coef_point, y_par-.25, pch = 20, col = colors[4])
# Mar 1933
y_par <- 0
mod_input <- model_turnout_wb1_matched_simple[[4]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.25, x1 = mod$coef_hardhi, y1 = y_par+.25, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.25, x1 = mod$coef_weakhi, y1 = y_par+.25, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.25, pch = 20, col = colors[1])
mod_input <- model_turnout_wb2_matched_simple[[4]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.083, x1 = mod$coef_hardhi, y1 = y_par+.083, length = 0, col = colors[2])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.083, x1 = mod$coef_weakhi, y1 = y_par+.083, length = 0, lwd = 2, col = colors[2])
points(mod$coef_point, y_par+.083, pch = 20, col = colors[2])
mod_input <- model_turnout_wb3_matched_simple[[4]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.083, x1 = mod$coef_hardhi, y1 = y_par-.083, length = 0, col = colors[3])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.083, x1 = mod$coef_weakhi, y1 = y_par-.083, length = 0, lwd = 2, col = colors[3])
points(mod$coef_point, y_par-.083, pch = 20, col = colors[3])
mod_input <- model_turnout_wb4_matched_simple[[4]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.25, x1 = mod$coef_hardhi, y1 = y_par-.25, length = 0, col = colors[4])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.25, x1 = mod$coef_weakhi, y1 = y_par-.25, length = 0, lwd = 2, col = colors[4])
points(mod$coef_point, y_par-.25, pch = 20, col = colors[4])
dev.off()



