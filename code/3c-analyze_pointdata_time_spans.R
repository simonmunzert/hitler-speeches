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
c()


## load packages and functions -------------------
source("packages.r")
source("functions.r")
source("functions-analysis.r")


## import prepared data --------------------------
load("county_df_pointdata_long.RData")
load("county_pres_df_pointdata_long.RData")
load("community_df_pointdata_long.RData")




## time span of effects -----------------------------

# model NSDAP vote share
model_voteshare_12w_formula_simple <- paste("p_nsdap ~ time + exposure + timeXexposure",sep = "") %>% as.formula
model_voteshare_12w_fullsample_simple <- list()
model_voteshare_12w_matched_simple <- list()
for (i in 1:4) {
  # full sample, lm simple model with voting population weights (i.e., only number of voters is considered)
  dat_full <- getDatFull("county_df_long", election = i+1, treatment = "visit_12w_10km")
  model_voteshare_12w_fullsample_simple[[i]] <- lm(model_voteshare_12w_formula_simple, data =  dat_full, weights = dat_full$gs)
  model_voteshare_12w_fullsample_simple[[i]]$clusterse <- runClusterRobustOLS(model_voteshare_12w_fullsample_simple[[i]], dat_full$lfnr)
  # 1:1 matching, lm simple model with voting population weights
  dat_raw <- county_df_long[county_df_long$election == i+1,]
  dat_raw$no_hidden_exposure <- dat_raw$visit_12w_10km == 1 | dat_raw$visit_10km == 0
  match.data.list <- list()
  match.data.list[[i]] <- performMatch("visit_12w_10km", dat.raw = "dat_raw", electiontype = "county", exactmatch = "no_hidden_exposure") # exactmatch parameter ensures that observations which were exposed to an appearance within the pre-election period but off the defined time span are excluded from matching
  dat_match <- match.data.list[[i]]
  dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "county_df_long", election = i+1, treatment = "visit_12w_10km")
  model_voteshare_12w_matched_simple[[i]] <- lm(model_voteshare_12w_formula_simple, data =  dat_match, weights = dat_match$gs)
  model_voteshare_12w_matched_simple[[i]]$clusterse <- runClusterRobustOLS(model_voteshare_12w_matched_simple[[i]], dat_match$lfnr)
}

model_voteshare_8w_formula_simple <- paste("p_nsdap ~ time + exposure + timeXexposure",sep = "") %>% as.formula
model_voteshare_8w_fullsample_simple <- list()
model_voteshare_8w_matched_simple <- list()
for (i in 1:4) {
  # full sample, lm simple model with voting population weights (i.e., only number of voters is considered)
  dat_full <- getDatFull("county_df_long", election = i+1, treatment = "visit_8w_10km")
  model_voteshare_8w_fullsample_simple[[i]] <- lm(model_voteshare_8w_formula_simple, data =  dat_full, weights = dat_full$gs)
  model_voteshare_8w_fullsample_simple[[i]]$clusterse <- runClusterRobustOLS(model_voteshare_8w_fullsample_simple[[i]], dat_full$lfnr)
  # 1:1 matching, lm simple model with voting population weights
  dat_raw <- county_df_long[county_df_long$election == i+1,]
  dat_raw$no_hidden_exposure <- dat_raw$visit_8w_10km == 1 | dat_raw$visit_10km == 0
  match.data.list <- list()
  match.data.list[[i]] <- performMatch("visit_8w_10km", dat.raw = "dat_raw", electiontype = "county", exactmatch = "no_hidden_exposure")
  dat_match <- match.data.list[[i]]
  dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "county_df_long", election = i+1, treatment = "visit_8w_10km")
  
  model_voteshare_8w_matched_simple[[i]] <- lm(model_voteshare_8w_formula_simple, data =  dat_match, weights = dat_match$gs)
  model_voteshare_8w_matched_simple[[i]]$clusterse <- runClusterRobustOLS(model_voteshare_8w_matched_simple[[i]], dat_match$lfnr)
}

model_voteshare_4w_formula_simple <- paste("p_nsdap ~ time + exposure + timeXexposure",sep = "") %>% as.formula
model_voteshare_4w_fullsample_simple <- list()
model_voteshare_4w_matched_simple <- list()
for (i in 1:4) {
  # full sample, lm simple model with voting population weights (i.e., only number of voters is considered)
  dat_full <- getDatFull("county_df_long", election = i+1, treatment = "visit_4w_10km")
  model_voteshare_4w_fullsample_simple[[i]] <- lm(model_voteshare_4w_formula_simple, data =  dat_full, weights = dat_full$gs)
  model_voteshare_4w_fullsample_simple[[i]]$clusterse <- runClusterRobustOLS(model_voteshare_4w_fullsample_simple[[i]], dat_full$lfnr)
  # 1:1 matching, lm simple model with voting population weights
  dat_raw <- county_df_long[county_df_long$election == i+1,]
  dat_raw$no_hidden_exposure <- dat_raw$visit_4w_10km == 1 | dat_raw$visit_10km == 0
  match.data.list <- list()
  match.data.list[[i]] <- performMatch("visit_4w_10km", dat.raw = "dat_raw", electiontype = "county", exactmatch = "no_hidden_exposure")
  dat_match <- match.data.list[[i]]
  dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "county_df_long", election = i+1, treatment = "visit_4w_10km")
  
  model_voteshare_4w_matched_simple[[i]] <- lm(model_voteshare_4w_formula_simple, data =  dat_match, weights = dat_match$gs)
  model_voteshare_4w_matched_simple[[i]]$clusterse <- runClusterRobustOLS(model_voteshare_4w_matched_simple[[i]], dat_match$lfnr)
}

model_voteshare_2w_formula_simple <- paste("p_nsdap ~ time + exposure + timeXexposure",sep = "") %>% as.formula
model_voteshare_2w_fullsample_simple <- list()
model_voteshare_2w_matched_simple <- list()
for (i in 1:4) {
  # full sample, lm simple model with voting population weights (i.e., only number of voters is considered)
  dat_full <- getDatFull("county_df_long", election = i+1, treatment = "visit_2w_10km")
  model_voteshare_2w_fullsample_simple[[i]] <- lm(model_voteshare_2w_formula_simple, data =  dat_full, weights = dat_full$gs)
  model_voteshare_2w_fullsample_simple[[i]]$clusterse <- runClusterRobustOLS(model_voteshare_2w_fullsample_simple[[i]], dat_full$lfnr)
  # 1:1 matching, lm simple model with voting population weights
  dat_raw <- county_df_long[county_df_long$election == i+1,]
  dat_raw$no_hidden_exposure <- dat_raw$visit_2w_10km == 1 | dat_raw$visit_10km == 0
  match.data.list <- list()
  match.data.list[[i]] <- performMatch("visit_2w_10km", dat.raw = "dat_raw", electiontype = "county", exactmatch = "no_hidden_exposure")
  dat_match <- match.data.list[[i]]
  dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "county_df_long", election = i+1, treatment = "visit_2w_10km")
  
  model_voteshare_2w_matched_simple[[i]] <- lm(model_voteshare_2w_formula_simple, data =  dat_match, weights = dat_match$gs)
  model_voteshare_2w_matched_simple[[i]]$clusterse <- runClusterRobustOLS(model_voteshare_2w_matched_simple[[i]], dat_match$lfnr)
}

# model KPD vote share
model_voteshare_kpd_12w_formula_simple <- paste("p_kpd ~ time + exposure + timeXexposure",sep = "") %>% as.formula
model_voteshare_kpd_12w_fullsample_simple <- list()
model_voteshare_kpd_12w_matched_simple <- list()
for (i in 1:4) {
  # full sample, lm simple model with voting population weights (i.e., only number of voters is considered)
  dat_full <- getDatFull("county_df_long", election = i+1, treatment = "visit_12w_10km")
  model_voteshare_kpd_12w_fullsample_simple[[i]] <- lm(model_voteshare_kpd_12w_formula_simple, data =  dat_full, weights = dat_full$gs)
  model_voteshare_kpd_12w_fullsample_simple[[i]]$clusterse <- runClusterRobustOLS(model_voteshare_kpd_12w_fullsample_simple[[i]], dat_full$lfnr)
  # 1:1 matching, lm simple model with voting population weights
  dat_raw <- county_df_long[county_df_long$election == i+1,]
  dat_raw$no_hidden_exposure <- dat_raw$visit_12w_10km == 1 | dat_raw$visit_10km == 0
  match.data.list <- list()
  match.data.list[[i]] <- performMatch("visit_12w_10km", dat.raw = "dat_raw", electiontype = "county", exactmatch = "no_hidden_exposure")
  dat_match <- match.data.list[[i]]
  dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "county_df_long", election = i+1, treatment = "visit_12w_10km")
  
  model_voteshare_kpd_12w_matched_simple[[i]] <- lm(model_voteshare_kpd_12w_formula_simple, data =  dat_match, weights = dat_match$gs)
  model_voteshare_kpd_12w_matched_simple[[i]]$clusterse <- runClusterRobustOLS(model_voteshare_kpd_12w_matched_simple[[i]], dat_match$lfnr)
}

model_voteshare_kpd_8w_formula_simple <- paste("p_kpd ~ time + exposure + timeXexposure",sep = "") %>% as.formula
model_voteshare_kpd_8w_fullsample_simple <- list()
model_voteshare_kpd_8w_matched_simple <- list()
for (i in 1:4) {
  # full sample, lm simple model with voting population weights (i.e., only number of voters is considered)
  dat_full <- getDatFull("county_df_long", election = i+1, treatment = "visit_8w_10km")
  model_voteshare_kpd_8w_fullsample_simple[[i]] <- lm(model_voteshare_kpd_8w_formula_simple, data =  dat_full, weights = dat_full$gs)
  model_voteshare_kpd_8w_fullsample_simple[[i]]$clusterse <- runClusterRobustOLS(model_voteshare_kpd_8w_fullsample_simple[[i]], dat_full$lfnr)
  # 1:1 matching, lm simple model with voting population weights
  dat_raw <- county_df_long[county_df_long$election == i+1,]
  dat_raw$no_hidden_exposure <- dat_raw$visit_8w_10km == 1 | dat_raw$visit_10km == 0
  match.data.list <- list()
  match.data.list[[i]] <- performMatch("visit_8w_10km", dat.raw = "dat_raw", electiontype = "county", exactmatch = "no_hidden_exposure")
  dat_match <- match.data.list[[i]]
  dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "county_df_long", election = i+1, treatment = "visit_8w_10km")
  
  model_voteshare_kpd_8w_matched_simple[[i]] <- lm(model_voteshare_kpd_8w_formula_simple, data =  dat_match, weights = dat_match$gs)
  model_voteshare_kpd_8w_matched_simple[[i]]$clusterse <- runClusterRobustOLS(model_voteshare_kpd_8w_matched_simple[[i]], dat_match$lfnr)
}

model_voteshare_kpd_4w_formula_simple <- paste("p_kpd ~ time + exposure + timeXexposure",sep = "") %>% as.formula
model_voteshare_kpd_4w_fullsample_simple <- list()
model_voteshare_kpd_4w_matched_simple <- list()
for (i in 1:4) {
  # full sample, lm simple model with voting population weights (i.e., only number of voters is considered)
  dat_full <- getDatFull("county_df_long", election = i+1, treatment = "visit_4w_10km")
  model_voteshare_kpd_4w_fullsample_simple[[i]] <- lm(model_voteshare_kpd_4w_formula_simple, data =  dat_full, weights = dat_full$gs)
  model_voteshare_kpd_4w_fullsample_simple[[i]]$clusterse <- runClusterRobustOLS(model_voteshare_kpd_4w_fullsample_simple[[i]], dat_full$lfnr)
  # 1:1 matching, lm simple model with voting population weights
  dat_raw <- county_df_long[county_df_long$election == i+1,]
  dat_raw$no_hidden_exposure <- dat_raw$visit_4w_10km == 1 | dat_raw$visit_10km == 0
  match.data.list <- list()
  match.data.list[[i]] <- performMatch("visit_4w_10km", dat.raw = "dat_raw", electiontype = "county", exactmatch = "no_hidden_exposure")
  dat_match <- match.data.list[[i]]
  dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "county_df_long", election = i+1, treatment = "visit_4w_10km")
  
  model_voteshare_kpd_4w_matched_simple[[i]] <- lm(model_voteshare_kpd_4w_formula_simple, data =  dat_match, weights = dat_match$gs)
  model_voteshare_kpd_4w_matched_simple[[i]]$clusterse <- runClusterRobustOLS(model_voteshare_kpd_4w_matched_simple[[i]], dat_match$lfnr)
}

model_voteshare_kpd_2w_formula_simple <- paste("p_kpd ~ time + exposure + timeXexposure",sep = "") %>% as.formula
model_voteshare_kpd_2w_fullsample_simple <- list()
model_voteshare_kpd_2w_matched_simple <- list()
for (i in 1:4) {
  # full sample, lm simple model with voting population weights (i.e., only number of voters is considered)
  dat_full <- getDatFull("county_df_long", election = i+1, treatment = "visit_2w_10km")
  model_voteshare_kpd_2w_fullsample_simple[[i]] <- lm(model_voteshare_kpd_2w_formula_simple, data =  dat_full, weights = dat_full$gs)
  model_voteshare_kpd_2w_fullsample_simple[[i]]$clusterse <- runClusterRobustOLS(model_voteshare_kpd_2w_fullsample_simple[[i]], dat_full$lfnr)
  # 1:1 matching, lm simple model with voting population weights
  dat_raw <- county_df_long[county_df_long$election == i+1,]
  dat_raw$no_hidden_exposure <- dat_raw$visit_2w_10km == 1 | dat_raw$visit_10km == 0
  match.data.list <- list()
  match.data.list[[i]] <- performMatch("visit_2w_10km", dat.raw = "dat_raw", electiontype = "county", exactmatch = "no_hidden_exposure")
  dat_match <- match.data.list[[i]]
  dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "county_df_long", election = i+1, treatment = "visit_2w_10km")
  
  model_voteshare_kpd_2w_matched_simple[[i]] <- lm(model_voteshare_kpd_2w_formula_simple, data =  dat_match, weights = dat_match$gs)
  model_voteshare_kpd_2w_matched_simple[[i]]$clusterse <- runClusterRobustOLS(model_voteshare_kpd_2w_matched_simple[[i]], dat_match$lfnr)
}

# model turnout
model_turnout_12w_formula_simple <- paste("p_turnout ~ time + exposure + timeXexposure",sep = "") %>% as.formula
model_turnout_12w_fullsample_simple <- list()
model_turnout_12w_matched_simple <- list()
for (i in 1:4) {
  # full sample, lm simple model with voting population weights (i.e., only number of voters is considered)
  dat_full <- getDatFull("county_df_long", election = i+1, treatment = "visit_12w_10km")
  model_turnout_12w_fullsample_simple[[i]] <- lm(model_turnout_12w_formula_simple, data =  dat_full, weights = dat_full$gs)
  model_turnout_12w_fullsample_simple[[i]]$clusterse <- runClusterRobustOLS(model_turnout_12w_fullsample_simple[[i]], dat_full$lfnr)
  # 1:1 matching, lm simple model with voting population weights
  dat_raw <- county_df_long[county_df_long$election == i+1,]
  dat_raw$no_hidden_exposure <- dat_raw$visit_12w_10km == 1 | dat_raw$visit_10km == 0
  match.data.list <- list()
  match.data.list[[i]] <- performMatch("visit_12w_10km", dat.raw = "dat_raw", electiontype = "county", exactmatch = "no_hidden_exposure")
  dat_match <- match.data.list[[i]]
  dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "county_df_long", election = i+1, treatment = "visit_12w_10km")
  
  model_turnout_12w_matched_simple[[i]] <- lm(model_turnout_12w_formula_simple, data =  dat_match, weights = dat_match$gs)
  model_turnout_12w_matched_simple[[i]]$clusterse <- runClusterRobustOLS(model_turnout_12w_matched_simple[[i]], dat_match$lfnr)
}

model_turnout_8w_formula_simple <- paste("p_turnout ~ time + exposure + timeXexposure",sep = "") %>% as.formula
model_turnout_8w_fullsample_simple <- list()
model_turnout_8w_matched_simple <- list()
for (i in 1:4) {
  # full sample, lm simple model with voting population weights (i.e., only number of voters is considered)
  dat_full <- getDatFull("county_df_long", election = i+1, treatment = "visit_8w_10km")
  model_turnout_8w_fullsample_simple[[i]] <- lm(model_turnout_8w_formula_simple, data =  dat_full, weights = dat_full$gs)
  model_turnout_8w_fullsample_simple[[i]]$clusterse <- runClusterRobustOLS(model_turnout_8w_fullsample_simple[[i]], dat_full$lfnr)
  # 1:1 matching, lm simple model with voting population weights
  dat_raw <- county_df_long[county_df_long$election == i+1,]
  dat_raw$no_hidden_exposure <- dat_raw$visit_8w_10km == 1 | dat_raw$visit_10km == 0
  match.data.list <- list()
  match.data.list[[i]] <- performMatch("visit_8w_10km", dat.raw = "dat_raw", electiontype = "county", exactmatch = "no_hidden_exposure")
  dat_match <- match.data.list[[i]]
  dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "county_df_long", election = i+1, treatment = "visit_8w_10km")
  
  model_turnout_8w_matched_simple[[i]] <- lm(model_turnout_8w_formula_simple, data =  dat_match, weights = dat_match$gs)
  model_turnout_8w_matched_simple[[i]]$clusterse <- runClusterRobustOLS(model_turnout_8w_matched_simple[[i]], dat_match$lfnr)
}

model_turnout_4w_formula_simple <- paste("p_turnout ~ time + exposure + timeXexposure",sep = "") %>% as.formula
model_turnout_4w_fullsample_simple <- list()
model_turnout_4w_matched_simple <- list()
for (i in 1:4) {
  # full sample, lm simple model with voting population weights (i.e., only number of voters is considered)
  dat_full <- getDatFull("county_df_long", election = i+1, treatment = "visit_4w_10km")
  model_turnout_4w_fullsample_simple[[i]] <- lm(model_turnout_4w_formula_simple, data =  dat_full, weights = dat_full$gs)
  model_turnout_4w_fullsample_simple[[i]]$clusterse <- runClusterRobustOLS(model_turnout_4w_fullsample_simple[[i]], dat_full$lfnr)
  # 1:1 matching, lm simple model with voting population weights
  dat_raw <- county_df_long[county_df_long$election == i+1,]
  dat_raw$no_hidden_exposure <- dat_raw$visit_4w_10km == 1 | dat_raw$visit_10km == 0
  match.data.list <- list()
  match.data.list[[i]] <- performMatch("visit_4w_10km", dat.raw = "dat_raw", electiontype = "county", exactmatch = "no_hidden_exposure")
  dat_match <- match.data.list[[i]]
  dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "county_df_long", election = i+1, treatment = "visit_4w_10km")
  
  model_turnout_4w_matched_simple[[i]] <- lm(model_turnout_4w_formula_simple, data =  dat_match, weights = dat_match$gs)
  model_turnout_4w_matched_simple[[i]]$clusterse <- runClusterRobustOLS(model_turnout_4w_matched_simple[[i]], dat_match$lfnr)
}

model_turnout_2w_formula_simple <- paste("p_turnout ~ time + exposure + timeXexposure",sep = "") %>% as.formula
model_turnout_2w_fullsample_simple <- list()
model_turnout_2w_matched_simple <- list()
for (i in 1:4) {
  # full sample, lm simple model with voting population weights (i.e., only number of voters is considered)
  dat_full <- getDatFull("county_df_long", election = i+1, treatment = "visit_2w_10km")
  model_turnout_2w_fullsample_simple[[i]] <- lm(model_turnout_2w_formula_simple, data =  dat_full, weights = dat_full$gs)
  model_turnout_2w_fullsample_simple[[i]]$clusterse <- runClusterRobustOLS(model_turnout_2w_fullsample_simple[[i]], dat_full$lfnr)
  # 1:1 matching, lm simple model with voting population weights
  dat_raw <- county_df_long[county_df_long$election == i+1,]
  dat_raw$no_hidden_exposure <- dat_raw$visit_2w_10km == 1 | dat_raw$visit_10km == 0
  match.data.list <- list()
  match.data.list[[i]] <- performMatch("visit_2w_10km", dat.raw = "dat_raw", electiontype = "county", exactmatch = "no_hidden_exposure")
  dat_match <- match.data.list[[i]]
  dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "county_df_long", election = i+1, treatment = "visit_2w_10km")
  
  model_turnout_2w_matched_simple[[i]] <- lm(model_turnout_2w_formula_simple, data =  dat_match, weights = dat_match$gs)
  model_turnout_2w_matched_simple[[i]]$clusterse <- runClusterRobustOLS(model_turnout_2w_matched_simple[[i]], dat_match$lfnr)
}


# model NSDAP vote share, 1930 election, municipal data
model_voteshare_comm_formula_simple <- paste("p_nsdap ~ time + exposure + timeXexposure",sep = "") %>% as.formula
# 12w
dat_full <- getDatFull("community_df_long", election = 2, treatment = "visit_12w_10km")
model_voteshare_comm_fullsample_simple_12w <- lm(model_voteshare_comm_formula_simple, data =  dat_full, weights = dat_full$gs)
model_voteshare_comm_fullsample_simple_12w$clusterse <- runClusterRobustOLS(model_voteshare_comm_fullsample_simple_12w, dat_full$lfnr)
dat_raw <- community_df_long[community_df_long$election == 2,]
dat_raw$no_hidden_exposure <- dat_raw$visit_12w_10km == 1 | dat_raw$visit_10km == 0
dat_match <- performMatch("visit_12w_10km", dat.raw = "dat_raw", electiontype = "comm", exactmatch = "no_hidden_exposure")
dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "community_df_long", election = 2, treatment = "visit_12w_10km")

model_voteshare_comm_matched_simple_12w <- lm(model_voteshare_comm_formula_simple, data =  dat_match, weights = dat_match$gs)
model_voteshare_comm_matched_simple_12w$clusterse <- runClusterRobustOLS(model_voteshare_comm_matched_simple_12w, dat_match$lfnr)
# 8w
dat_full <- getDatFull("community_df_long", election = 2, treatment = "visit_8w_10km")
model_voteshare_comm_fullsample_simple_8w <- lm(model_voteshare_comm_formula_simple, data =  dat_full, weights = dat_full$gs)
model_voteshare_comm_fullsample_simple_8w$clusterse <- runClusterRobustOLS(model_voteshare_comm_fullsample_simple_8w, dat_full$lfnr)
dat_raw <- community_df_long[community_df_long$election == 2,]
dat_raw$no_hidden_exposure <- dat_raw$visit_8w_10km == 1 | dat_raw$visit_10km == 0
dat_match <- performMatch("visit_8w_10km", dat.raw = "dat_raw", electiontype = "comm", exactmatch = "no_hidden_exposure")
dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "community_df_long", election = 2, treatment = "visit_8w_10km")

model_voteshare_comm_matched_simple_8w <- lm(model_voteshare_comm_formula_simple, data =  dat_match, weights = dat_match$gs)
model_voteshare_comm_matched_simple_8w$clusterse <- runClusterRobustOLS(model_voteshare_comm_matched_simple_8w, dat_match$lfnr)
# 4w
dat_full <- getDatFull("community_df_long", election = 2, treatment = "visit_4w_10km")
model_voteshare_comm_fullsample_simple_4w <- lm(model_voteshare_comm_formula_simple, data =  dat_full, weights = dat_full$gs)
model_voteshare_comm_fullsample_simple_4w$clusterse <- runClusterRobustOLS(model_voteshare_comm_fullsample_simple_4w, dat_full$lfnr)
dat_raw <- community_df_long[community_df_long$election == 2,]
dat_raw$no_hidden_exposure <- dat_raw$visit_4w_10km == 1 | dat_raw$visit_10km == 0
dat_match <- performMatch("visit_4w_10km", dat.raw = "dat_raw", electiontype = "comm", exactmatch = "no_hidden_exposure")
dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "community_df_long", election = 2, treatment = "visit_4w_10km")

model_voteshare_comm_matched_simple_4w <- lm(model_voteshare_comm_formula_simple, data =  dat_match, weights = dat_match$gs)
model_voteshare_comm_matched_simple_4w$clusterse <- runClusterRobustOLS(model_voteshare_comm_matched_simple_4w, dat_match$lfnr)
# 2w
dat_full <- getDatFull("community_df_long", election = 2, treatment = "visit_2w_10km")
model_voteshare_comm_fullsample_simple_2w <- lm(model_voteshare_comm_formula_simple, data =  dat_full, weights = dat_full$gs)
model_voteshare_comm_fullsample_simple_2w$clusterse <- runClusterRobustOLS(model_voteshare_comm_fullsample_simple_2w, dat_full$lfnr)
dat_raw <- community_df_long[community_df_long$election == 2,]
dat_raw$no_hidden_exposure <- dat_raw$visit_2w_10km == 1 | dat_raw$visit_10km == 0
dat_match <- performMatch("visit_2w_10km", dat.raw = "dat_raw", electiontype = "comm", exactmatch = "no_hidden_exposure")
dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "community_df_long", election = 2, treatment = "visit_2w_10km")

model_voteshare_comm_matched_simple_2w <- lm(model_voteshare_comm_formula_simple, data =  dat_match, weights = dat_match$gs)
model_voteshare_comm_matched_simple_2w$clusterse <- runClusterRobustOLS(model_voteshare_comm_matched_simple_2w, dat_match$lfnr)


# model KPD vote share, 1930 election, municipal data
model_voteshare_kpd_comm_formula_simple <- paste("p_kpd ~ time + exposure + timeXexposure",sep = "") %>% as.formula
# 12w
dat_full <- getDatFull("community_df_long", election = 2, treatment = "visit_12w_10km")
model_voteshare_kpd_comm_fullsample_simple_12w <- lm(model_voteshare_kpd_comm_formula_simple, data =  dat_full, weights = dat_full$gs)
model_voteshare_kpd_comm_fullsample_simple_12w$clusterse <- runClusterRobustOLS(model_voteshare_kpd_comm_fullsample_simple_12w, dat_full$lfnr)
dat_raw <- community_df_long[community_df_long$election == 2,]
dat_raw$no_hidden_exposure <- dat_raw$visit_12w_10km == 1 | dat_raw$visit_10km == 0
dat_match <- performMatch("visit_12w_10km", dat.raw = "dat_raw", electiontype = "comm", exactmatch = "no_hidden_exposure")
dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "community_df_long", election = 2, treatment = "visit_12w_10km")

model_voteshare_kpd_comm_matched_simple_12w <- lm(model_voteshare_kpd_comm_formula_simple, data =  dat_match, weights = dat_match$gs)
model_voteshare_kpd_comm_matched_simple_12w$clusterse <- runClusterRobustOLS(model_voteshare_kpd_comm_matched_simple_12w, dat_match$lfnr)
# 8w
dat_full <- getDatFull("community_df_long", election = 2, treatment = "visit_8w_10km")
model_voteshare_kpd_comm_fullsample_simple_8w <- lm(model_voteshare_kpd_comm_formula_simple, data =  dat_full, weights = dat_full$gs)
model_voteshare_kpd_comm_fullsample_simple_8w$clusterse <- runClusterRobustOLS(model_voteshare_kpd_comm_fullsample_simple_8w, dat_full$lfnr)
dat_raw <- community_df_long[community_df_long$election == 2,]
dat_raw$no_hidden_exposure <- dat_raw$visit_8w_10km == 1 | dat_raw$visit_10km == 0
dat_match <- performMatch("visit_8w_10km", dat.raw = "dat_raw", electiontype = "comm", exactmatch = "no_hidden_exposure")
dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "community_df_long", election = 2, treatment = "visit_8w_10km")

model_voteshare_kpd_comm_matched_simple_8w <- lm(model_voteshare_kpd_comm_formula_simple, data =  dat_match, weights = dat_match$gs)
model_voteshare_kpd_comm_matched_simple_8w$clusterse <- runClusterRobustOLS(model_voteshare_kpd_comm_matched_simple_8w, dat_match$lfnr)
# 4w
dat_full <- getDatFull("community_df_long", election = 2, treatment = "visit_4w_10km")
model_voteshare_kpd_comm_fullsample_simple_4w <- lm(model_voteshare_kpd_comm_formula_simple, data =  dat_full, weights = dat_full$gs)
model_voteshare_kpd_comm_fullsample_simple_4w$clusterse <- runClusterRobustOLS(model_voteshare_kpd_comm_fullsample_simple_4w, dat_full$lfnr)
dat_raw <- community_df_long[community_df_long$election == 2,]
dat_raw$no_hidden_exposure <- dat_raw$visit_4w_10km == 1 | dat_raw$visit_10km == 0
dat_match <- performMatch("visit_4w_10km", dat.raw = "dat_raw", electiontype = "comm", exactmatch = "no_hidden_exposure")
dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "community_df_long", election = 2, treatment = "visit_4w_10km")

model_voteshare_kpd_comm_matched_simple_4w <- lm(model_voteshare_kpd_comm_formula_simple, data =  dat_match, weights = dat_match$gs)
model_voteshare_kpd_comm_matched_simple_4w$clusterse <- runClusterRobustOLS(model_voteshare_kpd_comm_matched_simple_4w, dat_match$lfnr)
# 2w
dat_full <- getDatFull("community_df_long", election = 2, treatment = "visit_2w_10km")
model_voteshare_kpd_comm_fullsample_simple_2w <- lm(model_voteshare_kpd_comm_formula_simple, data =  dat_full, weights = dat_full$gs)
model_voteshare_kpd_comm_fullsample_simple_2w$clusterse <- runClusterRobustOLS(model_voteshare_kpd_comm_fullsample_simple_2w, dat_full$lfnr)
dat_raw <- community_df_long[community_df_long$election == 2,]
dat_raw$no_hidden_exposure <- dat_raw$visit_2w_10km == 1 | dat_raw$visit_10km == 0
dat_match <- performMatch("visit_2w_10km", dat.raw = "dat_raw", electiontype = "comm", exactmatch = "no_hidden_exposure")
dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "community_df_long", election = 2, treatment = "visit_2w_10km")

model_voteshare_kpd_comm_matched_simple_2w <- lm(model_voteshare_kpd_comm_formula_simple, data =  dat_match, weights = dat_match$gs)
model_voteshare_kpd_comm_matched_simple_2w$clusterse <- runClusterRobustOLS(model_voteshare_kpd_comm_matched_simple_2w, dat_match$lfnr)

# model turnout, 1930 election, municipal data
model_turnout_comm_formula_simple <- paste("p_turnout ~ time + exposure + timeXexposure",sep = "") %>% as.formula
# 12w
dat_full <- getDatFull("community_df_long", election = 2, treatment = "visit_12w_10km")
model_turnout_comm_fullsample_simple_12w <- lm(model_turnout_comm_formula_simple, data =  dat_full, weights = dat_full$gs)
model_turnout_comm_fullsample_simple_12w$clusterse <- runClusterRobustOLS(model_turnout_comm_fullsample_simple_12w, dat_full$lfnr)
dat_raw <- community_df_long[community_df_long$election == 2,]
dat_raw$no_hidden_exposure <- dat_raw$visit_12w_10km == 1 | dat_raw$visit_10km == 0
dat_match <- performMatch("visit_12w_10km", dat.raw = "dat_raw", electiontype = "comm", exactmatch = "no_hidden_exposure")
dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "community_df_long", election = 2, treatment = "visit_12w_10km")

model_turnout_comm_matched_simple_12w <- lm(model_turnout_comm_formula_simple, data =  dat_match, weights = dat_match$gs)
model_turnout_comm_matched_simple_12w$clusterse <- runClusterRobustOLS(model_turnout_comm_matched_simple_12w, dat_match$lfnr)
# 8w
dat_full <- getDatFull("community_df_long", election = 2, treatment = "visit_8w_10km")
model_turnout_comm_fullsample_simple_8w <- lm(model_turnout_comm_formula_simple, data =  dat_full, weights = dat_full$gs)
model_turnout_comm_fullsample_simple_8w$clusterse <- runClusterRobustOLS(model_turnout_comm_fullsample_simple_8w, dat_full$lfnr)
dat_raw <- community_df_long[community_df_long$election == 2,]
dat_raw$no_hidden_exposure <- dat_raw$visit_8w_10km == 1 | dat_raw$visit_10km == 0
dat_match <- performMatch("visit_8w_10km", dat.raw = "dat_raw", electiontype = "comm", exactmatch = "no_hidden_exposure")
dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "community_df_long", election = 2, treatment = "visit_8w_10km")

model_turnout_comm_matched_simple_8w <- lm(model_turnout_comm_formula_simple, data =  dat_match, weights = dat_match$gs)
model_turnout_comm_matched_simple_8w$clusterse <- runClusterRobustOLS(model_turnout_comm_matched_simple_8w, dat_match$lfnr)
# 4w
dat_full <- getDatFull("community_df_long", election = 2, treatment = "visit_4w_10km")
model_turnout_comm_fullsample_simple_4w <- lm(model_turnout_comm_formula_simple, data =  dat_full, weights = dat_full$gs)
model_turnout_comm_fullsample_simple_4w$clusterse <- runClusterRobustOLS(model_turnout_comm_fullsample_simple_4w, dat_full$lfnr)
dat_raw <- community_df_long[community_df_long$election == 2,]
dat_raw$no_hidden_exposure <- dat_raw$visit_4w_10km == 1 | dat_raw$visit_10km == 0
dat_match <- performMatch("visit_4w_10km", dat.raw = "dat_raw", electiontype = "comm", exactmatch = "no_hidden_exposure")
dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "community_df_long", election = 2, treatment = "visit_4w_10km")

model_turnout_comm_matched_simple_4w <- lm(model_turnout_comm_formula_simple, data =  dat_match, weights = dat_match$gs)
model_turnout_comm_matched_simple_4w$clusterse <- runClusterRobustOLS(model_turnout_comm_matched_simple_4w, dat_match$lfnr)
# 2w
dat_full <- getDatFull("community_df_long", election = 2, treatment = "visit_2w_10km")
model_turnout_comm_fullsample_simple_2w <- lm(model_turnout_comm_formula_simple, data =  dat_full, weights = dat_full$gs)
model_turnout_comm_fullsample_simple_2w$clusterse <- runClusterRobustOLS(model_turnout_comm_fullsample_simple_2w, dat_full$lfnr)
dat_raw <- community_df_long[community_df_long$election == 2,]
dat_raw$no_hidden_exposure <- dat_raw$visit_2w_10km == 1 | dat_raw$visit_10km == 0
dat_match <- performMatch("visit_2w_10km", dat.raw = "dat_raw", electiontype = "comm", exactmatch = "no_hidden_exposure")
dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "community_df_long", election = 2, treatment = "visit_2w_10km")

model_turnout_comm_matched_simple_2w <- lm(model_turnout_comm_formula_simple, data =  dat_match, weights = dat_match$gs)
model_turnout_comm_matched_simple_2w$clusterse <- runClusterRobustOLS(model_turnout_comm_matched_simple_2w, dat_match$lfnr)



## tables -----

# NSDAP vote share models, 1930 election, varying time spans
model_voteshare_list <- list(model_voteshare_12w_fullsample_simple[[1]], model_voteshare_12w_matched_simple[[1]], 
                             model_voteshare_8w_fullsample_simple[[1]], model_voteshare_8w_matched_simple[[1]], 
                             model_voteshare_4w_fullsample_simple[[1]], model_voteshare_4w_matched_simple[[1]], 
                             model_voteshare_2w_fullsample_simple[[1]], model_voteshare_2w_matched_simple[[1]])
model_voteshare_ses <- list(model_voteshare_12w_fullsample_simple[[1]]$clusterse[,2], model_voteshare_12w_matched_simple[[1]]$clusterse[,2],
                            model_voteshare_8w_fullsample_simple[[1]]$clusterse[,2], model_voteshare_8w_matched_simple[[1]]$clusterse[,2],
                            model_voteshare_4w_fullsample_simple[[1]]$clusterse[,2], model_voteshare_4w_matched_simple[[1]]$clusterse[,2],
                            model_voteshare_2w_fullsample_simple[[1]]$clusterse[,2], model_voteshare_2w_matched_simple[[1]]$clusterse[,2])
obs <- c(length(model_voteshare_12w_fullsample_simple[[1]]$residuals)/2,
         length(model_voteshare_12w_matched_simple[[1]]$residuals)/2,
         length(model_voteshare_8w_fullsample_simple[[1]]$residuals)/2,
         length(model_voteshare_8w_matched_simple[[1]]$residuals)/2,
         length(model_voteshare_4w_fullsample_simple[[1]]$residuals)/2,
         length(model_voteshare_4w_matched_simple[[1]]$residuals)/2,
         length(model_voteshare_2w_fullsample_simple[[1]]$residuals)/2,
         length(model_voteshare_2w_matched_simple[[1]]$residuals)/2)
varnames_long <- c("\\cmidrule(r){2-3}\\cmidrule(r){4-5}\\cmidrule(r){6-7}\\cmidrule(r){8-9}Exposure, time span of...", "(Intercept)")
election_names <- c("... 12 weeks", "... 8 weeks", "... 4 weeks", "... 2 weeks")
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
                               title = paste0("Diff-in-diff estimates of exposure effects on NSDAP vote share at the 1930 national parliamentary election with varying time span specifications.\\vspace{-.25cm}"), 
                               label = paste0("tab:nsdap-voteshare-timespan-dd-1930"),
                               out = paste0("../figures/tab-effect-voteshare-models-timespan-1930.tex"))

# KPD vote share models, 1930 election, varying time spans
model_voteshare_kpd_list <- list(model_voteshare_kpd_12w_fullsample_simple[[1]], model_voteshare_kpd_12w_matched_simple[[1]], 
                             model_voteshare_kpd_8w_fullsample_simple[[1]], model_voteshare_kpd_8w_matched_simple[[1]], 
                             model_voteshare_kpd_4w_fullsample_simple[[1]], model_voteshare_kpd_4w_matched_simple[[1]], 
                             model_voteshare_kpd_2w_fullsample_simple[[1]], model_voteshare_kpd_2w_matched_simple[[1]])
model_voteshare_kpd_ses <- list(model_voteshare_kpd_12w_fullsample_simple[[1]]$clusterse[,2], model_voteshare_kpd_12w_matched_simple[[1]]$clusterse[,2],
                            model_voteshare_kpd_8w_fullsample_simple[[1]]$clusterse[,2], model_voteshare_kpd_8w_matched_simple[[1]]$clusterse[,2],
                            model_voteshare_kpd_4w_fullsample_simple[[1]]$clusterse[,2], model_voteshare_kpd_4w_matched_simple[[1]]$clusterse[,2],
                            model_voteshare_kpd_2w_fullsample_simple[[1]]$clusterse[,2], model_voteshare_kpd_2w_matched_simple[[1]]$clusterse[,2])
obs <- c(length(model_voteshare_kpd_12w_fullsample_simple[[1]]$residuals)/2,
         length(model_voteshare_kpd_12w_matched_simple[[1]]$residuals)/2,
         length(model_voteshare_kpd_8w_fullsample_simple[[1]]$residuals)/2,
         length(model_voteshare_kpd_8w_matched_simple[[1]]$residuals)/2,
         length(model_voteshare_kpd_4w_fullsample_simple[[1]]$residuals)/2,
         length(model_voteshare_kpd_4w_matched_simple[[1]]$residuals)/2,
         length(model_voteshare_kpd_2w_fullsample_simple[[1]]$residuals)/2,
         length(model_voteshare_kpd_2w_matched_simple[[1]]$residuals)/2)
varnames_long <- c("\\cmidrule(r){2-3}\\cmidrule(r){4-5}\\cmidrule(r){6-7}\\cmidrule(r){8-9}Exposure, time span of...", "(Intercept)")
election_names <- c("... 12 weeks", "... 8 weeks", "... 4 weeks", "... 2 weeks")
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
                               title = paste0("Diff-in-diff estimates of exposure effects on KPD vote share at the 1930 national parliamentary election with varying time span specifications.\\vspace{-.25cm}"), 
                               label = paste0("tab:nsdap-voteshare-kpd-timespan-dd-1930"),
                               out = paste0("../figures/tab-effect-voteshare-kpd-models-timespan-1930.tex"))

# turnout models, 1930 election, varying time spans
model_turnout_list <- list(model_turnout_12w_fullsample_simple[[1]], model_turnout_12w_matched_simple[[1]], 
                             model_turnout_8w_fullsample_simple[[1]], model_turnout_8w_matched_simple[[1]], 
                             model_turnout_4w_fullsample_simple[[1]], model_turnout_4w_matched_simple[[1]], 
                             model_turnout_2w_fullsample_simple[[1]], model_turnout_2w_matched_simple[[1]])
model_turnout_ses <- list(model_turnout_12w_fullsample_simple[[1]]$clusterse[,2], model_turnout_12w_matched_simple[[1]]$clusterse[,2],
                            model_turnout_8w_fullsample_simple[[1]]$clusterse[,2], model_turnout_8w_matched_simple[[1]]$clusterse[,2],
                            model_turnout_4w_fullsample_simple[[1]]$clusterse[,2], model_turnout_4w_matched_simple[[1]]$clusterse[,2],
                            model_turnout_2w_fullsample_simple[[1]]$clusterse[,2], model_turnout_2w_matched_simple[[1]]$clusterse[,2])
obs <- c(length(model_turnout_12w_fullsample_simple[[1]]$residuals)/2,
         length(model_turnout_12w_matched_simple[[1]]$residuals)/2,
         length(model_turnout_8w_fullsample_simple[[1]]$residuals)/2,
         length(model_turnout_8w_matched_simple[[1]]$residuals)/2,
         length(model_turnout_4w_fullsample_simple[[1]]$residuals)/2,
         length(model_turnout_4w_matched_simple[[1]]$residuals)/2,
         length(model_turnout_2w_fullsample_simple[[1]]$residuals)/2,
         length(model_turnout_2w_matched_simple[[1]]$residuals)/2)
varnames_long <- c("\\cmidrule(r){2-3}\\cmidrule(r){4-5}\\cmidrule(r){6-7}\\cmidrule(r){8-9}Exposure, time span of...", "(Intercept)")
election_names <- c("... 12 weeks", "... 8 weeks", "... 4 weeks", "... 2 weeks")
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
                               title = paste0("Diff-in-diff estimates of exposure effects on turnout at the 1930 national parliamentary election with varying time span specifications.\\vspace{-.25cm}"), 
                               label = paste0("tab:nsdap-turnout-timespan-dd-1930"),
                               out = paste0("../figures/tab-effect-turnout-models-timespan-1930.tex"))

# NSDAP vote share models, 1932-1 election, varying time spans
model_voteshare_list <- list(model_voteshare_12w_fullsample_simple[[2]], model_voteshare_12w_matched_simple[[2]], 
                             model_voteshare_8w_fullsample_simple[[2]], model_voteshare_8w_matched_simple[[2]], 
                             model_voteshare_4w_fullsample_simple[[2]], model_voteshare_4w_matched_simple[[2]], 
                             model_voteshare_2w_fullsample_simple[[2]], model_voteshare_2w_matched_simple[[2]])
model_voteshare_ses <- list(model_voteshare_12w_fullsample_simple[[2]]$clusterse[,2], model_voteshare_12w_matched_simple[[2]]$clusterse[,2],
                            model_voteshare_8w_fullsample_simple[[2]]$clusterse[,2], model_voteshare_8w_matched_simple[[2]]$clusterse[,2],
                            model_voteshare_4w_fullsample_simple[[2]]$clusterse[,2], model_voteshare_4w_matched_simple[[2]]$clusterse[,2],
                            model_voteshare_2w_fullsample_simple[[2]]$clusterse[,2], model_voteshare_2w_matched_simple[[2]]$clusterse[,2])
obs <- c(length(model_voteshare_12w_fullsample_simple[[2]]$residuals)/2,
         length(model_voteshare_12w_matched_simple[[2]]$residuals)/2,
         length(model_voteshare_8w_fullsample_simple[[2]]$residuals)/2,
         length(model_voteshare_8w_matched_simple[[2]]$residuals)/2,
         length(model_voteshare_4w_fullsample_simple[[2]]$residuals)/2,
         length(model_voteshare_4w_matched_simple[[2]]$residuals)/2,
         length(model_voteshare_2w_fullsample_simple[[2]]$residuals)/2,
         length(model_voteshare_2w_matched_simple[[2]]$residuals)/2)
varnames_long <- c("\\cmidrule(r){2-3}\\cmidrule(r){4-5}\\cmidrule(r){6-7}\\cmidrule(r){8-9}Exposure, time span of...", "(Intercept)")
election_names <- c("... 12 weeks", "... 8 weeks", "... 4 weeks", "... 2 weeks")
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
                               title = paste0("Diff-in-diff estimates of exposure effects on NSDAP vote share at the Jul 1932 national parliamentary election with varying time span specifications.\\vspace{-.25cm}"), 
                               label = paste0("tab:nsdap-voteshare-timespan-dd-1932-1"),
                               out = paste0("../figures/tab-effect-voteshare-models-timespan-1932-1.tex"))

# KPD vote share models, 1932-1 election, varying time spans
model_voteshare_kpd_list <- list(model_voteshare_kpd_12w_fullsample_simple[[2]], model_voteshare_kpd_12w_matched_simple[[2]], 
                                 model_voteshare_kpd_8w_fullsample_simple[[2]], model_voteshare_kpd_8w_matched_simple[[2]], 
                                 model_voteshare_kpd_4w_fullsample_simple[[2]], model_voteshare_kpd_4w_matched_simple[[2]], 
                                 model_voteshare_kpd_2w_fullsample_simple[[2]], model_voteshare_kpd_2w_matched_simple[[2]])
model_voteshare_kpd_ses <- list(model_voteshare_kpd_12w_fullsample_simple[[2]]$clusterse[,2], model_voteshare_kpd_12w_matched_simple[[2]]$clusterse[,2],
                                model_voteshare_kpd_8w_fullsample_simple[[2]]$clusterse[,2], model_voteshare_kpd_8w_matched_simple[[2]]$clusterse[,2],
                                model_voteshare_kpd_4w_fullsample_simple[[2]]$clusterse[,2], model_voteshare_kpd_4w_matched_simple[[2]]$clusterse[,2],
                                model_voteshare_kpd_2w_fullsample_simple[[2]]$clusterse[,2], model_voteshare_kpd_2w_matched_simple[[2]]$clusterse[,2])
obs <- c(length(model_voteshare_kpd_12w_fullsample_simple[[2]]$residuals)/2,
         length(model_voteshare_kpd_12w_matched_simple[[2]]$residuals)/2,
         length(model_voteshare_kpd_8w_fullsample_simple[[2]]$residuals)/2,
         length(model_voteshare_kpd_8w_matched_simple[[2]]$residuals)/2,
         length(model_voteshare_kpd_4w_fullsample_simple[[2]]$residuals)/2,
         length(model_voteshare_kpd_4w_matched_simple[[2]]$residuals)/2,
         length(model_voteshare_kpd_2w_fullsample_simple[[2]]$residuals)/2,
         length(model_voteshare_kpd_2w_matched_simple[[2]]$residuals)/2)
varnames_long <- c("\\cmidrule(r){2-3}\\cmidrule(r){4-5}\\cmidrule(r){6-7}\\cmidrule(r){8-9}Exposure, time span of...", "(Intercept)")
election_names <- c("... 12 weeks", "... 8 weeks", "... 4 weeks", "... 2 weeks")
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
                               title = paste0("Diff-in-diff estimates of exposure effects on KPD vote share at the Jul 1932 national parliamentary election with varying time span specifications.\\vspace{-.25cm}"), 
                               label = paste0("tab:nsdap-voteshare-kpd-timespan-dd-1932-1"),
                               out = paste0("../figures/tab-effect-voteshare-kpd-models-timespan-1932-1.tex"))

# turnout models, 1932-1 election, varying time spans
model_turnout_list <- list(model_turnout_12w_fullsample_simple[[2]], model_turnout_12w_matched_simple[[2]], 
                           model_turnout_8w_fullsample_simple[[2]], model_turnout_8w_matched_simple[[2]], 
                           model_turnout_4w_fullsample_simple[[2]], model_turnout_4w_matched_simple[[2]], 
                           model_turnout_2w_fullsample_simple[[2]], model_turnout_2w_matched_simple[[2]])
model_turnout_ses <- list(model_turnout_12w_fullsample_simple[[2]]$clusterse[,2], model_turnout_12w_matched_simple[[2]]$clusterse[,2],
                          model_turnout_8w_fullsample_simple[[2]]$clusterse[,2], model_turnout_8w_matched_simple[[2]]$clusterse[,2],
                          model_turnout_4w_fullsample_simple[[2]]$clusterse[,2], model_turnout_4w_matched_simple[[2]]$clusterse[,2],
                          model_turnout_2w_fullsample_simple[[2]]$clusterse[,2], model_turnout_2w_matched_simple[[2]]$clusterse[,2])
obs <- c(length(model_turnout_12w_fullsample_simple[[2]]$residuals)/2,
         length(model_turnout_12w_matched_simple[[2]]$residuals)/2,
         length(model_turnout_8w_fullsample_simple[[2]]$residuals)/2,
         length(model_turnout_8w_matched_simple[[2]]$residuals)/2,
         length(model_turnout_4w_fullsample_simple[[2]]$residuals)/2,
         length(model_turnout_4w_matched_simple[[2]]$residuals)/2,
         length(model_turnout_2w_fullsample_simple[[2]]$residuals)/2,
         length(model_turnout_2w_matched_simple[[2]]$residuals)/2)
varnames_long <- c("\\cmidrule(r){2-3}\\cmidrule(r){4-5}\\cmidrule(r){6-7}\\cmidrule(r){8-9}Exposure, time span of...", "(Intercept)")
election_names <- c("... 12 weeks", "... 8 weeks", "... 4 weeks", "... 2 weeks")
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
                               title = paste0("Diff-in-diff estimates of exposure effects on turnout at the Jul 1932 national parliamentary election with varying time span specifications.\\vspace{-.25cm}"), 
                               label = paste0("tab:turnout-timespan-dd-1932-1"),
                               out = paste0("../figures/tab-effect-turnout-models-timespan-1932-1.tex"))


# NSDAP vote share models, 1932-2 election, varying time spans
model_voteshare_list <- list(model_voteshare_12w_fullsample_simple[[3]], model_voteshare_12w_matched_simple[[3]], 
                             model_voteshare_8w_fullsample_simple[[3]], model_voteshare_8w_matched_simple[[3]], 
                             model_voteshare_4w_fullsample_simple[[3]], model_voteshare_4w_matched_simple[[3]], 
                             model_voteshare_2w_fullsample_simple[[3]], model_voteshare_2w_matched_simple[[3]])
model_voteshare_ses <- list(model_voteshare_12w_fullsample_simple[[3]]$clusterse[,2], model_voteshare_12w_matched_simple[[3]]$clusterse[,2],
                            model_voteshare_8w_fullsample_simple[[3]]$clusterse[,2], model_voteshare_8w_matched_simple[[3]]$clusterse[,2],
                            model_voteshare_4w_fullsample_simple[[3]]$clusterse[,2], model_voteshare_4w_matched_simple[[3]]$clusterse[,2],
                            model_voteshare_2w_fullsample_simple[[3]]$clusterse[,2], model_voteshare_2w_matched_simple[[3]]$clusterse[,2])
obs <- c(length(model_voteshare_12w_fullsample_simple[[3]]$residuals)/2,
         length(model_voteshare_12w_matched_simple[[3]]$residuals)/2,
         length(model_voteshare_8w_fullsample_simple[[3]]$residuals)/2,
         length(model_voteshare_8w_matched_simple[[3]]$residuals)/2,
         length(model_voteshare_4w_fullsample_simple[[3]]$residuals)/2,
         length(model_voteshare_4w_matched_simple[[3]]$residuals)/2,
         length(model_voteshare_2w_fullsample_simple[[3]]$residuals)/2,
         length(model_voteshare_2w_matched_simple[[3]]$residuals)/2)
varnames_long <- c("\\cmidrule(r){2-3}\\cmidrule(r){4-5}\\cmidrule(r){6-7}\\cmidrule(r){8-9}Exposure, time span of...", "(Intercept)")
election_names <- c("... 12 weeks", "... 8 weeks", "... 4 weeks", "... 2 weeks")
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
                               title = paste0("Diff-in-diff estimates of exposure effects on NSDAP vote share at the Nov 1932 national parliamentary election with varying time span specifications.\\vspace{-.25cm}"), 
                               label = paste0("tab:nsdap-voteshare-timespan-dd-1932-2"),
                               out = paste0("../figures/tab-effect-voteshare-models-timespan-1932-2.tex"))

# KPD vote share models, 1932-2 election, varying time spans
model_voteshare_kpd_list <- list(model_voteshare_kpd_12w_fullsample_simple[[3]], model_voteshare_kpd_12w_matched_simple[[3]], 
                                 model_voteshare_kpd_8w_fullsample_simple[[3]], model_voteshare_kpd_8w_matched_simple[[3]], 
                                 model_voteshare_kpd_4w_fullsample_simple[[3]], model_voteshare_kpd_4w_matched_simple[[3]], 
                                 model_voteshare_kpd_2w_fullsample_simple[[3]], model_voteshare_kpd_2w_matched_simple[[3]])
model_voteshare_kpd_ses <- list(model_voteshare_kpd_12w_fullsample_simple[[3]]$clusterse[,2], model_voteshare_kpd_12w_matched_simple[[3]]$clusterse[,2],
                                model_voteshare_kpd_8w_fullsample_simple[[3]]$clusterse[,2], model_voteshare_kpd_8w_matched_simple[[3]]$clusterse[,2],
                                model_voteshare_kpd_4w_fullsample_simple[[3]]$clusterse[,2], model_voteshare_kpd_4w_matched_simple[[3]]$clusterse[,2],
                                model_voteshare_kpd_2w_fullsample_simple[[3]]$clusterse[,2], model_voteshare_kpd_2w_matched_simple[[3]]$clusterse[,2])
obs <- c(length(model_voteshare_kpd_12w_fullsample_simple[[3]]$residuals)/2,
         length(model_voteshare_kpd_12w_matched_simple[[3]]$residuals)/2,
         length(model_voteshare_kpd_8w_fullsample_simple[[3]]$residuals)/2,
         length(model_voteshare_kpd_8w_matched_simple[[3]]$residuals)/2,
         length(model_voteshare_kpd_4w_fullsample_simple[[3]]$residuals)/2,
         length(model_voteshare_kpd_4w_matched_simple[[3]]$residuals)/2,
         length(model_voteshare_kpd_2w_fullsample_simple[[3]]$residuals)/2,
         length(model_voteshare_kpd_2w_matched_simple[[3]]$residuals)/2)
varnames_long <- c("\\cmidrule(r){2-3}\\cmidrule(r){4-5}\\cmidrule(r){6-7}\\cmidrule(r){8-9}Exposure, time span of...", "(Intercept)")
election_names <- c("... 12 weeks", "... 8 weeks", "... 4 weeks", "... 2 weeks")
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
                               title = paste0("Diff-in-diff estimates of exposure effects on KPD vote share at the Nov 1932 national parliamentary election with varying time span specifications.\\vspace{-.25cm}"), 
                               label = paste0("tab:nsdap-voteshare-kpd-timespan-dd-1932-2"),
                               out = paste0("../figures/tab-effect-voteshare-kpd-models-timespan-1932-2.tex"))

# turnout models, 1932-2 election, varying time spans
model_turnout_list <- list(model_turnout_12w_fullsample_simple[[3]], model_turnout_12w_matched_simple[[3]], 
                           model_turnout_8w_fullsample_simple[[3]], model_turnout_8w_matched_simple[[3]], 
                           model_turnout_4w_fullsample_simple[[3]], model_turnout_4w_matched_simple[[3]], 
                           model_turnout_2w_fullsample_simple[[3]], model_turnout_2w_matched_simple[[3]])
model_turnout_ses <- list(model_turnout_12w_fullsample_simple[[3]]$clusterse[,2], model_turnout_12w_matched_simple[[3]]$clusterse[,2],
                          model_turnout_8w_fullsample_simple[[3]]$clusterse[,2], model_turnout_8w_matched_simple[[3]]$clusterse[,2],
                          model_turnout_4w_fullsample_simple[[3]]$clusterse[,2], model_turnout_4w_matched_simple[[3]]$clusterse[,2],
                          model_turnout_2w_fullsample_simple[[3]]$clusterse[,2], model_turnout_2w_matched_simple[[3]]$clusterse[,2])
obs <- c(length(model_turnout_12w_fullsample_simple[[3]]$residuals)/2,
         length(model_turnout_12w_matched_simple[[3]]$residuals)/2,
         length(model_turnout_8w_fullsample_simple[[3]]$residuals)/2,
         length(model_turnout_8w_matched_simple[[3]]$residuals)/2,
         length(model_turnout_4w_fullsample_simple[[3]]$residuals)/2,
         length(model_turnout_4w_matched_simple[[3]]$residuals)/2,
         length(model_turnout_2w_fullsample_simple[[3]]$residuals)/2,
         length(model_turnout_2w_matched_simple[[3]]$residuals)/2)
varnames_long <- c("\\cmidrule(r){2-3}\\cmidrule(r){4-5}\\cmidrule(r){6-7}\\cmidrule(r){8-9}Exposure, time span of...", "(Intercept)")
election_names <- c("... 12 weeks", "... 8 weeks", "... 4 weeks", "... 2 weeks")
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
                               title = paste0("Diff-in-diff estimates of exposure effects on turnout at the Nov 1932 national parliamentary election with varying time span specifications.\\vspace{-.25cm}"), 
                               label = paste0("tab:turnout-timespan-dd-1932-2"),
                               out = paste0("../figures/tab-effect-turnout-models-timespan-1932-2.tex"))


# NSDAP vote share models, 1933 election, varying time spans
model_voteshare_list <- list(model_voteshare_12w_fullsample_simple[[4]], model_voteshare_12w_matched_simple[[4]], 
                             model_voteshare_8w_fullsample_simple[[4]], model_voteshare_8w_matched_simple[[4]], 
                             model_voteshare_4w_fullsample_simple[[4]], model_voteshare_4w_matched_simple[[4]], 
                             model_voteshare_2w_fullsample_simple[[4]], model_voteshare_2w_matched_simple[[4]])
model_voteshare_ses <- list(model_voteshare_12w_fullsample_simple[[4]]$clusterse[,2], model_voteshare_12w_matched_simple[[4]]$clusterse[,2],
                            model_voteshare_8w_fullsample_simple[[4]]$clusterse[,2], model_voteshare_8w_matched_simple[[4]]$clusterse[,2],
                            model_voteshare_4w_fullsample_simple[[4]]$clusterse[,2], model_voteshare_4w_matched_simple[[4]]$clusterse[,2],
                            model_voteshare_2w_fullsample_simple[[4]]$clusterse[,2], model_voteshare_2w_matched_simple[[4]]$clusterse[,2])
obs <- c(length(model_voteshare_12w_fullsample_simple[[4]]$residuals)/2,
         length(model_voteshare_12w_matched_simple[[4]]$residuals)/2,
         length(model_voteshare_8w_fullsample_simple[[4]]$residuals)/2,
         length(model_voteshare_8w_matched_simple[[4]]$residuals)/2,
         length(model_voteshare_4w_fullsample_simple[[4]]$residuals)/2,
         length(model_voteshare_4w_matched_simple[[4]]$residuals)/2,
         length(model_voteshare_2w_fullsample_simple[[4]]$residuals)/2,
         length(model_voteshare_2w_matched_simple[[4]]$residuals)/2)
varnames_long <- c("\\cmidrule(r){2-3}\\cmidrule(r){4-5}\\cmidrule(r){6-7}\\cmidrule(r){8-9}Exposure, time span of...", "(Intercept)")
election_names <- c("... 12 weeks", "... 8 weeks", "... 4 weeks", "... 2 weeks")
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
                               title = paste0("Diff-in-diff estimates of exposure effects on NSDAP vote share at the 1933 national parliamentary election with varying time span specifications.\\vspace{-.25cm}"), 
                               label = paste0("tab:nsdap-voteshare-timespan-dd-1933"),
                               out = paste0("../figures/tab-effect-voteshare-models-timespan-1933.tex"))

# KPD vote share models, 1933 election, varying time spans
model_voteshare_kpd_list <- list(model_voteshare_kpd_12w_fullsample_simple[[4]], model_voteshare_kpd_12w_matched_simple[[4]], 
                                 model_voteshare_kpd_8w_fullsample_simple[[4]], model_voteshare_kpd_8w_matched_simple[[4]], 
                                 model_voteshare_kpd_4w_fullsample_simple[[4]], model_voteshare_kpd_4w_matched_simple[[4]], 
                                 model_voteshare_kpd_2w_fullsample_simple[[4]], model_voteshare_kpd_2w_matched_simple[[4]])
model_voteshare_kpd_ses <- list(model_voteshare_kpd_12w_fullsample_simple[[4]]$clusterse[,2], model_voteshare_kpd_12w_matched_simple[[4]]$clusterse[,2],
                                model_voteshare_kpd_8w_fullsample_simple[[4]]$clusterse[,2], model_voteshare_kpd_8w_matched_simple[[4]]$clusterse[,2],
                                model_voteshare_kpd_4w_fullsample_simple[[4]]$clusterse[,2], model_voteshare_kpd_4w_matched_simple[[4]]$clusterse[,2],
                                model_voteshare_kpd_2w_fullsample_simple[[4]]$clusterse[,2], model_voteshare_kpd_2w_matched_simple[[4]]$clusterse[,2])
obs <- c(length(model_voteshare_kpd_12w_fullsample_simple[[4]]$residuals)/2,
         length(model_voteshare_kpd_12w_matched_simple[[4]]$residuals)/2,
         length(model_voteshare_kpd_8w_fullsample_simple[[4]]$residuals)/2,
         length(model_voteshare_kpd_8w_matched_simple[[4]]$residuals)/2,
         length(model_voteshare_kpd_4w_fullsample_simple[[4]]$residuals)/2,
         length(model_voteshare_kpd_4w_matched_simple[[4]]$residuals)/2,
         length(model_voteshare_kpd_2w_fullsample_simple[[4]]$residuals)/2,
         length(model_voteshare_kpd_2w_matched_simple[[4]]$residuals)/2)
varnames_long <- c("\\cmidrule(r){2-3}\\cmidrule(r){4-5}\\cmidrule(r){6-7}\\cmidrule(r){8-9}Exposure, time span of...", "(Intercept)")
election_names <- c("... 12 weeks", "... 8 weeks", "... 4 weeks", "... 2 weeks")
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
                               title = paste0("Diff-in-diff estimates of exposure effects on KPD vote share at the 1933 national parliamentary election with varying time span specifications.\\vspace{-.25cm}"), 
                               label = paste0("tab:nsdap-voteshare-kpd-timespan-dd-1933"),
                               out = paste0("../figures/tab-effect-voteshare-kpd-models-timespan-1933.tex"))

# turnout models, 1933 election, varying time spans
model_turnout_list <- list(model_turnout_12w_fullsample_simple[[4]], model_turnout_12w_matched_simple[[4]], 
                           model_turnout_8w_fullsample_simple[[4]], model_turnout_8w_matched_simple[[4]], 
                           model_turnout_4w_fullsample_simple[[4]], model_turnout_4w_matched_simple[[4]], 
                           model_turnout_2w_fullsample_simple[[4]], model_turnout_2w_matched_simple[[4]])
model_turnout_ses <- list(model_turnout_12w_fullsample_simple[[4]]$clusterse[,2], model_turnout_12w_matched_simple[[4]]$clusterse[,2],
                          model_turnout_8w_fullsample_simple[[4]]$clusterse[,2], model_turnout_8w_matched_simple[[4]]$clusterse[,2],
                          model_turnout_4w_fullsample_simple[[4]]$clusterse[,2], model_turnout_4w_matched_simple[[4]]$clusterse[,2],
                          model_turnout_2w_fullsample_simple[[4]]$clusterse[,2], model_turnout_2w_matched_simple[[4]]$clusterse[,2])
obs <- c(length(model_turnout_12w_fullsample_simple[[4]]$residuals)/2,
         length(model_turnout_12w_matched_simple[[4]]$residuals)/2,
         length(model_turnout_8w_fullsample_simple[[4]]$residuals)/2,
         length(model_turnout_8w_matched_simple[[4]]$residuals)/2,
         length(model_turnout_4w_fullsample_simple[[4]]$residuals)/2,
         length(model_turnout_4w_matched_simple[[4]]$residuals)/2,
         length(model_turnout_2w_fullsample_simple[[4]]$residuals)/2,
         length(model_turnout_2w_matched_simple[[4]]$residuals)/2)
varnames_long <- c("\\cmidrule(r){2-3}\\cmidrule(r){4-5}\\cmidrule(r){6-7}\\cmidrule(r){8-9}Exposure, time span of...", "(Intercept)")
election_names <- c("... 12 weeks", "... 8 weeks", "... 4 weeks", "... 2 weeks")
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
                               title = paste0("Diff-in-diff estimates of exposure effects on turnout at the 1933 national parliamentary election with varying time span specifications.\\vspace{-.25cm}"), 
                               label = paste0("tab:turnout-timespan-dd-1933"),
                               out = paste0("../figures/tab-effect-turnout-models-timespan-1933.tex"))

# NSDAP vote share models, 1930 election, municipal level, varying time spans
model_voteshare_list <- list(model_voteshare_comm_fullsample_simple_12w, model_voteshare_comm_matched_simple_12w, 
                             model_voteshare_comm_fullsample_simple_8w, model_voteshare_comm_matched_simple_8w, 
                             model_voteshare_comm_fullsample_simple_4w, model_voteshare_comm_matched_simple_4w, 
                             model_voteshare_comm_fullsample_simple_2w, model_voteshare_comm_matched_simple_2w)
model_voteshare_ses <- list(model_voteshare_comm_fullsample_simple_12w$clusterse[,2], model_voteshare_comm_matched_simple_12w$clusterse[,2],
                            model_voteshare_comm_fullsample_simple_8w$clusterse[,2], model_voteshare_comm_matched_simple_8w$clusterse[,2],
                            model_voteshare_comm_fullsample_simple_4w$clusterse[,2], model_voteshare_comm_matched_simple_4w$clusterse[,2],
                            model_voteshare_comm_fullsample_simple_2w$clusterse[,2], model_voteshare_comm_matched_simple_2w$clusterse[,2])
obs <- c(length(model_voteshare_comm_fullsample_simple_12w$residuals)/2,
         length(model_voteshare_comm_matched_simple_12w$residuals)/2,
         length(model_voteshare_comm_fullsample_simple_8w$residuals)/2,
         length(model_voteshare_comm_matched_simple_8w$residuals)/2,
         length(model_voteshare_comm_fullsample_simple_4w$residuals)/2,
         length(model_voteshare_comm_matched_simple_4w$residuals)/2,
         length(model_voteshare_comm_fullsample_simple_2w$residuals)/2,
         length(model_voteshare_comm_matched_simple_2w$residuals)/2)
varnames_long <- c("\\cmidrule(r){2-3}\\cmidrule(r){4-5}\\cmidrule(r){6-7}\\cmidrule(r){8-9}Exposure, time span of...", "(Intercept)")
election_names <- c("... 12 weeks", "... 8 weeks", "... 4 weeks", "... 2 weeks")
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
                               title = paste0("Diff-in-diff estimates of exposure effects on NSDAP vote share at the 1930 national parliamentary election, using municipal-level data, with varying time span specifications.\\vspace{-.25cm}"), 
                               label = paste0("tab:nsdap-voteshare-timespan-dd-1930comm"),
                               out = paste0("../figures/tab-effect-voteshare-models-timespan-1930comm.tex"))


# KPD vote share models, 1930 election, municipal level, varying time spans
model_voteshare_kpd_list <- list(model_voteshare_kpd_comm_fullsample_simple_12w, model_voteshare_kpd_comm_matched_simple_12w, 
                                 model_voteshare_kpd_comm_fullsample_simple_8w, model_voteshare_kpd_comm_matched_simple_8w, 
                                 model_voteshare_kpd_comm_fullsample_simple_4w, model_voteshare_kpd_comm_matched_simple_4w, 
                                 model_voteshare_kpd_comm_fullsample_simple_2w, model_voteshare_kpd_comm_matched_simple_2w)
model_voteshare_kpd_ses <- list(model_voteshare_kpd_comm_fullsample_simple_12w$clusterse[,2], model_voteshare_kpd_comm_matched_simple_12w$clusterse[,2],
                                model_voteshare_kpd_comm_fullsample_simple_8w$clusterse[,2], model_voteshare_kpd_comm_matched_simple_8w$clusterse[,2],
                                model_voteshare_kpd_comm_fullsample_simple_4w$clusterse[,2], model_voteshare_kpd_comm_matched_simple_4w$clusterse[,2],
                                model_voteshare_kpd_comm_fullsample_simple_2w$clusterse[,2], model_voteshare_kpd_comm_matched_simple_2w$clusterse[,2])
obs <- c(length(model_voteshare_kpd_comm_fullsample_simple_12w$residuals)/2,
         length(model_voteshare_kpd_comm_matched_simple_12w$residuals)/2,
         length(model_voteshare_kpd_comm_fullsample_simple_8w$residuals)/2,
         length(model_voteshare_kpd_comm_matched_simple_8w$residuals)/2,
         length(model_voteshare_kpd_comm_fullsample_simple_4w$residuals)/2,
         length(model_voteshare_kpd_comm_matched_simple_4w$residuals)/2,
         length(model_voteshare_kpd_comm_fullsample_simple_2w$residuals)/2,
         length(model_voteshare_kpd_comm_matched_simple_2w$residuals)/2)
varnames_long <- c("\\cmidrule(r){2-3}\\cmidrule(r){4-5}\\cmidrule(r){6-7}\\cmidrule(r){8-9}Exposure, radius of...", "(Intercept)")
election_names <- c("... 12 weeks", "... 8 weeks", "... 4 weeks", "... 2 weeks")
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
                               title = paste0("Diff-in-diff estimates of exposure effects on KPD vote share at the 1930 national parliamentary election, using municipal-level data, with varying time span specifications.\\vspace{-.25cm}"), 
                               label = paste0("tab:voteshare-kpd-timespan-dd-1930comm"),
                               out = paste0("../figures/tab-effect-voteshare-kpd-models-timespan-1930comm.tex"))

# turnout models, 1930 election, municipal level, varying time spans
model_turnout_list <- list(model_turnout_comm_fullsample_simple_12w, model_turnout_comm_matched_simple_12w, 
                           model_turnout_comm_fullsample_simple_8w, model_turnout_comm_matched_simple_8w, 
                           model_turnout_comm_fullsample_simple_4w, model_turnout_comm_matched_simple_4w, 
                           model_turnout_comm_fullsample_simple_2w, model_turnout_comm_matched_simple_2w)
model_turnout_ses <- list(model_turnout_comm_fullsample_simple_12w$clusterse[,2], model_turnout_comm_matched_simple_12w$clusterse[,2],
                          model_turnout_comm_fullsample_simple_8w$clusterse[,2], model_turnout_comm_matched_simple_8w$clusterse[,2],
                          model_turnout_comm_fullsample_simple_4w$clusterse[,2], model_turnout_comm_matched_simple_4w$clusterse[,2],
                          model_turnout_comm_fullsample_simple_2w$clusterse[,2], model_turnout_comm_matched_simple_2w$clusterse[,2])
obs <- c(length(model_turnout_comm_fullsample_simple_12w$residuals)/2,
         length(model_turnout_comm_matched_simple_12w$residuals)/2,
         length(model_turnout_comm_fullsample_simple_8w$residuals)/2,
         length(model_turnout_comm_matched_simple_8w$residuals)/2,
         length(model_turnout_comm_fullsample_simple_4w$residuals)/2,
         length(model_turnout_comm_matched_simple_4w$residuals)/2,
         length(model_turnout_comm_fullsample_simple_2w$residuals)/2,
         length(model_turnout_comm_matched_simple_2w$residuals)/2)
varnames_long <- c("\\cmidrule(r){2-3}\\cmidrule(r){4-5}\\cmidrule(r){6-7}\\cmidrule(r){8-9}Exposure, radius of...", "(Intercept)")
election_names <- c("... 12 weeks", "... 8 weeks", "... 4 weeks", "... 2 weeks")
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
                               title = paste0("Diff-in-diff estimates of exposure effects on turnout at the 1930 national parliamentary election, using municipal-level data, with varying time span specifications.\\vspace{-.25cm}"), 
                               label = paste0("tab:turnout-timespan-dd-1930comm"),
                               out = paste0("../figures/tab-effect-turnout-models-timespan-1930comm.tex"))


## plot ----
pdf(file="../figures/effects_coefplot_timespan.pdf", height=4, width=8, family="URWTimes")
par(oma=c(.5,0,.5,0))
par(mar=c(3,7,3,1))
layout(matrix(1:3, 1, byrow = TRUE), widths=c(1.3, 1, 1), heights=c(2, 2, 2))
ylabel_names <- rev(c("Sep 1930", "Sep 1930 (mun.)", "Jul 1932", "Nov 1932", "Mar 1933"))
colors <- brewer.pal(9,"Greys")
colors <- c(colors[4], colors[5], colors[7], colors[9])
## voteshare effect plot
plot(0, 0, xlim = c(-0.07, .07), ylim = c(-.3, 4.3), pch = 20, col = "white", xlab = "", ylab = "", yaxt = "n", xaxt = "n")
axis(2, at = 0:4, labels = F, tick = F)
text(y = 0:4, par("usr")[1], labels = ylabel_names, srt = 0, pos = 2, xpd = TRUE)
axis(1, at = seq(-.06, .06, .03), labels = seq(-.06, .06, .03), tick = T)
mtext("NSDAP vote share", side = 3, line = 1.2, outer = FALSE)
abline(v = 0, lty = 2)
abline(h = c(.5, 1.5, 2.5, 3.5), lty = 1, col = "darkgrey")
# Sep 1930
y_par <- 4
mod_input <- model_voteshare_12w_matched_simple[[1]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.25, x1 = mod$coef_hardhi, y1 = y_par+.25, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.25, x1 = mod$coef_weakhi, y1 = y_par+.25, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.25, pch = 20, col = colors[1])
mod_input <- model_voteshare_8w_matched_simple[[1]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.083, x1 = mod$coef_hardhi, y1 = y_par+.083, length = 0, col = colors[2])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.083, x1 = mod$coef_weakhi, y1 = y_par+.083, length = 0, lwd = 2, col = colors[2])
points(mod$coef_point, y_par+.083, pch = 20, col = colors[2])
mod_input <- model_voteshare_4w_matched_simple[[1]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.083, x1 = mod$coef_hardhi, y1 = y_par-.083, length = 0, col = colors[3])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.083, x1 = mod$coef_weakhi, y1 = y_par-.083, length = 0, lwd = 2, col = colors[3])
points(mod$coef_point, y_par-.083, pch = 20, col = colors[3])
mod_input <- model_voteshare_2w_matched_simple[[1]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.25, x1 = mod$coef_hardhi, y1 = y_par-.25, length = 0, col = colors[4])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.25, x1 = mod$coef_weakhi, y1 = y_par-.25, length = 0, lwd = 2, col = colors[4])
points(mod$coef_point, y_par-.25, pch = 20, col = colors[4])
# Sep 1930 (mun.)
y_par <- 3
mod_input <- model_voteshare_comm_matched_simple_12w
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.25, x1 = mod$coef_hardhi, y1 = y_par+.25, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.25, x1 = mod$coef_weakhi, y1 = y_par+.25, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.25, pch = 20, col = colors[1])
mod_input <- model_voteshare_comm_matched_simple_8w
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.083, x1 = mod$coef_hardhi, y1 = y_par+.083, length = 0, col = colors[2])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.083, x1 = mod$coef_weakhi, y1 = y_par+.083, length = 0, lwd = 2, col = colors[2])
points(mod$coef_point, y_par+.083, pch = 20, col = colors[2])
mod_input <- model_voteshare_comm_matched_simple_4w
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.083, x1 = mod$coef_hardhi, y1 = y_par-.083, length = 0, col = colors[3])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.083, x1 = mod$coef_weakhi, y1 = y_par-.083, length = 0, lwd = 2, col = colors[3])
points(mod$coef_point, y_par-.083, pch = 20, col = colors[3])
mod_input <- model_voteshare_comm_matched_simple_2w
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.25, x1 = mod$coef_hardhi, y1 = y_par-.25, length = 0, col = colors[4])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.25, x1 = mod$coef_weakhi, y1 = y_par-.25, length = 0, lwd = 2, col = colors[4])
points(mod$coef_point, y_par-.25, pch = 20, col = colors[4])
# Jul 1932
y_par <- 2
mod_input <- model_voteshare_12w_matched_simple[[2]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.25, x1 = mod$coef_hardhi, y1 = y_par+.25, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.25, x1 = mod$coef_weakhi, y1 = y_par+.25, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.25, pch = 20, col = colors[1])
mod_input <- model_voteshare_8w_matched_simple[[2]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.083, x1 = mod$coef_hardhi, y1 = y_par+.083, length = 0, col = colors[2])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.083, x1 = mod$coef_weakhi, y1 = y_par+.083, length = 0, lwd = 2, col = colors[2])
points(mod$coef_point, y_par+.083, pch = 20, col = colors[2])
mod_input <- model_voteshare_4w_matched_simple[[2]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.083, x1 = mod$coef_hardhi, y1 = y_par-.083, length = 0, col = colors[3])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.083, x1 = mod$coef_weakhi, y1 = y_par-.083, length = 0, lwd = 2, col = colors[3])
points(mod$coef_point, y_par-.083, pch = 20, col = colors[3])
mod_input <- model_voteshare_2w_matched_simple[[2]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.25, x1 = mod$coef_hardhi, y1 = y_par-.25, length = 0, col = colors[4])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.25, x1 = mod$coef_weakhi, y1 = y_par-.25, length = 0, lwd = 2, col = colors[4])
points(mod$coef_point, y_par-.25, pch = 20, col = colors[4])
# Nov 1932
y_par <- 1
mod_input <- model_voteshare_12w_matched_simple[[3]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.25, x1 = mod$coef_hardhi, y1 = y_par+.25, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.25, x1 = mod$coef_weakhi, y1 = y_par+.25, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.25, pch = 20, col = colors[1])
mod_input <- model_voteshare_8w_matched_simple[[3]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.083, x1 = mod$coef_hardhi, y1 = y_par+.083, length = 0, col = colors[2])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.083, x1 = mod$coef_weakhi, y1 = y_par+.083, length = 0, lwd = 2, col = colors[2])
points(mod$coef_point, y_par+.083, pch = 20, col = colors[2])
mod_input <- model_voteshare_4w_matched_simple[[3]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.083, x1 = mod$coef_hardhi, y1 = y_par-.083, length = 0, col = colors[3])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.083, x1 = mod$coef_weakhi, y1 = y_par-.083, length = 0, lwd = 2, col = colors[3])
points(mod$coef_point, y_par-.083, pch = 20, col = colors[3])
mod_input <- model_voteshare_2w_matched_simple[[3]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.25, x1 = mod$coef_hardhi, y1 = y_par-.25, length = 0, col = colors[4])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.25, x1 = mod$coef_weakhi, y1 = y_par-.25, length = 0, lwd = 2, col = colors[4])
points(mod$coef_point, y_par-.25, pch = 20, col = colors[4])
# Mar 1933
y_par <- 0
mod_input <- model_voteshare_12w_matched_simple[[4]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.25, x1 = mod$coef_hardhi, y1 = y_par+.25, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.25, x1 = mod$coef_weakhi, y1 = y_par+.25, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.25, pch = 20, col = colors[1])
mod_input <- model_voteshare_8w_matched_simple[[4]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.083, x1 = mod$coef_hardhi, y1 = y_par+.083, length = 0, col = colors[2])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.083, x1 = mod$coef_weakhi, y1 = y_par+.083, length = 0, lwd = 2, col = colors[2])
points(mod$coef_point, y_par+.083, pch = 20, col = colors[2])
mod_input <- model_voteshare_4w_matched_simple[[4]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.083, x1 = mod$coef_hardhi, y1 = y_par-.083, length = 0, col = colors[3])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.083, x1 = mod$coef_weakhi, y1 = y_par-.083, length = 0, lwd = 2, col = colors[3])
points(mod$coef_point, y_par-.083, pch = 20, col = colors[3])
mod_input <- model_voteshare_2w_matched_simple[[4]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.25, x1 = mod$coef_hardhi, y1 = y_par-.25, length = 0, col = colors[4])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.25, x1 = mod$coef_weakhi, y1 = y_par-.25, length = 0, lwd = 2, col = colors[4])
points(mod$coef_point, y_par-.25, pch = 20, col = colors[4])

## kpd voteshare effect plot
par(mar=c(3,0,3,1))
plot(0, 0, xlim = c(-0.07, .07), ylim = c(-.3, 4.3), pch = 20, col = "white", xlab = "", ylab = "", yaxt = "n", xaxt = "n")
axis(1, at = seq(-.06, .06, .03), labels = seq(-.06, .06, .03), tick = T)
mtext("KPD vote share", side = 3, line = 1.2, outer = FALSE)
abline(v = 0, lty = 2)
abline(h = c(.5, 1.5, 2.5, 3.5), lty = 1, col = "darkgrey")
# Sep 1930
y_par <- 4
mod_input <- model_voteshare_kpd_12w_matched_simple[[1]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.25, x1 = mod$coef_hardhi, y1 = y_par+.25, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.25, x1 = mod$coef_weakhi, y1 = y_par+.25, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.25, pch = 20, col = colors[1])
mod_input <- model_voteshare_kpd_8w_matched_simple[[1]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.083, x1 = mod$coef_hardhi, y1 = y_par+.083, length = 0, col = colors[2])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.083, x1 = mod$coef_weakhi, y1 = y_par+.083, length = 0, lwd = 2, col = colors[2])
points(mod$coef_point, y_par+.083, pch = 20, col = colors[2])
mod_input <- model_voteshare_kpd_4w_matched_simple[[1]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.083, x1 = mod$coef_hardhi, y1 = y_par-.083, length = 0, col = colors[3])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.083, x1 = mod$coef_weakhi, y1 = y_par-.083, length = 0, lwd = 2, col = colors[3])
points(mod$coef_point, y_par-.083, pch = 20, col = colors[3])
mod_input <- model_voteshare_kpd_2w_matched_simple[[1]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.25, x1 = mod$coef_hardhi, y1 = y_par-.25, length = 0, col = colors[4])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.25, x1 = mod$coef_weakhi, y1 = y_par-.25, length = 0, lwd = 2, col = colors[4])
points(mod$coef_point, y_par-.25, pch = 20, col = colors[4])
# Sep 1930 (mun.)
y_par <- 3
mod_input <- model_voteshare_kpd_comm_matched_simple_12w
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.25, x1 = mod$coef_hardhi, y1 = y_par+.25, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.25, x1 = mod$coef_weakhi, y1 = y_par+.25, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.25, pch = 20, col = colors[1])
mod_input <- model_voteshare_kpd_comm_matched_simple_8w
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.083, x1 = mod$coef_hardhi, y1 = y_par+.083, length = 0, col = colors[2])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.083, x1 = mod$coef_weakhi, y1 = y_par+.083, length = 0, lwd = 2, col = colors[2])
points(mod$coef_point, y_par+.083, pch = 20, col = colors[2])
mod_input <- model_voteshare_kpd_comm_matched_simple_4w
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.083, x1 = mod$coef_hardhi, y1 = y_par-.083, length = 0, col = colors[3])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.083, x1 = mod$coef_weakhi, y1 = y_par-.083, length = 0, lwd = 2, col = colors[3])
points(mod$coef_point, y_par-.083, pch = 20, col = colors[3])
mod_input <- model_voteshare_kpd_comm_matched_simple_2w
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.25, x1 = mod$coef_hardhi, y1 = y_par-.25, length = 0, col = colors[4])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.25, x1 = mod$coef_weakhi, y1 = y_par-.25, length = 0, lwd = 2, col = colors[4])
points(mod$coef_point, y_par-.25, pch = 20, col = colors[4])
# Jul 1932
y_par <- 2
mod_input <- model_voteshare_kpd_12w_matched_simple[[2]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.25, x1 = mod$coef_hardhi, y1 = y_par+.25, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.25, x1 = mod$coef_weakhi, y1 = y_par+.25, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.25, pch = 20, col = colors[1])
mod_input <- model_voteshare_kpd_8w_matched_simple[[2]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.083, x1 = mod$coef_hardhi, y1 = y_par+.083, length = 0, col = colors[2])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.083, x1 = mod$coef_weakhi, y1 = y_par+.083, length = 0, lwd = 2, col = colors[2])
points(mod$coef_point, y_par+.083, pch = 20, col = colors[2])
mod_input <- model_voteshare_kpd_4w_matched_simple[[2]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.083, x1 = mod$coef_hardhi, y1 = y_par-.083, length = 0, col = colors[3])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.083, x1 = mod$coef_weakhi, y1 = y_par-.083, length = 0, lwd = 2, col = colors[3])
points(mod$coef_point, y_par-.083, pch = 20, col = colors[3])
mod_input <- model_voteshare_kpd_2w_matched_simple[[2]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.25, x1 = mod$coef_hardhi, y1 = y_par-.25, length = 0, col = colors[4])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.25, x1 = mod$coef_weakhi, y1 = y_par-.25, length = 0, lwd = 2, col = colors[4])
points(mod$coef_point, y_par-.25, pch = 20, col = colors[4])
# Nov 1932
y_par <- 1
mod_input <- model_voteshare_kpd_12w_matched_simple[[3]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.25, x1 = mod$coef_hardhi, y1 = y_par+.25, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.25, x1 = mod$coef_weakhi, y1 = y_par+.25, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.25, pch = 20, col = colors[1])
mod_input <- model_voteshare_kpd_8w_matched_simple[[3]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.083, x1 = mod$coef_hardhi, y1 = y_par+.083, length = 0, col = colors[2])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.083, x1 = mod$coef_weakhi, y1 = y_par+.083, length = 0, lwd = 2, col = colors[2])
points(mod$coef_point, y_par+.083, pch = 20, col = colors[2])
mod_input <- model_voteshare_kpd_4w_matched_simple[[3]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.083, x1 = mod$coef_hardhi, y1 = y_par-.083, length = 0, col = colors[3])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.083, x1 = mod$coef_weakhi, y1 = y_par-.083, length = 0, lwd = 2, col = colors[3])
points(mod$coef_point, y_par-.083, pch = 20, col = colors[3])
mod_input <- model_voteshare_kpd_2w_matched_simple[[3]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.25, x1 = mod$coef_hardhi, y1 = y_par-.25, length = 0, col = colors[4])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.25, x1 = mod$coef_weakhi, y1 = y_par-.25, length = 0, lwd = 2, col = colors[4])
points(mod$coef_point, y_par-.25, pch = 20, col = colors[4])
# Mar 1933
y_par <- 0
mod_input <- model_voteshare_kpd_12w_matched_simple[[4]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.25, x1 = mod$coef_hardhi, y1 = y_par+.25, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.25, x1 = mod$coef_weakhi, y1 = y_par+.25, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.25, pch = 20, col = colors[1])
mod_input <- model_voteshare_kpd_8w_matched_simple[[4]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.083, x1 = mod$coef_hardhi, y1 = y_par+.083, length = 0, col = colors[2])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.083, x1 = mod$coef_weakhi, y1 = y_par+.083, length = 0, lwd = 2, col = colors[2])
points(mod$coef_point, y_par+.083, pch = 20, col = colors[2])
mod_input <- model_voteshare_kpd_4w_matched_simple[[4]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.083, x1 = mod$coef_hardhi, y1 = y_par-.083, length = 0, col = colors[3])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.083, x1 = mod$coef_weakhi, y1 = y_par-.083, length = 0, lwd = 2, col = colors[3])
points(mod$coef_point, y_par-.083, pch = 20, col = colors[3])
mod_input <- model_voteshare_kpd_2w_matched_simple[[4]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.25, x1 = mod$coef_hardhi, y1 = y_par-.25, length = 0, col = colors[4])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.25, x1 = mod$coef_weakhi, y1 = y_par-.25, length = 0, lwd = 2, col = colors[4])
points(mod$coef_point, y_par-.25, pch = 20, col = colors[4])
text(.07, y_par+.25, labels = "within 12 weeks b.e.", col = colors[1], pos = 2)
text(.07, y_par+.083, labels = "within 8 weeks b.e.", col = colors[2], pos = 2)
text(.07, y_par-.083, labels = "within 4 weeks b.e.", col = colors[3], pos = 2)
text(.07, y_par-.25, labels = "within 2 weeks b.e.", col = colors[4], pos = 2)

## turnout effect plot
par(mar=c(3,0,3,1))
plot(0, 0, xlim = c(-0.07, .07), ylim = c(-.3, 4.3), pch = 20, col = "white", xlab = "", ylab = "", yaxt = "n", xaxt = "n")
axis(1, at = seq(-.06, .06, .03), labels = seq(-.06, .06, .03), tick = T)
mtext("Turnout", side = 3, line = 1.2, outer = FALSE)
abline(v = 0, lty = 2)
abline(h = c(.5, 1.5, 2.5, 3.5), lty = 1, col = "darkgrey")
# Sep 1930
y_par <- 4
mod_input <- model_turnout_12w_matched_simple[[1]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.25, x1 = mod$coef_hardhi, y1 = y_par+.25, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.25, x1 = mod$coef_weakhi, y1 = y_par+.25, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.25, pch = 20, col = colors[1])
mod_input <- model_turnout_8w_matched_simple[[1]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.083, x1 = mod$coef_hardhi, y1 = y_par+.083, length = 0, col = colors[2])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.083, x1 = mod$coef_weakhi, y1 = y_par+.083, length = 0, lwd = 2, col = colors[2])
points(mod$coef_point, y_par+.083, pch = 20, col = colors[2])
mod_input <- model_turnout_4w_matched_simple[[1]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.083, x1 = mod$coef_hardhi, y1 = y_par-.083, length = 0, col = colors[3])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.083, x1 = mod$coef_weakhi, y1 = y_par-.083, length = 0, lwd = 2, col = colors[3])
points(mod$coef_point, y_par-.083, pch = 20, col = colors[3])
mod_input <- model_turnout_2w_matched_simple[[1]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.25, x1 = mod$coef_hardhi, y1 = y_par-.25, length = 0, col = colors[4])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.25, x1 = mod$coef_weakhi, y1 = y_par-.25, length = 0, lwd = 2, col = colors[4])
points(mod$coef_point, y_par-.25, pch = 20, col = colors[4])
# Sep 1930 (mun.)
y_par <- 3
mod_input <- model_turnout_comm_matched_simple_12w
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.25, x1 = mod$coef_hardhi, y1 = y_par+.25, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.25, x1 = mod$coef_weakhi, y1 = y_par+.25, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.25, pch = 20, col = colors[1])
mod_input <- model_turnout_comm_matched_simple_8w
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.083, x1 = mod$coef_hardhi, y1 = y_par+.083, length = 0, col = colors[2])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.083, x1 = mod$coef_weakhi, y1 = y_par+.083, length = 0, lwd = 2, col = colors[2])
points(mod$coef_point, y_par+.083, pch = 20, col = colors[2])
mod_input <- model_turnout_comm_matched_simple_4w
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.083, x1 = mod$coef_hardhi, y1 = y_par-.083, length = 0, col = colors[3])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.083, x1 = mod$coef_weakhi, y1 = y_par-.083, length = 0, lwd = 2, col = colors[3])
points(mod$coef_point, y_par-.083, pch = 20, col = colors[3])
mod_input <- model_turnout_comm_matched_simple_2w
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.25, x1 = mod$coef_hardhi, y1 = y_par-.25, length = 0, col = colors[4])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.25, x1 = mod$coef_weakhi, y1 = y_par-.25, length = 0, lwd = 2, col = colors[4])
points(mod$coef_point, y_par-.25, pch = 20, col = colors[4])
# Jul 1932
y_par <- 2
mod_input <- model_turnout_12w_matched_simple[[2]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.25, x1 = mod$coef_hardhi, y1 = y_par+.25, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.25, x1 = mod$coef_weakhi, y1 = y_par+.25, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.25, pch = 20, col = colors[1])
mod_input <- model_turnout_8w_matched_simple[[2]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.083, x1 = mod$coef_hardhi, y1 = y_par+.083, length = 0, col = colors[2])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.083, x1 = mod$coef_weakhi, y1 = y_par+.083, length = 0, lwd = 2, col = colors[2])
points(mod$coef_point, y_par+.083, pch = 20, col = colors[2])
mod_input <- model_turnout_4w_matched_simple[[2]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.083, x1 = mod$coef_hardhi, y1 = y_par-.083, length = 0, col = colors[3])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.083, x1 = mod$coef_weakhi, y1 = y_par-.083, length = 0, lwd = 2, col = colors[3])
points(mod$coef_point, y_par-.083, pch = 20, col = colors[3])
mod_input <- model_turnout_2w_matched_simple[[2]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.25, x1 = mod$coef_hardhi, y1 = y_par-.25, length = 0, col = colors[4])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.25, x1 = mod$coef_weakhi, y1 = y_par-.25, length = 0, lwd = 2, col = colors[4])
points(mod$coef_point, y_par-.25, pch = 20, col = colors[4])
# Nov 1932
y_par <- 1
mod_input <- model_turnout_12w_matched_simple[[3]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.25, x1 = mod$coef_hardhi, y1 = y_par+.25, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.25, x1 = mod$coef_weakhi, y1 = y_par+.25, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.25, pch = 20, col = colors[1])
mod_input <- model_turnout_8w_matched_simple[[3]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.083, x1 = mod$coef_hardhi, y1 = y_par+.083, length = 0, col = colors[2])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.083, x1 = mod$coef_weakhi, y1 = y_par+.083, length = 0, lwd = 2, col = colors[2])
points(mod$coef_point, y_par+.083, pch = 20, col = colors[2])
mod_input <- model_turnout_4w_matched_simple[[3]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.083, x1 = mod$coef_hardhi, y1 = y_par-.083, length = 0, col = colors[3])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.083, x1 = mod$coef_weakhi, y1 = y_par-.083, length = 0, lwd = 2, col = colors[3])
points(mod$coef_point, y_par-.083, pch = 20, col = colors[3])
mod_input <- model_turnout_2w_matched_simple[[3]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.25, x1 = mod$coef_hardhi, y1 = y_par-.25, length = 0, col = colors[4])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.25, x1 = mod$coef_weakhi, y1 = y_par-.25, length = 0, lwd = 2, col = colors[4])
points(mod$coef_point, y_par-.25, pch = 20, col = colors[4])
# Mar 1933
y_par <- 0
mod_input <- model_turnout_12w_matched_simple[[4]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.25, x1 = mod$coef_hardhi, y1 = y_par+.25, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.25, x1 = mod$coef_weakhi, y1 = y_par+.25, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.25, pch = 20, col = colors[1])
mod_input <- model_turnout_8w_matched_simple[[4]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.083, x1 = mod$coef_hardhi, y1 = y_par+.083, length = 0, col = colors[2])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.083, x1 = mod$coef_weakhi, y1 = y_par+.083, length = 0, lwd = 2, col = colors[2])
points(mod$coef_point, y_par+.083, pch = 20, col = colors[2])
mod_input <- model_turnout_4w_matched_simple[[4]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.083, x1 = mod$coef_hardhi, y1 = y_par-.083, length = 0, col = colors[3])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.083, x1 = mod$coef_weakhi, y1 = y_par-.083, length = 0, lwd = 2, col = colors[3])
points(mod$coef_point, y_par-.083, pch = 20, col = colors[3])
mod_input <- model_turnout_2w_matched_simple[[4]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.25, x1 = mod$coef_hardhi, y1 = y_par-.25, length = 0, col = colors[4])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.25, x1 = mod$coef_weakhi, y1 = y_par-.25, length = 0, lwd = 2, col = colors[4])
points(mod$coef_point, y_par-.25, pch = 20, col = colors[4])
dev.off()





