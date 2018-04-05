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



## number of visits -----------------------------

# overview
table(county_df_long$vcount_10km, county_df_long$election) # cases rare where county is exposed more often than just once!
table(community_df_long$vcount_10km)

# define new exposure variables
county_df_long$visit_10km_1count <- county_df_long$vcount_10km == 1 
county_df_long$visit_10km_1count_nomatch <- county_df_long$visit_10km_nomatch
county_df_long$visit_10km_2count <- county_df_long$vcount_10km == 2 
county_df_long$visit_10km_2count_nomatch <- county_df_long$visit_10km_nomatch
county_df_long$visit_10km_3count <- county_df_long$vcount_10km >= 3
county_df_long$visit_10km_3count_nomatch <- county_df_long$visit_10km_nomatch


# generate separate matching data frames
dat_match_1count_list <- list()
dat_match_2count_list <- list()
dat_match_3count_list <- list()
dat_full_1count_list <- list()
dat_full_2count_list <- list()
dat_full_3count_list <- list()
for (i in 1:4) {
 # prepare data
  dat_raw <- county_df_long[county_df_long$election == i+1,]
  dat_raw$no_1count <- dat_raw$visit_10km_1count == 1 | dat_raw$visit_10km == 0
  dat_raw$no_2count <- dat_raw$visit_10km_2count == 1 | dat_raw$visit_10km == 0
  dat_raw$no_3count <- dat_raw$visit_10km_3count == 1 | dat_raw$visit_10km == 0
  # full sample, lm simple model with voting population weights (i.e., only number of voters is considered)
  dat_full_1count_list[[i]] <- getDatFull("county_df_long", election = i+1, treatment = "visit_10km_1count")
  dat_full_2count_list[[i]] <- getDatFull("county_df_long", election = i+1, treatment = "visit_10km_2count")
  dat_full_3count_list[[i]] <- getDatFull("county_df_long", election = i+1, treatment = "visit_10km_3count")
  # 1:1 matching, lm simple model with voting population weights
  match.data.1count.list <- list()
  match.data.2count.list <- list()
  match.data.3count.list <- list()
  try(match.data.1count.list[[i]] <- performMatch("visit_10km_1count", dat.raw = "dat_raw", electiontype = "county", goebbels.adapt = FALSE, exactmatch = "no_1count", treat.var.red = FALSE))
  try(match.data.2count.list[[i]] <- performMatch("visit_10km_2count", dat.raw = "dat_raw", electiontype = "county", goebbels.adapt = FALSE, exactmatch = "no_2count", treat.var.red = FALSE)) 
  try(match.data.3count.list[[i]] <- performMatch("visit_10km_3count", dat.raw = "dat_raw", electiontype = "county", goebbels.adapt = FALSE, exactmatch = "no_3count", treat.var.red = FALSE)) 
  try(dat_match_1count <- match.data.1count.list[[i]])
  try(dat_match_2count <- match.data.2count.list[[i]])
  try(  dat_match_3count <- match.data.3count.list[[i]])
  try(dat_match_1count_list[[i]] <- getDatMatch(dat.match = "dat_match_1count", dat.orig = "county_df_long", election = i+1, treatment = "visit_10km_1count"))
  try(dat_match_2count_list[[i]] <- getDatMatch(dat.match = "dat_match_2count", dat.orig = "county_df_long", election = i+1, treatment = "visit_10km_2count"))
  try(dat_match_3count_list[[i]] <- getDatMatch(dat.match = "dat_match_3count", dat.orig = "county_df_long", election = i+1, treatment = "visit_10km_3count"))
}

# collapse data into pooled data frame
dat_full_1count_pooled <- do.call(rbind.fill, dat_full_1count_list)
dat_full_2count_pooled <- do.call(rbind.fill, dat_full_2count_list)
dat_full_3count_pooled <- do.call(rbind.fill, dat_full_3count_list)
dat_match_1count_pooled <- do.call(rbind.fill, dat_match_1count_list)
dat_match_2count_pooled <- do.call(rbind.fill, dat_match_2count_list)
dat_match_3count_pooled <- do.call(rbind.fill, dat_match_3count_list)

# run model
model_voteshare_formula_simple <- paste("p_nsdap ~ as.factor(election) + time + exposure + timeXexposure",sep = "") %>% as.formula

model_voteshare_1count_fullsample_simple <- lm(model_voteshare_formula_simple, data =  dat_full_1count_pooled, weights = dat_full_1count_pooled$gs)
model_voteshare_1count_fullsample_simple$clusterse <- runClusterRobustOLS(model_voteshare_1count_fullsample_simple, dat_full_1count_pooled$lfnr)
summary(model_voteshare_1count_full_simple)

model_voteshare_2count_fullsample_simple <- lm(model_voteshare_formula_simple, data =  dat_full_2count_pooled, weights = dat_full_2count_pooled$gs)
model_voteshare_2count_fullsample_simple$clusterse <- runClusterRobustOLS(model_voteshare_2count_fullsample_simple, dat_full_2count_pooled$lfnr)
summary(model_voteshare_2count_fullsample_simple)

model_voteshare_3count_fullsample_simple <- lm(model_voteshare_formula_simple, data =  dat_full_3count_pooled, weights = dat_full_3count_pooled$gs)
model_voteshare_3count_fullsample_simple$clusterse <- runClusterRobustOLS(model_voteshare_3count_fullsample_simple, dat_full_3count_pooled$lfnr)
summary(model_voteshare_3count_fullsample_simple)

model_voteshare_1count_matched_simple <- lm(model_voteshare_formula_simple, data =  dat_match_1count_pooled, weights = dat_match_1count_pooled$gs)
model_voteshare_1count_matched_simple$clusterse <- runClusterRobustOLS(model_voteshare_1count_matched_simple, dat_match_1count_pooled$lfnr)
summary(model_voteshare_1count_matched_simple)

model_voteshare_2count_matched_simple <- lm(model_voteshare_formula_simple, data =  dat_match_2count_pooled, weights = dat_match_2count_pooled$gs)
model_voteshare_2count_matched_simple$clusterse <- runClusterRobustOLS(model_voteshare_2count_matched_simple, dat_match_2count_pooled$lfnr)
summary(model_voteshare_2count_matched_simple)

model_voteshare_3count_matched_simple <- lm(model_voteshare_formula_simple, data =  dat_match_3count_pooled, weights = dat_match_3count_pooled$gs)
model_voteshare_3count_matched_simple$clusterse <- runClusterRobustOLS(model_voteshare_3count_matched_simple, dat_match_3count_pooled$lfnr)
summary(model_voteshare_3count_matched_simple)


# table ------
model_voteshare_list <- list(model_voteshare_1count_fullsample_simple, model_voteshare_1count_matched_simple, 
                             model_voteshare_2count_fullsample_simple, model_voteshare_2count_matched_simple, 
                             model_voteshare_3count_fullsample_simple, model_voteshare_3count_matched_simple)
model_voteshare_ses <- list(model_voteshare_1count_fullsample_simple$clusterse[,2], model_voteshare_1count_matched_simple$clusterse[,2],
                            model_voteshare_2count_fullsample_simple$clusterse[,2], model_voteshare_2count_matched_simple$clusterse[,2],
                            model_voteshare_3count_fullsample_simple$clusterse[,2], model_voteshare_3count_matched_simple$clusterse[,2])
obs <- c(length(model_voteshare_1count_fullsample_simple$residuals)/2,
         length(model_voteshare_1count_matched_simple$residuals)/2,
         length(model_voteshare_2count_fullsample_simple$residuals)/2,
         length(model_voteshare_2count_matched_simple$residuals)/2,
         length(model_voteshare_3count_fullsample_simple$residuals)/2,
         length(model_voteshare_3count_matched_simple$residuals)/2)
varnames_long <- c("\\cmidrule(r){2-3}\\cmidrule(r){4-5}\\cmidrule(r){6-7}Number of visits:", "(Intercept)")
election_names <- c("One visit ", "Two visits", "Three or more visits")
effect_models_tex <- stargazer(model_voteshare_list, 
                               dep.var.caption = "", #dep.var.caption = "Hitler vote share", 
                               omit.table.layout = "d",
                               covariate.labels = varnames_long, 
                               model.numbers = FALSE, 
                               keep=c("Constant", "^timeXexposure"),
                               omit.stat = c("rsq", "res.dev", "ser", "n", "f"),
                               no.space = TRUE,
                               df = FALSE, 
                               se = model_voteshare_ses,
                               add.lines = list(c("Sample", rep(c("full", "matched"), 3)),
                                                c("Observations", obs)),
                               notes        = "Clustered SEs shown. $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01", 
                               notes.append = FALSE,
                               column.labels = election_names, 
                               column.separate = c(2, 2, 2), 
                               font.size = "scriptsize",
                               title = paste0("Diff-in-diff-in-diff estimates of exposure effects on NSDAP vote share with varying number of visits specifications (election-pair fixed effects included).\\vspace{-.25cm}"), 
                               label = paste0("tab:nsdap-voteshare-numvisits-dd"),
                               out = paste0("../figures/tab-effect-voteshare-models-numvisits.tex"))


