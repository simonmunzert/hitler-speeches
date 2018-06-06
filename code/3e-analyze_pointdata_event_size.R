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
c("../figures/tab-effect-voteshare-models-numvisitors.tex")


## load packages and functions -------------------
source("packages.r")
source("functions.r")
source("functions-analysis.r")


## import prepared data --------------------------
load("county_df_pointdata_long.RData")
load("county_pres_df_pointdata_long.RData")
load("community_df_pointdata_long.RData")


## event size -----------------------------

# overview
county_df_long$esizecut <- cut(county_df_long$esize_10km, breaks = c(0, 1, 5000, 20000, max(county_df_long$esize_10km, na.rm = TRUE)), include.lowest = TRUE, labels = c("N exposed unknown", "N exposed < 5000", "5000 <= N exposed < 20000", "N exposed >= 20000"))
table(county_df_long$esizecut, county_df_long$visit_10km)

# define new exposure variables
county_df_long$visit_10km_1size <- county_df_long$esize_10km == 0 & county_df_long$visit_10km == 1
county_df_long$visit_10km_2size <- county_df_long$esize_10km > 0 & county_df_long$esize_10km <= 5000
county_df_long$visit_10km_3size <- county_df_long$esize_10km > 5000 & county_df_long$esize_10km <= 20000
county_df_long$visit_10km_4size <- county_df_long$esize_10km > 20000
# county_df_long$esize_fraq_10km <- county_df_long$esize_10km / county_df_long$wb
# county_df_long$visit_10km_1size <- county_df_long$esize_fraq_10km == 0 & county_df_long$visit_10km == 1
# county_df_long$visit_10km_2size <- county_df_long$esize_fraq_10km > 0 & county_df_long$esize_fraq_10km <= .3
# county_df_long$visit_10km_3size <- county_df_long$esize_fraq_10km > .3 & county_df_long$esize_fraq_10km <= .6
# county_df_long$visit_10km_4size <- county_df_long$esize_fraq_10km > .6
county_df_long$visit_10km_1size_nomatch <- county_df_long$visit_10km_nomatch
county_df_long$visit_10km_2size_nomatch <- county_df_long$visit_10km_nomatch
county_df_long$visit_10km_3size_nomatch <- county_df_long$visit_10km_nomatch
county_df_long$visit_10km_4size_nomatch <- county_df_long$visit_10km_nomatch


# generate separate matching data frames
dat_match_1size_list <- list()
dat_match_2size_list <- list()
dat_match_3size_list <- list()
dat_match_4size_list <- list()
dat_full_1size_list <- list()
dat_full_2size_list <- list()
dat_full_3size_list <- list()
dat_full_4size_list <- list()
for (i in 1:4) {
  # prepare data
  dat_raw <- county_df_long[county_df_long$election == i+1,]
  dat_raw$no_1size <- dat_raw$visit_10km_1size == 1 | dat_raw$visit_10km == 0
  dat_raw$no_2size <- dat_raw$visit_10km_2size == 1 | dat_raw$visit_10km == 0
  dat_raw$no_3size <- dat_raw$visit_10km_3size == 1 | dat_raw$visit_10km == 0
  dat_raw$no_4size <- dat_raw$visit_10km_4size == 1 | dat_raw$visit_10km == 0
  # full sample, lm simple model with voting population weights (i.e., only number of voters is considered)
  dat_full_1size_list[[i]] <- getDatFull("county_df_long", election = i+1, treatment = "visit_10km_1size")
  dat_full_2size_list[[i]] <- getDatFull("county_df_long", election = i+1, treatment = "visit_10km_2size")
  dat_full_3size_list[[i]] <- getDatFull("county_df_long", election = i+1, treatment = "visit_10km_3size")
  dat_full_4size_list[[i]] <- getDatFull("county_df_long", election = i+1, treatment = "visit_10km_4size")
  # 1:1 matching, lm simple model with voting population weights
  match.data.1size.list <- list()
  match.data.2size.list <- list()
  match.data.3size.list <- list()
  match.data.4size.list <- list()
  try(match.data.1size.list[[i]] <- performMatch("visit_10km_1size", dat.raw = "dat_raw", electiontype = "county", goebbels.adapt = FALSE, exactmatch = "no_1size", treat.var.red = FALSE))
  try(match.data.2size.list[[i]] <- performMatch("visit_10km_2size", dat.raw = "dat_raw", electiontype = "county", goebbels.adapt = FALSE, exactmatch = "no_2size", treat.var.red = FALSE))
  try(match.data.3size.list[[i]] <- performMatch("visit_10km_3size", dat.raw = "dat_raw", electiontype = "county", goebbels.adapt = FALSE, exactmatch = "no_3size", treat.var.red = FALSE))
  try(match.data.4size.list[[i]] <- performMatch("visit_10km_4size", dat.raw = "dat_raw", electiontype = "county", goebbels.adapt = FALSE, exactmatch = "no_4size", treat.var.red = FALSE))
  try(dat_match_1size <- match.data.1size.list[[i]])
  try(dat_match_2size <- match.data.2size.list[[i]])
  try(dat_match_3size <- match.data.3size.list[[i]])
  try(dat_match_4size <- match.data.4size.list[[i]])
  try(dat_match_1size_list[[i]] <- getDatMatch(dat.match = "dat_match_1size", dat.orig = "county_df_long", election = i+1, treatment = "visit_10km_1size"))
  try(dat_match_2size_list[[i]] <- getDatMatch(dat.match = "dat_match_2size", dat.orig = "county_df_long", election = i+1, treatment = "visit_10km_2size"))
  try(dat_match_3size_list[[i]] <- getDatMatch(dat.match = "dat_match_3size", dat.orig = "county_df_long", election = i+1, treatment = "visit_10km_3size"))
  try(dat_match_4size_list[[i]] <- getDatMatch(dat.match = "dat_match_4size", dat.orig = "county_df_long", election = i+1, treatment = "visit_10km_4size"))
}

# collapse data into pooled data frame
dat_full_1size_pooled <- do.call(rbind.fill, dat_full_1size_list)
dat_full_2size_pooled <- do.call(rbind.fill, dat_full_2size_list)
dat_full_3size_pooled <- do.call(rbind.fill, dat_full_3size_list)
dat_full_4size_pooled <- do.call(rbind.fill, dat_full_4size_list)
dat_match_1size_pooled <- do.call(rbind.fill, dat_match_1size_list)
dat_match_2size_pooled <- do.call(rbind.fill, dat_match_2size_list)
dat_match_3size_pooled <- do.call(rbind.fill, dat_match_3size_list)
dat_match_4size_pooled <- do.call(rbind.fill, dat_match_4size_list)


# run model
model_voteshare_formula_simple <- paste("p_nsdap ~ as.factor(election) + time + exposure + timeXexposure",sep = "") %>% as.formula
#model_voteshare_formula_simple <- paste("p_nsdap ~ as.factor(election)*time*exposure",sep = "") %>% as.formula

model_voteshare_1size_fullsample_simple <- lm(model_voteshare_formula_simple, data =  dat_full_1size_pooled, weights = dat_full_1size_pooled$gs)
model_voteshare_1size_fullsample_simple$clusterse <- runClusterRobustOLS(model_voteshare_1size_fullsample_simple, dat_full_1size_pooled$lfnr)
summary(model_voteshare_1size_fullsample_simple)

model_voteshare_2size_fullsample_simple <- lm(model_voteshare_formula_simple, data =  dat_full_2size_pooled, weights = dat_full_2size_pooled$gs)
model_voteshare_2size_fullsample_simple$clusterse <- runClusterRobustOLS(model_voteshare_2size_fullsample_simple, dat_full_2size_pooled$lfnr)
summary(model_voteshare_2size_fullsample_simple)

model_voteshare_3size_fullsample_simple <- lm(model_voteshare_formula_simple, data =  dat_full_3size_pooled, weights = dat_full_3size_pooled$gs)
model_voteshare_3size_fullsample_simple$clusterse <- runClusterRobustOLS(model_voteshare_3size_fullsample_simple, dat_full_3size_pooled$lfnr)
summary(model_voteshare_3size_fullsample_simple)

model_voteshare_4size_fullsample_simple <- lm(model_voteshare_formula_simple, data =  dat_full_4size_pooled, weights = dat_full_4size_pooled$gs)
model_voteshare_4size_fullsample_simple$clusterse <- runClusterRobustOLS(model_voteshare_4size_fullsample_simple, dat_full_4size_pooled$lfnr)
summary(model_voteshare_3size_fullsample_simple)

model_voteshare_1size_matched_simple <- lm(model_voteshare_formula_simple, data =  dat_match_1size_pooled, weights = dat_match_1size_pooled$gs)
model_voteshare_1size_matched_simple$clusterse <- runClusterRobustOLS(model_voteshare_1size_matched_simple, dat_match_1size_pooled$lfnr)
summary(model_voteshare_1size_matched_simple)

model_voteshare_2size_matched_simple <- lm(model_voteshare_formula_simple, data =  dat_match_2size_pooled, weights = dat_match_2size_pooled$gs)
model_voteshare_2size_matched_simple$clusterse <- runClusterRobustOLS(model_voteshare_2size_matched_simple, dat_match_2size_pooled$lfnr)
summary(model_voteshare_2size_matched_simple)

model_voteshare_3size_matched_simple <- lm(model_voteshare_formula_simple, data =  dat_match_3size_pooled, weights = dat_match_3size_pooled$gs)
model_voteshare_3size_matched_simple$clusterse <- runClusterRobustOLS(model_voteshare_3size_matched_simple, dat_match_3size_pooled$lfnr)
summary(model_voteshare_3size_matched_simple)

model_voteshare_4size_matched_simple <- lm(model_voteshare_formula_simple, data =  dat_match_4size_pooled, weights = dat_match_4size_pooled$gs)
model_voteshare_4size_matched_simple$clusterse <- runClusterRobustOLS(model_voteshare_4size_matched_simple, dat_match_4size_pooled$lfnr)
summary(model_voteshare_4size_matched_simple)


# table ------
model_voteshare_list <- list(model_voteshare_1size_fullsample_simple, model_voteshare_1size_matched_simple, 
                             model_voteshare_2size_fullsample_simple, model_voteshare_2size_matched_simple, 
                             model_voteshare_3size_fullsample_simple, model_voteshare_3size_matched_simple,
                             model_voteshare_4size_fullsample_simple, model_voteshare_4size_matched_simple)
model_voteshare_ses <- list(model_voteshare_1size_fullsample_simple$clusterse[,2], model_voteshare_1size_matched_simple$clusterse[,2],
                            model_voteshare_2size_fullsample_simple$clusterse[,2], model_voteshare_2size_matched_simple$clusterse[,2],
                            model_voteshare_3size_fullsample_simple$clusterse[,2], model_voteshare_3size_matched_simple$clusterse[,2],
                            model_voteshare_4size_fullsample_simple$clusterse[,2], model_voteshare_4size_matched_simple$clusterse[,2])
obs <- c(length(model_voteshare_1size_fullsample_simple$residuals)/2,
         length(model_voteshare_1size_matched_simple$residuals)/2,
         length(model_voteshare_2size_fullsample_simple$residuals)/2,
         length(model_voteshare_2size_matched_simple$residuals)/2,
         length(model_voteshare_3size_fullsample_simple$residuals)/2,
         length(model_voteshare_3size_matched_simple$residuals)/2,
         length(model_voteshare_4size_fullsample_simple$residuals)/2,
         length(model_voteshare_4size_matched_simple$residuals)/2)
varnames_long <- c("\\cmidrule(r){2-3}\\cmidrule(r){4-5}\\cmidrule(r){6-7}\\cmidrule(r){8-9}Number of visitors:", "(Intercept)")
election_names <- c("Unknown", "Less than 5,000", "Between 5,000 and 20,000", "20,000 or more")
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
                               add.lines = list(c("Sample", rep(c("full", "matched"), 4)),
                                                c("Observations", obs)),
                               notes        = "DIDID models with number of actual voters as pop weights. Clustered SEs shown. $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01", 
                               notes.append = FALSE,
                               column.labels = election_names, 
                               column.separate = c(2, 2, 2, 2), 
                               font.size = "scriptsize",
                               title = paste0("Diff-in-diff-in-diff estimates of exposure effects on NSDAP vote share with varying number of visitors specifications (election-pair fixed effects included).\\vspace{-.25cm}"), 
                               label = paste0("tab:nsdap-voteshare-numvisitors-dd"),
                               out = paste0("../figures/tab-effect-voteshare-models-numvisitors.tex"))


