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
c("../figures/tab-effect-voteshare-models-radius.tex",
  "../figures/tab-effect-voteshare-kpd-models-radius.tex",
  "../figures/tab-effect-turnout-models-radius.tex",
  "../figures/effects_coefplot_municip_radius.pdf")


## load packages and functions -------------------
source("packages.r")
source("functions.r")
source("functions-analysis.r")


## import prepared data --------------------------
load("county_df_pointdata_long.RData")
load("county_pres_df_pointdata_long.RData")
load("community_df_pointdata_long.RData")



### geographic scope of events -----------------------------

# model NSDAP vote share, 1930 election, municipal data
model_voteshare_comm_formula_simple <- paste("p_nsdap ~ time + exposure + timeXexposure",sep = "") %>% as.formula
# 5km
dat_full <- getDatFull("community_df_long", election = 2, treatment = "visit_5km")
model_voteshare_comm_fullsample_simple_5km <- lm(model_voteshare_comm_formula_simple, data =  dat_full, weights = dat_full$gs)
model_voteshare_comm_fullsample_simple_5km$clusterse <- runClusterRobustOLS(model_voteshare_comm_fullsample_simple_5km, dat_full$lfnr)
dat_raw <- community_df_long[community_df_long$election == 2,]
dat_match <- performMatch("visit_5km", dat.raw = "dat_raw", electiontype = "community", nomatch.radius.adapt = TRUE)
dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "community_df_long", election = 2, treatment = "visit_5km")
model_voteshare_comm_matched_simple_5km <- lm(model_voteshare_comm_formula_simple, data =  dat_match, weights = dat_match$gs)
model_voteshare_comm_matched_simple_5km$clusterse <- runClusterRobustOLS(model_voteshare_comm_matched_simple_5km, dat_match$lfnr)
# 10km
dat_full <- getDatFull("community_df_long", election = 2, treatment = "visit_10km")
model_voteshare_comm_fullsample_simple_10km <- lm(model_voteshare_comm_formula_simple, data =  dat_full, weights = dat_full$gs)
model_voteshare_comm_fullsample_simple_10km$clusterse <- runClusterRobustOLS(model_voteshare_comm_fullsample_simple_10km, dat_full$lfnr)
dat_raw <- community_df_long[community_df_long$election == 2,]
dat_match <- performMatch("visit_10km", dat.raw = "dat_raw", electiontype = "community", nomatch.radius.adapt = TRUE)
dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "community_df_long", election = 2, treatment = "visit_10km")
model_voteshare_comm_matched_simple_10km <- lm(model_voteshare_comm_formula_simple, data =  dat_match, weights = dat_match$gs)
model_voteshare_comm_matched_simple_10km$clusterse <- runClusterRobustOLS(model_voteshare_comm_matched_simple_10km, dat_match$lfnr)
# 25km
dat_full <- getDatFull("community_df_long", election = 2, treatment = "visit_25km")
model_voteshare_comm_fullsample_simple_25km <- lm(model_voteshare_comm_formula_simple, data =  dat_full, weights = dat_full$gs)
model_voteshare_comm_fullsample_simple_25km$clusterse <- runClusterRobustOLS(model_voteshare_comm_fullsample_simple_25km, dat_full$lfnr)
dat_raw <- community_df_long[community_df_long$election == 2,]
dat_match <- performMatch("visit_25km", dat.raw = "dat_raw", electiontype = "community", nomatch.radius.adapt = TRUE)
dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "community_df_long", election = 2, treatment = "visit_25km")
model_voteshare_comm_matched_simple_25km <- lm(model_voteshare_comm_formula_simple, data =  dat_match, weights = dat_match$gs)
model_voteshare_comm_matched_simple_25km$clusterse <- runClusterRobustOLS(model_voteshare_comm_matched_simple_25km, dat_match$lfnr)
# 50km
dat_full <- getDatFull("community_df_long", election = 2, treatment = "visit_50km")
model_voteshare_comm_fullsample_simple_50km <- lm(model_voteshare_comm_formula_simple, data =  dat_full, weights = dat_full$gs)
model_voteshare_comm_fullsample_simple_50km$clusterse <- runClusterRobustOLS(model_voteshare_comm_fullsample_simple_50km, dat_full$lfnr)
dat_raw <- community_df_long[community_df_long$election == 2,]
dat_match <- performMatch("visit_50km", dat.raw = "dat_raw", electiontype = "community", nomatch.radius.adapt = TRUE)
dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "community_df_long", election = 2, treatment = "visit_50km")
model_voteshare_comm_matched_simple_50km <- lm(model_voteshare_comm_formula_simple, data =  dat_match, weights = dat_match$gs)
model_voteshare_comm_matched_simple_50km$clusterse <- runClusterRobustOLS(model_voteshare_comm_matched_simple_50km, dat_match$lfnr)


# model KPD vote share, 1930 election, municipal data
model_voteshare_kpd_comm_formula_simple <- paste("p_kpd ~ time + exposure + timeXexposure",sep = "") %>% as.formula
# 5km
dat_full <- getDatFull("community_df_long", election = 2, treatment = "visit_5km")
model_voteshare_kpd_comm_fullsample_simple_5km <- lm(model_voteshare_kpd_comm_formula_simple, data =  dat_full, weights = dat_full$gs)
model_voteshare_kpd_comm_fullsample_simple_5km$clusterse <- runClusterRobustOLS(model_voteshare_kpd_comm_fullsample_simple_5km, dat_full$lfnr)
dat_raw <- community_df_long[community_df_long$election == 2,]
dat_match <- performMatch("visit_5km", dat.raw = "dat_raw", electiontype = "community", nomatch.radius.adapt = TRUE)
dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "community_df_long", election = 2, treatment = "visit_5km")
model_voteshare_kpd_comm_matched_simple_5km <- lm(model_voteshare_kpd_comm_formula_simple, data =  dat_match, weights = dat_match$gs)
model_voteshare_kpd_comm_matched_simple_5km$clusterse <- runClusterRobustOLS(model_voteshare_kpd_comm_matched_simple_5km, dat_match$lfnr)
# 10km
dat_full <- getDatFull("community_df_long", election = 2, treatment = "visit_10km")
model_voteshare_kpd_comm_fullsample_simple_10km <- lm(model_voteshare_kpd_comm_formula_simple, data =  dat_full, weights = dat_full$gs)
model_voteshare_kpd_comm_fullsample_simple_10km$clusterse <- runClusterRobustOLS(model_voteshare_kpd_comm_fullsample_simple_10km, dat_full$lfnr)
dat_raw <- community_df_long[community_df_long$election == 2,]
dat_match <- performMatch("visit_10km", dat.raw = "dat_raw", electiontype = "community", nomatch.radius.adapt = TRUE)
dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "community_df_long", election = 2, treatment = "visit_10km")
model_voteshare_kpd_comm_matched_simple_10km <- lm(model_voteshare_kpd_comm_formula_simple, data =  dat_match, weights = dat_match$gs)
model_voteshare_kpd_comm_matched_simple_10km$clusterse <- runClusterRobustOLS(model_voteshare_kpd_comm_matched_simple_10km, dat_match$lfnr)
# 25km
dat_full <- getDatFull("community_df_long", election = 2, treatment = "visit_25km")
model_voteshare_kpd_comm_fullsample_simple_25km <- lm(model_voteshare_kpd_comm_formula_simple, data =  dat_full, weights = dat_full$gs)
model_voteshare_kpd_comm_fullsample_simple_25km$clusterse <- runClusterRobustOLS(model_voteshare_kpd_comm_fullsample_simple_25km, dat_full$lfnr)
dat_raw <- community_df_long[community_df_long$election == 2,]
dat_match <- performMatch("visit_25km", dat.raw = "dat_raw", electiontype = "community", nomatch.radius.adapt = TRUE)
dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "community_df_long", election = 2, treatment = "visit_25km")
model_voteshare_kpd_comm_matched_simple_25km <- lm(model_voteshare_kpd_comm_formula_simple, data =  dat_match, weights = dat_match$gs)
model_voteshare_kpd_comm_matched_simple_25km$clusterse <- runClusterRobustOLS(model_voteshare_kpd_comm_matched_simple_25km, dat_match$lfnr)
# 50km
dat_full <- getDatFull("community_df_long", election = 2, treatment = "visit_50km")
model_voteshare_kpd_comm_fullsample_simple_50km <- lm(model_voteshare_kpd_comm_formula_simple, data =  dat_full, weights = dat_full$gs)
model_voteshare_kpd_comm_fullsample_simple_50km$clusterse <- runClusterRobustOLS(model_voteshare_kpd_comm_fullsample_simple_50km, dat_full$lfnr)
dat_raw <- community_df_long[community_df_long$election == 2,]
dat_match <- performMatch("visit_50km", dat.raw = "dat_raw", electiontype = "community", nomatch.radius.adapt = TRUE)
dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "community_df_long", election = 2, treatment = "visit_50km")
model_voteshare_kpd_comm_matched_simple_50km <- lm(model_voteshare_kpd_comm_formula_simple, data =  dat_match, weights = dat_match$gs)
model_voteshare_kpd_comm_matched_simple_50km$clusterse <- runClusterRobustOLS(model_voteshare_kpd_comm_matched_simple_50km, dat_match$lfnr)


# model turnout, 1930 election, municipal data
model_turnout_comm_formula_simple <- paste("p_turnout ~ time + exposure + timeXexposure",sep = "") %>% as.formula
# 5km
dat_full <- getDatFull("community_df_long", election = 2, treatment = "visit_5km")
model_turnout_comm_fullsample_simple_5km <- lm(model_turnout_comm_formula_simple, data =  dat_full, weights = dat_full$wbht)
model_turnout_comm_fullsample_simple_5km$clusterse <- runClusterRobustOLS(model_turnout_comm_fullsample_simple_5km, dat_full$lfnr)
dat_raw <- community_df_long[community_df_long$election == 2,]
dat_match <- performMatch("visit_5km", dat.raw = "dat_raw", electiontype = "community", nomatch.radius.adapt = TRUE)
dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "community_df_long", election = 2, treatment = "visit_5km")
model_turnout_comm_matched_simple_5km <- lm(model_turnout_comm_formula_simple, data =  dat_match, weights = dat_match$wbht)
model_turnout_comm_matched_simple_5km$clusterse <- runClusterRobustOLS(model_turnout_comm_matched_simple_5km, dat_match$lfnr)
# 10km
dat_full <- getDatFull("community_df_long", election = 2, treatment = "visit_10km")
model_turnout_comm_fullsample_simple_10km <- lm(model_turnout_comm_formula_simple, data =  dat_full, weights = dat_full$wbht)
model_turnout_comm_fullsample_simple_10km$clusterse <- runClusterRobustOLS(model_turnout_comm_fullsample_simple_10km, dat_full$lfnr)
dat_raw <- community_df_long[community_df_long$election == 2,]
dat_match <- performMatch("visit_10km", dat.raw = "dat_raw", electiontype = "community", nomatch.radius.adapt = TRUE)
dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "community_df_long", election = 2, treatment = "visit_10km")
model_turnout_comm_matched_simple_10km <- lm(model_turnout_comm_formula_simple, data =  dat_match, weights = dat_match$wbht)
model_turnout_comm_matched_simple_10km$clusterse <- runClusterRobustOLS(model_turnout_comm_matched_simple_10km, dat_match$lfnr)
# 25km
dat_full <- getDatFull("community_df_long", election = 2, treatment = "visit_25km")
model_turnout_comm_fullsample_simple_25km <- lm(model_turnout_comm_formula_simple, data =  dat_full, weights = dat_full$wbht)
model_turnout_comm_fullsample_simple_25km$clusterse <- runClusterRobustOLS(model_turnout_comm_fullsample_simple_25km, dat_full$lfnr)
dat_raw <- community_df_long[community_df_long$election == 2,]
dat_match <- performMatch("visit_25km", dat.raw = "dat_raw", electiontype = "community", nomatch.radius.adapt = TRUE)
dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "community_df_long", election = 2, treatment = "visit_25km")
model_turnout_comm_matched_simple_25km <- lm(model_turnout_comm_formula_simple, data =  dat_match, weights = dat_match$wbht)
model_turnout_comm_matched_simple_25km$clusterse <- runClusterRobustOLS(model_turnout_comm_matched_simple_25km, dat_match$lfnr)
# 50km
dat_full <- getDatFull("community_df_long", election = 2, treatment = "visit_50km")
model_turnout_comm_fullsample_simple_50km <- lm(model_turnout_comm_formula_simple, data =  dat_full, weights = dat_full$wbht)
model_turnout_comm_fullsample_simple_50km$clusterse <- runClusterRobustOLS(model_turnout_comm_fullsample_simple_50km, dat_full$lfnr)
dat_raw <- community_df_long[community_df_long$election == 2,]
dat_match <- performMatch("visit_50km", dat.raw = "dat_raw", electiontype = "community", nomatch.radius.adapt = TRUE)
dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "community_df_long", election = 2, treatment = "visit_50km")
model_turnout_comm_matched_simple_50km <- lm(model_turnout_comm_formula_simple, data =  dat_match, weights = dat_match$wbht)
model_turnout_comm_matched_simple_50km$clusterse <- runClusterRobustOLS(model_turnout_comm_matched_simple_50km, dat_match$lfnr)


## tables -----

# NSDAP vote share models, 1930 election, varying radiuses
model_voteshare_list <- list(model_voteshare_comm_fullsample_simple_5km, model_voteshare_comm_matched_simple_5km, 
                             model_voteshare_comm_fullsample_simple_10km, model_voteshare_comm_matched_simple_10km, 
                             model_voteshare_comm_fullsample_simple_25km, model_voteshare_comm_matched_simple_25km, 
                             model_voteshare_comm_fullsample_simple_50km, model_voteshare_comm_matched_simple_50km)
model_voteshare_ses <- list(model_voteshare_comm_fullsample_simple_5km$clusterse[,2], model_voteshare_comm_matched_simple_5km$clusterse[,2],
                            model_voteshare_comm_fullsample_simple_10km$clusterse[,2], model_voteshare_comm_matched_simple_10km$clusterse[,2],
                            model_voteshare_comm_fullsample_simple_25km$clusterse[,2], model_voteshare_comm_matched_simple_25km$clusterse[,2],
                            model_voteshare_comm_fullsample_simple_50km$clusterse[,2], model_voteshare_comm_matched_simple_50km$clusterse[,2])
obs <- c(length(model_voteshare_comm_fullsample_simple_5km$residuals)/2,
         length(model_voteshare_comm_matched_simple_5km$residuals)/2,
         length(model_voteshare_comm_fullsample_simple_10km$residuals)/2,
         length(model_voteshare_comm_matched_simple_10km$residuals)/2,
         length(model_voteshare_comm_fullsample_simple_25km$residuals)/2,
         length(model_voteshare_comm_matched_simple_25km$residuals)/2,
         length(model_voteshare_comm_fullsample_simple_50km$residuals)/2,
         length(model_voteshare_comm_matched_simple_50km$residuals)/2)
varnames_long <- c("\\cmidrule(r){2-3}\\cmidrule(r){4-5}\\cmidrule(r){6-7}\\cmidrule(r){8-9}Exposure, radius of...", "(Intercept)")
election_names <- c("... 5km", "... 10km", "... 25km", "... 50km")
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
                               title = paste0("Diff-in-diff estimates of exposure effects on NSDAP vote share at the 1930 national parliamentary election using community-level data with varying radius of exposure specifications.\\vspace{-.25cm}"), 
                               label = paste0("tab:nsdap-voteshare-radius-dd"),
                               out = paste0("../figures/tab-effect-voteshare-models-radius.tex"))

# KPD vote share models, 1930 election, varying radiuses
model_voteshare_kpd_list <- list(model_voteshare_kpd_comm_fullsample_simple_5km, model_voteshare_kpd_comm_matched_simple_5km, 
                             model_voteshare_kpd_comm_fullsample_simple_10km, model_voteshare_kpd_comm_matched_simple_10km, 
                             model_voteshare_kpd_comm_fullsample_simple_25km, model_voteshare_kpd_comm_matched_simple_25km, 
                             model_voteshare_kpd_comm_fullsample_simple_50km, model_voteshare_kpd_comm_matched_simple_50km)
model_voteshare_kpd_ses <- list(model_voteshare_kpd_comm_fullsample_simple_5km$clusterse[,2], model_voteshare_kpd_comm_matched_simple_5km$clusterse[,2],
                            model_voteshare_kpd_comm_fullsample_simple_10km$clusterse[,2], model_voteshare_kpd_comm_matched_simple_10km$clusterse[,2],
                            model_voteshare_kpd_comm_fullsample_simple_25km$clusterse[,2], model_voteshare_kpd_comm_matched_simple_25km$clusterse[,2],
                            model_voteshare_kpd_comm_fullsample_simple_50km$clusterse[,2], model_voteshare_kpd_comm_matched_simple_50km$clusterse[,2])
obs <- c(length(model_voteshare_kpd_comm_fullsample_simple_5km$residuals)/2,
         length(model_voteshare_kpd_comm_matched_simple_5km$residuals)/2,
         length(model_voteshare_kpd_comm_fullsample_simple_10km$residuals)/2,
         length(model_voteshare_kpd_comm_matched_simple_10km$residuals)/2,
         length(model_voteshare_kpd_comm_fullsample_simple_25km$residuals)/2,
         length(model_voteshare_kpd_comm_matched_simple_25km$residuals)/2,
         length(model_voteshare_kpd_comm_fullsample_simple_50km$residuals)/2,
         length(model_voteshare_kpd_comm_matched_simple_50km$residuals)/2)
varnames_long <- c("\\cmidrule(r){2-3}\\cmidrule(r){4-5}\\cmidrule(r){6-7}\\cmidrule(r){8-9}Exposure, radius of...", "(Intercept)")
election_names <- c("... 5km", "... 10km", "... 25km", "... 50km")
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
                               title = paste0("Diff-in-diff estimates of exposure effects on KPD vote share at the 1930 national parliamentary election using community-level data with varying radius of exposure specifications.\\vspace{-.25cm}"), 
                               label = paste0("tab:voteshare-kpd-radius-dd"),
                               out = paste0("../figures/tab-effect-voteshare-kpd-models-radius.tex"))

# turnout models, 1930 election, varying radiuses
model_turnout_list <- list(model_turnout_comm_fullsample_simple_5km, model_turnout_comm_matched_simple_5km, 
                             model_turnout_comm_fullsample_simple_10km, model_turnout_comm_matched_simple_10km, 
                             model_turnout_comm_fullsample_simple_25km, model_turnout_comm_matched_simple_25km, 
                             model_turnout_comm_fullsample_simple_50km, model_turnout_comm_matched_simple_50km)
model_turnout_ses <- list(model_turnout_comm_fullsample_simple_5km$clusterse[,2], model_turnout_comm_matched_simple_5km$clusterse[,2],
                            model_turnout_comm_fullsample_simple_10km$clusterse[,2], model_turnout_comm_matched_simple_10km$clusterse[,2],
                            model_turnout_comm_fullsample_simple_25km$clusterse[,2], model_turnout_comm_matched_simple_25km$clusterse[,2],
                            model_turnout_comm_fullsample_simple_50km$clusterse[,2], model_turnout_comm_matched_simple_50km$clusterse[,2])
obs <- c(length(model_turnout_comm_fullsample_simple_5km$residuals)/2,
         length(model_turnout_comm_matched_simple_5km$residuals)/2,
         length(model_turnout_comm_fullsample_simple_10km$residuals)/2,
         length(model_turnout_comm_matched_simple_10km$residuals)/2,
         length(model_turnout_comm_fullsample_simple_25km$residuals)/2,
         length(model_turnout_comm_matched_simple_25km$residuals)/2,
         length(model_turnout_comm_fullsample_simple_50km$residuals)/2,
         length(model_turnout_comm_matched_simple_50km$residuals)/2)
varnames_long <- c("\\cmidrule(r){2-3}\\cmidrule(r){4-5}\\cmidrule(r){6-7}\\cmidrule(r){8-9}Exposure, radius of...", "(Intercept)")
election_names <- c("... 5km", "... 10km", "... 25km", "... 50km")
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
                               title = paste0("Diff-in-diff estimates of exposure effects on turnout at the 1930 national parliamentary election using community-level data with varying radius of exposure specifications.\\vspace{-.25cm}"), 
                               label = paste0("tab:turnout-radius-dd"),
                               out = paste0("../figures/tab-effect-turnout-models-radius.tex"))


## plot -----
pdf(file="../figures/effects_coefplot_municip_radius.pdf", height=3, width=8, family="URWTimes")
par(oma=c(.5,0,.5,0))
par(mar=c(3,5,3,1))
layout(matrix(1:3, 1, byrow = TRUE), widths=c(1.25, 1, 1), heights=c(2, 2, 2))
ylabel_names <- rev(c("5km", "10km", "25km", "50km"))
colors <- brewer.pal(4,"BrBG")
colors <- c("darkgrey", "black")
## voteshare effect plot
plot(0, 0, xlim = c(-0.07, .07), ylim = c(-.3, 3.3), pch = 20, col = "white", xlab = "", ylab = "", yaxt = "n", xaxt = "n")
axis(2, at = 0:3, labels = F, tick = F)
text(y = 0:3, par("usr")[1], labels = ylabel_names, srt = 0, pos = 2, xpd = TRUE)
axis(1, at = seq(-.06, .06, .03), labels = seq(-.06, .06, .03), tick = T)
mtext("NSDAP vote share", side = 3, line = 1.2, outer = FALSE)
abline(v = 0, lty = 2)
abline(h = c(.5, 1.5, 2.5, 3.5), lty = 1, col = "darkgrey")
# 5km
y_par <- 3
mod_input <- model_voteshare_comm_fullsample_simple_5km
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.15, x1 = mod$coef_hardhi, y1 = y_par+.15, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.15, x1 = mod$coef_weakhi, y1 = y_par+.15, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.15, pch = 20, col = colors[1])
mod_input <- model_voteshare_comm_matched_simple_5km
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.15, x1 = mod$coef_hardhi, y1 = y_par-.15, length = 0, col = colors[2])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.15, x1 = mod$coef_weakhi, y1 = y_par-.15, length = 0, lwd = 2, col = colors[2])
points(mod$coef_point, y_par-.15, pch = 20, col = colors[2])
# 10km
y_par <- 2
mod_input <- model_voteshare_comm_fullsample_simple_10km
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.15, x1 = mod$coef_hardhi, y1 = y_par+.15, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.15, x1 = mod$coef_weakhi, y1 = y_par+.15, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.15, pch = 20, col = colors[1])
mod_input <- model_voteshare_comm_matched_simple_10km
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.15, x1 = mod$coef_hardhi, y1 = y_par-.15, length = 0, col = colors[2])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.15, x1 = mod$coef_weakhi, y1 = y_par-.15, length = 0, lwd = 2, col = colors[2])
points(mod$coef_point, y_par-.15, pch = 20, col = colors[2])
# 25km
y_par <- 1
mod_input <- model_voteshare_comm_fullsample_simple_25km
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.15, x1 = mod$coef_hardhi, y1 = y_par+.15, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.15, x1 = mod$coef_weakhi, y1 = y_par+.15, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.15, pch = 20, col = colors[1])
mod_input <- model_voteshare_comm_matched_simple_25km
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.15, x1 = mod$coef_hardhi, y1 = y_par-.15, length = 0, col = colors[2])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.15, x1 = mod$coef_weakhi, y1 = y_par-.15, length = 0, lwd = 2, col = colors[2])
points(mod$coef_point, y_par-.15, pch = 20, col = colors[2])
# 50km
y_par <- 0
mod_input <- model_voteshare_comm_fullsample_simple_50km
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.15, x1 = mod$coef_hardhi, y1 = y_par+.15, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.15, x1 = mod$coef_weakhi, y1 = y_par+.15, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.15, pch = 20, col = colors[1])
mod_input <- model_voteshare_comm_matched_simple_50km
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.15, x1 = mod$coef_hardhi, y1 = y_par-.15, length = 0, col = colors[2])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.15, x1 = mod$coef_weakhi, y1 = y_par-.15, length = 0, lwd = 2, col = colors[2])
points(mod$coef_point, y_par-.15, pch = 20, col = colors[2])

## kpd voteshare effect plot
par(mar=c(3,0,3,1))
plot(0, 0, xlim = c(-0.07, .07), ylim = c(-.3, 3.3), pch = 20, col = "white", xlab = "", ylab = "", yaxt = "n", xaxt = "n")
axis(1, at = seq(-.06, .06, .03), labels = seq(-.06, .06, .03), tick = T)
mtext("KPD vote share", side = 3, line = 1.2, outer = FALSE)
abline(v = 0, lty = 2)
abline(h = c(.5, 1.5, 2.5, 3.5), lty = 1, col = "darkgrey")
# 5km
y_par <- 3
mod_input <- model_voteshare_kpd_comm_fullsample_simple_5km
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.15, x1 = mod$coef_hardhi, y1 = y_par+.15, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.15, x1 = mod$coef_weakhi, y1 = y_par+.15, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.15, pch = 20, col = colors[1])
mod_input <- model_voteshare_kpd_comm_matched_simple_5km
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.15, x1 = mod$coef_hardhi, y1 = y_par-.15, length = 0, col = colors[2])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.15, x1 = mod$coef_weakhi, y1 = y_par-.15, length = 0, lwd = 2, col = colors[2])
points(mod$coef_point, y_par-.15, pch = 20, col = colors[2])
# 10km
y_par <- 2
mod_input <- model_voteshare_kpd_comm_fullsample_simple_10km
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.15, x1 = mod$coef_hardhi, y1 = y_par+.15, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.15, x1 = mod$coef_weakhi, y1 = y_par+.15, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.15, pch = 20, col = colors[1])
mod_input <- model_voteshare_kpd_comm_matched_simple_10km
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.15, x1 = mod$coef_hardhi, y1 = y_par-.15, length = 0, col = colors[2])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.15, x1 = mod$coef_weakhi, y1 = y_par-.15, length = 0, lwd = 2, col = colors[2])
points(mod$coef_point, y_par-.15, pch = 20, col = colors[2])
# 25km
y_par <- 1
mod_input <- model_voteshare_kpd_comm_fullsample_simple_25km
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.15, x1 = mod$coef_hardhi, y1 = y_par+.15, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.15, x1 = mod$coef_weakhi, y1 = y_par+.15, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.15, pch = 20, col = colors[1])
mod_input <- model_voteshare_kpd_comm_matched_simple_25km
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.15, x1 = mod$coef_hardhi, y1 = y_par-.15, length = 0, col = colors[2])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.15, x1 = mod$coef_weakhi, y1 = y_par-.15, length = 0, lwd = 2, col = colors[2])
points(mod$coef_point, y_par-.15, pch = 20, col = colors[2])
# 50km
y_par <- 0
mod_input <- model_voteshare_kpd_comm_fullsample_simple_50km
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.15, x1 = mod$coef_hardhi, y1 = y_par+.15, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.15, x1 = mod$coef_weakhi, y1 = y_par+.15, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.15, pch = 20, col = colors[1])
mod_input <- model_voteshare_kpd_comm_matched_simple_50km
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.15, x1 = mod$coef_hardhi, y1 = y_par-.15, length = 0, col = colors[2])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.15, x1 = mod$coef_weakhi, y1 = y_par-.15, length = 0, lwd = 2, col = colors[2])
points(mod$coef_point, y_par-.15, pch = 20, col = colors[2])
text(.07, y_par+.15, labels = "full sample", col = colors[1], pos = 2)
text(.07, y_par-.15, labels = "matched sample", col = colors[2], pos = 2)
## turnout effect plot
par(mar=c(3,0,3,1))
plot(0, 0, xlim = c(-0.07, .07), ylim = c(-.3, 3.3), pch = 20, col = "white", xlab = "", ylab = "", yaxt = "n", xaxt = "n")
axis(1, at = seq(-.06, .06, .03), labels = seq(-.06, .06, .03), tick = T)
mtext("Turnout", side = 3, line = 1.2, outer = FALSE)
abline(v = 0, lty = 2)
abline(h = c(.5, 1.5, 2.5, 3.5), lty = 1, col = "darkgrey")
# 5km
y_par <- 3
mod_input <- model_turnout_comm_fullsample_simple_5km
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.15, x1 = mod$coef_hardhi, y1 = y_par+.15, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.15, x1 = mod$coef_weakhi, y1 = y_par+.15, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.15, pch = 20, col = colors[1])
mod_input <- model_turnout_comm_matched_simple_5km
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.15, x1 = mod$coef_hardhi, y1 = y_par-.15, length = 0, col = colors[2])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.15, x1 = mod$coef_weakhi, y1 = y_par-.15, length = 0, lwd = 2, col = colors[2])
points(mod$coef_point, y_par-.15, pch = 20, col = colors[2])
# 10km
y_par <- 2
mod_input <- model_turnout_comm_fullsample_simple_10km
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.15, x1 = mod$coef_hardhi, y1 = y_par+.15, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.15, x1 = mod$coef_weakhi, y1 = y_par+.15, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.15, pch = 20, col = colors[1])
mod_input <- model_turnout_comm_matched_simple_10km
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.15, x1 = mod$coef_hardhi, y1 = y_par-.15, length = 0, col = colors[2])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.15, x1 = mod$coef_weakhi, y1 = y_par-.15, length = 0, lwd = 2, col = colors[2])
points(mod$coef_point, y_par-.15, pch = 20, col = colors[2])
# 25km
y_par <- 1
mod_input <- model_turnout_comm_fullsample_simple_25km
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.15, x1 = mod$coef_hardhi, y1 = y_par+.15, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.15, x1 = mod$coef_weakhi, y1 = y_par+.15, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.15, pch = 20, col = colors[1])
mod_input <- model_turnout_comm_matched_simple_25km
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.15, x1 = mod$coef_hardhi, y1 = y_par-.15, length = 0, col = colors[2])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.15, x1 = mod$coef_weakhi, y1 = y_par-.15, length = 0, lwd = 2, col = colors[2])
points(mod$coef_point, y_par-.15, pch = 20, col = colors[2])
# 50km
y_par <- 0
mod_input <- model_turnout_comm_fullsample_simple_50km
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.15, x1 = mod$coef_hardhi, y1 = y_par+.15, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.15, x1 = mod$coef_weakhi, y1 = y_par+.15, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.15, pch = 20, col = colors[1])
mod_input <- model_turnout_comm_matched_simple_50km
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.15, x1 = mod$coef_hardhi, y1 = y_par-.15, length = 0, col = colors[2])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.15, x1 = mod$coef_weakhi, y1 = y_par-.15, length = 0, lwd = 2, col = colors[2])
points(mod$coef_point, y_par-.15, pch = 20, col = colors[2])
dev.off()





