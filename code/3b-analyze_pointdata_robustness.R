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



### geographic scope of events -----------------------------

# model NSDAP vote share, 1930 election, municipal data
model_voteshare_comm_formula_simple <- paste("p_nsdap ~ time + exposure + timeXexposure",sep = "") %>% as.formula
# 5km
dat_full <- getDatFull("community_df_long", election = 2, treatment = "visit_5km")
model_voteshare_comm_fullsample_simple_5km <- lm(model_voteshare_comm_formula_simple, data =  dat_full, weights = dat_full$gs)
model_voteshare_comm_fullsample_simple_5km$clusterse <- runClusterRobustOLS(model_voteshare_comm_fullsample_simple_5km, dat_full$lfnr)
dat_raw <- community_df_long[community_df_long$election == 2,]
dat_match <- performMatch("visit_5km", dat.raw = "dat_raw", electiontype = "community")
dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "community_df_long", election = 2, treatment = "visit_5km")
model_voteshare_comm_matched_simple_5km <- lm(model_voteshare_comm_formula_simple, data =  dat_match, weights = dat_match$gs)
model_voteshare_comm_matched_simple_5km$clusterse <- runClusterRobustOLS(model_voteshare_comm_matched_simple_5km, dat_match$lfnr)
# 10km
dat_full <- getDatFull("community_df_long", election = 2, treatment = "visit_10km")
model_voteshare_comm_fullsample_simple_10km <- lm(model_voteshare_comm_formula_simple, data =  dat_full, weights = dat_full$gs)
model_voteshare_comm_fullsample_simple_10km$clusterse <- runClusterRobustOLS(model_voteshare_comm_fullsample_simple_10km, dat_full$lfnr)
dat_raw <- community_df_long[community_df_long$election == 2,]
dat_match <- performMatch("visit_10km", dat.raw = "dat_raw", electiontype = "community")
dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "community_df_long", election = 2, treatment = "visit_10km")
model_voteshare_comm_matched_simple_10km <- lm(model_voteshare_comm_formula_simple, data =  dat_match, weights = dat_match$gs)
model_voteshare_comm_matched_simple_10km$clusterse <- runClusterRobustOLS(model_voteshare_comm_matched_simple_10km, dat_match$lfnr)
# 25km
dat_full <- getDatFull("community_df_long", election = 2, treatment = "visit_25km")
model_voteshare_comm_fullsample_simple_25km <- lm(model_voteshare_comm_formula_simple, data =  dat_full, weights = dat_full$gs)
model_voteshare_comm_fullsample_simple_25km$clusterse <- runClusterRobustOLS(model_voteshare_comm_fullsample_simple_25km, dat_full$lfnr)
dat_raw <- community_df_long[community_df_long$election == 2,]
dat_match <- performMatch("visit_25km", dat.raw = "dat_raw", electiontype = "community")
dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "community_df_long", election = 2, treatment = "visit_25km")
model_voteshare_comm_matched_simple_25km <- lm(model_voteshare_comm_formula_simple, data =  dat_match, weights = dat_match$gs)
model_voteshare_comm_matched_simple_25km$clusterse <- runClusterRobustOLS(model_voteshare_comm_matched_simple_25km, dat_match$lfnr)
# 50km
dat_full <- getDatFull("community_df_long", election = 2, treatment = "visit_50km")
model_voteshare_comm_fullsample_simple_50km <- lm(model_voteshare_comm_formula_simple, data =  dat_full, weights = dat_full$gs)
model_voteshare_comm_fullsample_simple_50km$clusterse <- runClusterRobustOLS(model_voteshare_comm_fullsample_simple_50km, dat_full$lfnr)
dat_raw <- community_df_long[community_df_long$election == 2,]
dat_match <- performMatch("visit_50km", dat.raw = "dat_raw", electiontype = "community")
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
dat_match <- performMatch("visit_5km", dat.raw = "dat_raw", electiontype = "community")
dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "community_df_long", election = 2, treatment = "visit_5km")
model_voteshare_kpd_comm_matched_simple_5km <- lm(model_voteshare_kpd_comm_formula_simple, data =  dat_match, weights = dat_match$gs)
model_voteshare_kpd_comm_matched_simple_5km$clusterse <- runClusterRobustOLS(model_voteshare_kpd_comm_matched_simple_5km, dat_match$lfnr)
# 10km
dat_full <- getDatFull("community_df_long", election = 2, treatment = "visit_10km")
model_voteshare_kpd_comm_fullsample_simple_10km <- lm(model_voteshare_kpd_comm_formula_simple, data =  dat_full, weights = dat_full$gs)
model_voteshare_kpd_comm_fullsample_simple_10km$clusterse <- runClusterRobustOLS(model_voteshare_kpd_comm_fullsample_simple_10km, dat_full$lfnr)
dat_raw <- community_df_long[community_df_long$election == 2,]
dat_match <- performMatch("visit_10km", dat.raw = "dat_raw", electiontype = "community")
dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "community_df_long", election = 2, treatment = "visit_10km")
model_voteshare_kpd_comm_matched_simple_10km <- lm(model_voteshare_kpd_comm_formula_simple, data =  dat_match, weights = dat_match$gs)
model_voteshare_kpd_comm_matched_simple_10km$clusterse <- runClusterRobustOLS(model_voteshare_kpd_comm_matched_simple_10km, dat_match$lfnr)
# 25km
dat_full <- getDatFull("community_df_long", election = 2, treatment = "visit_25km")
model_voteshare_kpd_comm_fullsample_simple_25km <- lm(model_voteshare_kpd_comm_formula_simple, data =  dat_full, weights = dat_full$gs)
model_voteshare_kpd_comm_fullsample_simple_25km$clusterse <- runClusterRobustOLS(model_voteshare_kpd_comm_fullsample_simple_25km, dat_full$lfnr)
dat_raw <- community_df_long[community_df_long$election == 2,]
dat_match <- performMatch("visit_25km", dat.raw = "dat_raw", electiontype = "community")
dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "community_df_long", election = 2, treatment = "visit_25km")
model_voteshare_kpd_comm_matched_simple_25km <- lm(model_voteshare_kpd_comm_formula_simple, data =  dat_match, weights = dat_match$gs)
model_voteshare_kpd_comm_matched_simple_25km$clusterse <- runClusterRobustOLS(model_voteshare_kpd_comm_matched_simple_25km, dat_match$lfnr)
# 50km
dat_full <- getDatFull("community_df_long", election = 2, treatment = "visit_50km")
model_voteshare_kpd_comm_fullsample_simple_50km <- lm(model_voteshare_kpd_comm_formula_simple, data =  dat_full, weights = dat_full$gs)
model_voteshare_kpd_comm_fullsample_simple_50km$clusterse <- runClusterRobustOLS(model_voteshare_kpd_comm_fullsample_simple_50km, dat_full$lfnr)
dat_raw <- community_df_long[community_df_long$election == 2,]
dat_match <- performMatch("visit_50km", dat.raw = "dat_raw", electiontype = "community")
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
dat_match <- performMatch("visit_5km", dat.raw = "dat_raw", electiontype = "community")
dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "community_df_long", election = 2, treatment = "visit_5km")
model_turnout_comm_matched_simple_5km <- lm(model_turnout_comm_formula_simple, data =  dat_match, weights = dat_match$wbht)
model_turnout_comm_matched_simple_5km$clusterse <- runClusterRobustOLS(model_turnout_comm_matched_simple_5km, dat_match$lfnr)
# 10km
dat_full <- getDatFull("community_df_long", election = 2, treatment = "visit_10km")
model_turnout_comm_fullsample_simple_10km <- lm(model_turnout_comm_formula_simple, data =  dat_full, weights = dat_full$wbht)
model_turnout_comm_fullsample_simple_10km$clusterse <- runClusterRobustOLS(model_turnout_comm_fullsample_simple_10km, dat_full$lfnr)
dat_raw <- community_df_long[community_df_long$election == 2,]
dat_match <- performMatch("visit_10km", dat.raw = "dat_raw", electiontype = "community")
dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "community_df_long", election = 2, treatment = "visit_10km")
model_turnout_comm_matched_simple_10km <- lm(model_turnout_comm_formula_simple, data =  dat_match, weights = dat_match$wbht)
model_turnout_comm_matched_simple_10km$clusterse <- runClusterRobustOLS(model_turnout_comm_matched_simple_10km, dat_match$lfnr)
# 25km
dat_full <- getDatFull("community_df_long", election = 2, treatment = "visit_25km")
model_turnout_comm_fullsample_simple_25km <- lm(model_turnout_comm_formula_simple, data =  dat_full, weights = dat_full$wbht)
model_turnout_comm_fullsample_simple_25km$clusterse <- runClusterRobustOLS(model_turnout_comm_fullsample_simple_25km, dat_full$lfnr)
dat_raw <- community_df_long[community_df_long$election == 2,]
dat_match <- performMatch("visit_25km", dat.raw = "dat_raw", electiontype = "community")
dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "community_df_long", election = 2, treatment = "visit_25km")
model_turnout_comm_matched_simple_25km <- lm(model_turnout_comm_formula_simple, data =  dat_match, weights = dat_match$wbht)
model_turnout_comm_matched_simple_25km$clusterse <- runClusterRobustOLS(model_turnout_comm_matched_simple_25km, dat_match$lfnr)
# 50km
dat_full <- getDatFull("community_df_long", election = 2, treatment = "visit_50km")
model_turnout_comm_fullsample_simple_50km <- lm(model_turnout_comm_formula_simple, data =  dat_full, weights = dat_full$wbht)
model_turnout_comm_fullsample_simple_50km$clusterse <- runClusterRobustOLS(model_turnout_comm_fullsample_simple_50km, dat_full$lfnr)
dat_raw <- community_df_long[community_df_long$election == 2,]
dat_match <- performMatch("visit_50km", dat.raw = "dat_raw", electiontype = "community")
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


