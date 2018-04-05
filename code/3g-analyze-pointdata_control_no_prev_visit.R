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
load("community_df_pointdata_long.RData")
varnames_tab <- read.csv("varnames_labels.csv", stringsAsFactors = FALSE, sep = ",")

# NEW IN THIS SCRIPT: exclude controls with previous Hitler or current Goebbels visit
# NOTE: does not work for presidential election 1932; hardly any plausible controls available since Hitler only visits places he had visited before


## estimate propensity score models + matching -----------------------

# 1:1 matching, Reichstag elections
model.out.list <- list()
model.out.probit.list <- list()
match.data.list <- list()
covar_names <- c("wkr_comp_last", "wkv_comp_last", "memberst", "airfields_dist_min100km", "wbht", "l_p_nsdap", "l_visit_10km", "goebbels_10km")
model_formula <- paste("visit_10km ~ ", paste(covar_names, collapse=" + "), sep = "") %>% as.formula
set.seed(42)
for (i in 1:4) {
  dat_ps <- county_df_long[county_df_long$election == i+1,]
  rownames(dat_ps) <- 1:nrow(dat_ps)
  model.out.list[[i]] <- matchit(formula = model_formula, 
                                 data = dat_ps,
                                 method = "nearest", # NN matching
                                 m.order = "random", # matches in random order
                                 ratio = 1, # 1:1 matching
                                 caliper = .25, # .25 standard deviations of the distance measure within which to draw control units
                                 replace = FALSE, # control unit can be matched to more than one treated unit
                                 discard = "none", # no units (treated and control) that are outside the support  will be discarded.
                                 exact = "visit_10km_nomatch", # introduce matching buffer zone
                                 distance = "probit") # probit distance measure
  model.out.probit.list[[i]] <- model.out.list[[i]]$model
  match.data.list[[i]] <- match.data(model.out.list[[i]])
  match.data.list[[i]] <- filter(match.data.list[[i]], visit_10km == 1 | (visit_10km == 0 & l_visit_10km == 0 & goebbels_10km == 0)) # NEW IN THIS SCRIPT: exclude controls with previous Hitler or current Goebbels visit
  #print(summary(model.out.list[[i]]))
  print(model.out.list[[i]]$nn)
}		


# 1:1 matching, presidential election
covar_names_pres <- c( "memberst", "airfields_dist_min100km", "wbht", "l_p_hitl", "l_visit_10km", "goebbels_10km")
model_formula <- paste("visit_10km ~ ", paste(covar_names_pres, collapse=" + "),sep = "") %>% as.formula
set.seed(42)
dat_ps <- county_pres_df_long[county_pres_df_long$election == 7,]
rownames(dat_ps) <- 1:nrow(dat_ps)
model.out.pres <- matchit(formula = model_formula, 
                          data = dat_ps,
                          method = "nearest", 
                          m.order = "random",
                          ratio = 1,
                          caliper = .25,
                          replace = FALSE,
                          discard = "none",
                          exact = "visit_10km_nomatch",
                          distance = "probit")
model.out.probit.pres <- model.out.pres$model
match.data.pres <- match.data(model.out.pres)
#print(summary(model.out.pres))
print(model.out.pres$nn)


# 1:1 matching, 1930 Reichstag election, community-level data
covar_names <- c("wkr_comp_last", "wkv_comp_last", "memberst", "airfields_dist_min100km", "wbht", "l_p_nsdap", "l_visit_10km", "goebbels_10km")
model_formula <- paste("visit_10km ~ ", paste(covar_names, collapse=" + "),sep = "") %>% as.formula
set.seed(42)
dat_ps <- community_df_long[community_df_long$election == 2,]
rownames(dat_ps) <- 1:nrow(dat_ps)
model.out.comm <- matchit(formula = model_formula, 
                          data = dat_ps,
                          method = "nearest", 
                          m.order = "random",
                          ratio = 1,
                          caliper = .25,
                          replace = FALSE,
                          discard = "none",
                          exact = "visit_10km_nomatch",
                          distance = "probit")
model.out.probit.comm <- model.out.comm$model
match.data.comm <- match.data(model.out.comm)
match.data.comm <- filter(match.data.comm, visit_10km == 1 | (visit_10km == 0 & l_visit_10km == 0 & goebbels_10km == 0)) # exclude controls with previous Hitler or current Goebbels visit
#print(summary(model.out.comm))
print(model.out.comm$nn)
table(match.data.comm$visit_10km, match.data.comm$l_visit_10km)



## table: probit models -------------
model.out.list[[5]] <- model.out.pres
model.out.list[[6]] <- model.out.comm


## visualize propensity scores -----------------

# plot propensity scores by matching and treatment status
pdf(file="../figures/propscores_jitterplot-control-no-prev-visit.pdf", height=6, width=8, family="URWTimes")
par(oma=c(0,0,1,0))
#layout(matrix(c(1,2,3,4,5), 1, 5, byrow = TRUE), widths=c(1.15, 1, 1, 1, 1), heights=c(2, 2, 2, 2, 2))
layout(matrix(c(1,2,3,4,5,6), 2, 3, byrow = TRUE), widths=c(1.15, 1, 1), heights=c(2, 2))
election_names <- c("Sep 1930\n", "Jul 1932\n", "Nov 1932\n", "Mar 1933\n", "Apr 1932 (P)\n", "Sep 1930 (mun.)\n")
for (i in c(1, 6, 5, 2, 3, 4)) {
  if (i %in% c(3, 4, 5, 6)) {par(mar=c(6,.5,2,.5)) }
  if (i %in% c(1, 2)) {par(mar=c(6,4,2,.5)) }
  if (i %in% c(1:4)) {
    match_dat <- matchDatFunMatrix(i, "county_df_long")
  }else if (i == 5){ match_dat <- matchDatFunMatrix(i, "county_pres_df_long")
  }else {match_dat <- matchDatFunMatrix(i, "community_df_long")}
  dat_treated_unmatched <- match_dat$distance[match_dat$treated == TRUE & match_dat$matched == FALSE]
  dat_treated_matched <- match_dat$distance[match_dat$treated == TRUE & match_dat$matched == TRUE & !is.na(match_dat$id_var_match)  & match_dat$id_var_match != -1]
  dat_untreated_matched <- match_dat$distance_match[match_dat$treated == TRUE & match_dat$matched == TRUE & !is.na(match_dat$id_var_match) & match_dat$id_var_match != -1] # look out; treated == TRUE is correct because we look at distance_match variable!
  dat_untreated_unmatched <- match_dat$distance[match_dat$treated == FALSE & match_dat$matched == FALSE & match_dat$buffer == 0]
  set.seed(123)
  dat_treated_unmatched_x <- rnorm(length(dat_treated_unmatched), 0, .02)
  dat_treated_matched_x <- rnorm(length(dat_treated_matched), 1, .02)
  dat_untreated_matched_x <- rnorm(length(dat_untreated_matched), 2, .02)
  dat_untreated_unmatched_x <- rnorm(length(dat_untreated_unmatched), 3, .02)
  n_groups <- c(match_dat %>% filter(treated == 1, matched == 0) %>% .$id_var %>% unique() %>% length(),
                match_dat %>% filter(treated == 1, matched == 1) %>% .$id_var %>% unique() %>% length(),
                match_dat %>% filter(treated == 0, matched == 1) %>% .$id_var %>% unique() %>% length(),
                match_dat %>% filter(treated == 0, matched == 0, buffer == 0) %>% .$id_var %>% unique() %>% length())
  if(i == 1 | i == 2) {
    plot(dat_treated_unmatched_x, dat_treated_unmatched, xlim = c(-.25,3.25), ylim = c(0,1), pch = 20, col = rgb(.1,.1,.1, .3), xaxt = "n", xlab = "", ylab ="Propensity score", main = election_names[i]) }else{
      plot(dat_treated_unmatched_x, dat_treated_unmatched, xlim = c(-.25,3.25), ylim = c(0,1), pch = 20, col = rgb(.1,.1,.1, .3), xaxt = "n", yaxt = "n", xlab = "", ylab ="", main = election_names[i])
    }
  points(dat_treated_matched_x, dat_treated_matched, pch = 20, col = rgb(.1,.1,.1, .3))
  points(dat_untreated_matched_x, dat_untreated_matched, pch = 20, col = rgb(.1,.1,.1, .3))
  points(dat_untreated_unmatched_x, dat_untreated_unmatched, pch = 20, col = rgb(.1,.1,.1, .3))
  axis(1, at = c(0,1,2,3), labels = c("Unmatched,", "Matched,", "Matched,", "Unmatched,"), line = 0, tick = FALSE)
  axis(1, at = c(0,1,2,3), labels = c("treated", "treated", "control", "control"), line = 1, tick = FALSE)
  axis(1, at = c(0,1,2,3), labels = paste0("(n = ", n_groups, ")"), line = 2, tick = FALSE)
  arrows(x0 = dat_treated_matched_x, y0 = dat_treated_matched, x1 = dat_untreated_matched_x, y1 = dat_untreated_matched, length = 0)
  abline(h=seq(0,1,.2),lty = 5, lwd = 1, col = "darkgrey")
}
dev.off()





## diff-in-diff analysis -----------------------

# model NSDAP vote share
model_voteshare_formula_simple <- paste("p_nsdap ~ time + exposure + timeXexposure",sep = "") %>% as.formula
model_voteshare_fullsample_simple <- list()
model_voteshare_matched_simple <- list()
for (i in 1:4) {
  # full sample, lm simple model with voting population weights (i.e., only number of voters is considered)
  dat_full <- getDatFull("county_df_long", election = i+1, treatment = "visit_10km")
  model_voteshare_fullsample_simple[[i]] <- lm(model_voteshare_formula_simple, data =  dat_full, weights = dat_full$gs)
  model_voteshare_fullsample_simple[[i]]$clusterse <- runClusterRobustOLS(model_voteshare_fullsample_simple[[i]], dat_full$lfnr)
  # 1:1 matching, lm simple model with voting population weights * matching weights
  dat_match <- match.data.list[[i]]
  dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "county_df_long", election = i+1, treatment = "visit_10km")
  dat_match$weights <- rep(dat_match$weights[!is.na(dat_match$weights)], 2)
  dat_match$combweights <- dat_match$gs  * dat_match$weights
  model_voteshare_matched_simple[[i]] <- lm(model_voteshare_formula_simple, data =  dat_match, weights = dat_match$combweights)
  model_voteshare_matched_simple[[i]]$clusterse <- runClusterRobustOLS(model_voteshare_matched_simple[[i]], dat_match$lfnr)
}



# model KPD vote share
model_voteshare_kpd_formula_simple <- paste("p_kpd ~ time + exposure + timeXexposure",sep = "") %>% as.formula
model_voteshare_kpd_fullsample_simple <- list()
model_voteshare_kpd_matched_simple <- list()
for (i in 1:4) {
  # full sample, lm simple model with voting population weights (i.e., only number of voters is considered)
  dat_full <- getDatFull("county_df_long", election = i+1, treatment = "visit_10km")
  model_voteshare_kpd_fullsample_simple[[i]] <- lm(model_voteshare_kpd_formula_simple, data =  dat_full, weights = dat_full$gs)
  model_voteshare_kpd_fullsample_simple[[i]]$clusterse <- runClusterRobustOLS(model_voteshare_kpd_fullsample_simple[[i]], dat_full$lfnr)
  # 1:1 matching, lm simple model with voting population weights * matching weights
  dat_match <- match.data.list[[i]]
  dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "county_df_long", election = i+1, treatment = "visit_10km")
  dat_match$weights <- rep(dat_match$weights[!is.na(dat_match$weights)], 2)
  dat_match$combweights <- dat_match$gs  * dat_match$weights
  model_voteshare_kpd_matched_simple[[i]] <- lm(model_voteshare_kpd_formula_simple, data =  dat_match, weights = dat_match$combweights)
  model_voteshare_kpd_matched_simple[[i]]$clusterse <- runClusterRobustOLS(model_voteshare_kpd_matched_simple[[i]], dat_match$lfnr)
}

# model turnout
model_turnout_formula_simple <- paste("p_turnout ~ time + exposure + timeXexposure",sep = "") %>% as.formula
model_turnout_fullsample_simple <- list()
model_turnout_matched_simple <- list()
for (i in 1:4) {
  # full sample, lm simple model with voting population weights (i.e., only number of voters is considered)
  dat_full <- getDatFull("county_df_long", election = i+1, treatment = "visit_10km")
  model_turnout_fullsample_simple[[i]] <- lm(model_turnout_formula_simple, data =  dat_full, weights = dat_full$wbht)
  model_turnout_fullsample_simple[[i]]$clusterse <- runClusterRobustOLS(model_turnout_fullsample_simple[[i]], dat_full$lfnr)
  # 1:1 matching, lm simple model with voting population weights * matching weights
  dat_match <- match.data.list[[i]]
  dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "county_df_long", election = i+1, treatment = "visit_10km")
  dat_match$weights <- rep(dat_match$weights[!is.na(dat_match$weights)], 2)
  dat_match$combweights <- dat_match$wbht  * dat_match$weights
  model_turnout_matched_simple[[i]] <- lm(model_turnout_formula_simple, data =  dat_match, weights = dat_match$combweights)
  model_turnout_matched_simple[[i]]$clusterse <- runClusterRobustOLS(model_turnout_matched_simple[[i]], dat_match$lfnr)
}


# model NSDAP vote share, 1930 election, municipal data
model_voteshare_comm_formula_simple <- paste("p_nsdap ~ time + exposure + timeXexposure",sep = "") %>% as.formula
# full sample, lm simple model with voting population weights (i.e., only number of voters is considered)
dat_full <- getDatFull("community_df_long", election = 2, treatment = "visit_10km")
model_voteshare_comm_fullsample_simple <- lm(model_voteshare_comm_formula_simple, data =  dat_full, weights = dat_full$gs)
model_voteshare_comm_fullsample_simple$clusterse <- runClusterRobustOLS(model_voteshare_comm_fullsample_simple, dat_full$lfnr)
# 1:1 matching, lm simple model with voting population weights
dat_match <- match.data.comm
dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "community_df_long", election = 2, treatment = "visit_10km")
dat_match$weights <- rep(dat_match$weights[!is.na(dat_match$weights)], 2)
dat_match$combweights <- dat_match$gs  * dat_match$weights
model_voteshare_comm_matched_simple <- lm(model_voteshare_comm_formula_simple, data =  dat_match, weights = dat_match$combweights)
model_voteshare_comm_matched_simple$clusterse <- runClusterRobustOLS(model_voteshare_comm_matched_simple, dat_match$lfnr)

# model KPD vote share, 1930 election, municipal data
model_voteshare_kpd_comm_formula_simple <- paste("p_kpd ~ time + exposure + timeXexposure",sep = "") %>% as.formula
# full sample, lm simple model with voting population weights (i.e., only number of voters is considered)
dat_full <- getDatFull("community_df_long", election = 2, treatment = "visit_10km")
model_voteshare_kpd_comm_fullsample_simple <- lm(model_voteshare_kpd_comm_formula_simple, data =  dat_full, weights = dat_full$gs)
model_voteshare_kpd_comm_fullsample_simple$clusterse <- runClusterRobustOLS(model_voteshare_kpd_comm_fullsample_simple, dat_full$lfnr)
# 1:1 matching, lm simple model with voting population weights
dat_match <- match.data.comm
dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "community_df_long", election = 2, treatment = "visit_10km")
dat_match$weights <- rep(dat_match$weights[!is.na(dat_match$weights)], 2)
dat_match$combweights <- dat_match$gs  * dat_match$weights
model_voteshare_kpd_comm_matched_simple <- lm(model_voteshare_kpd_comm_formula_simple, data =  dat_match, weights = dat_match$combweights)
model_voteshare_kpd_comm_matched_simple$clusterse <- runClusterRobustOLS(model_voteshare_kpd_comm_matched_simple, dat_match$lfnr)

# model turnout, 1930 election, municipal data
model_turnout_comm_formula_simple <- paste("p_turnout ~ time + exposure + timeXexposure",sep = "") %>% as.formula
# full sample, lm simple model with voting population weights (i.e., only number of voters is considered)
dat_full <- getDatFull("community_df_long", election = 2, treatment = "visit_10km")
model_turnout_comm_fullsample_simple <- lm(model_turnout_comm_formula_simple, data =  dat_full, weights = dat_full$gs)
model_turnout_comm_fullsample_simple$clusterse <- runClusterRobustOLS(model_turnout_comm_fullsample_simple, dat_full$lfnr)
# 1:1 matching, lm simple model with voting population weights
dat_match <- match.data.comm
dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "community_df_long", election = 2, treatment = "visit_10km")
dat_match$weights <- rep(dat_match$weights[!is.na(dat_match$weights)], 2)
dat_match$combweights <- dat_match$wbht  * dat_match$weights
model_turnout_comm_matched_simple <- lm(model_turnout_comm_formula_simple, data =  dat_match, weights = dat_match$combweights)
model_turnout_comm_matched_simple$clusterse <- runClusterRobustOLS(model_turnout_comm_matched_simple, dat_match$lfnr)




## tables: diff-in-diff models -----------------------

# export NSDAP vote share models
model_voteshare_list <- list(model_voteshare_fullsample_simple[[1]], model_voteshare_matched_simple[[1]], 
                             model_voteshare_comm_fullsample_simple, model_voteshare_comm_matched_simple,
                             model_voteshare_fullsample_simple[[2]], model_voteshare_matched_simple[[2]], 
                             model_voteshare_fullsample_simple[[3]], model_voteshare_matched_simple[[3]],
                             model_voteshare_fullsample_simple[[4]], model_voteshare_matched_simple[[4]])
model_voteshare_ses <- list(model_voteshare_fullsample_simple[[1]]$clusterse[,2], model_voteshare_matched_simple[[1]]$clusterse[,2],
                            model_voteshare_comm_fullsample_simple$clusterse[,2], model_voteshare_comm_matched_simple$clusterse[,2],
                            model_voteshare_fullsample_simple[[2]]$clusterse[,2], model_voteshare_matched_simple[[2]]$clusterse[,2], 
                            model_voteshare_fullsample_simple[[3]]$clusterse[,2], model_voteshare_matched_simple[[3]]$clusterse[,2],
                            model_voteshare_fullsample_simple[[4]]$clusterse[,2], model_voteshare_matched_simple[[4]]$clusterse[,2])
obs <- c(length(model_voteshare_fullsample_simple[[1]]$residuals)/2,
         length(model_voteshare_matched_simple[[1]]$residuals)/2,
         length(model_voteshare_comm_fullsample_simple$residuals)/2,
         length(model_voteshare_comm_matched_simple$residuals)/2,
         length(model_voteshare_fullsample_simple[[2]]$residuals)/2,
         length(model_voteshare_matched_simple[[2]]$residuals)/2,
         length(model_voteshare_fullsample_simple[[3]]$residuals)/2,
         length(model_voteshare_matched_simple[[3]]$residuals)/2,
         length(model_voteshare_fullsample_simple[[4]]$residuals)/2,
         length(model_voteshare_matched_simple[[4]]$residuals)/2) %>% round
varnames_long <- c("\\cmidrule(r){2-3}\\cmidrule(r){4-5}\\cmidrule(r){6-7}\\cmidrule(r){8-9}\\cmidrule(r){10-11}Time trend", "Base rate difference", "Exposure, 10km", "(Intercept)")
election_names <- c("Sep 1930", "Sep 1930 (mun.)", "Jul 1932", "Nov 1932", "Mar 1933")
effect_models_tex <- stargazer(model_voteshare_list, 
                               dep.var.caption = "", #dep.var.caption = "NSDAP/Hitler vote share", 
                               omit.table.layout = "d",
                               covariate.labels = varnames_long, 
                               model.numbers = FALSE, 
                               keep=c("Constant", "^timeXexposure", "exposure", "time"),
                               omit.stat = c("rsq", "res.dev", "ser", "n", "f"),
                               no.space = TRUE,
                               df = FALSE, 
                               se = model_voteshare_ses,
                               add.lines = list(c("Sample", rep(c("full", "matched"), 5)),
                                                c("Observations", obs)),
                               notes        = "Diff-in-diff models with number of actual voters as population weights. Clustered SEs shown. $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01", 
                               notes.append = FALSE,
                               column.labels = election_names, 
                               column.separate = c(2, 2, 2, 2, 2), 
                               font.size = "scriptsize",
                               title = paste0("Diff-in-diff estimates of exposure effects on NSDAP/Hitler vote share, discarding control units with a previous Hitler or Goebbels visit.\\vspace{-.25cm}"), 
                               label = paste0("tab:nsdap-voteshare-dd-control-no-prev-visit"),
                               out = paste0("../figures/tab-effect-voteshare-models-control-no-prev-visit.tex"))


# export KPD vote share models
model_voteshare_kpd_list <- list(model_voteshare_kpd_fullsample_simple[[1]], model_voteshare_kpd_matched_simple[[1]], 
                                 model_voteshare_kpd_comm_fullsample_simple, model_voteshare_kpd_comm_matched_simple,
                                 model_voteshare_kpd_fullsample_simple[[2]], model_voteshare_kpd_matched_simple[[2]], 
                                 model_voteshare_kpd_fullsample_simple[[3]], model_voteshare_kpd_matched_simple[[3]],
                                 model_voteshare_kpd_fullsample_simple[[4]], model_voteshare_kpd_matched_simple[[4]])
model_voteshare_kpd_ses <- list(model_voteshare_kpd_fullsample_simple[[1]]$clusterse[,2], model_voteshare_kpd_matched_simple[[1]]$clusterse[,2],
                                model_voteshare_kpd_comm_fullsample_simple$clusterse[,2], model_voteshare_kpd_comm_matched_simple$clusterse[,2],
                                model_voteshare_kpd_fullsample_simple[[2]]$clusterse[,2], model_voteshare_kpd_matched_simple[[2]]$clusterse[,2], 
                                model_voteshare_kpd_fullsample_simple[[3]]$clusterse[,2], model_voteshare_kpd_matched_simple[[3]]$clusterse[,2],
                                model_voteshare_kpd_fullsample_simple[[4]]$clusterse[,2], model_voteshare_kpd_matched_simple[[4]]$clusterse[,2])
obs <- c(length(model_voteshare_kpd_fullsample_simple[[1]]$residuals)/2,
         length(model_voteshare_kpd_matched_simple[[1]]$residuals)/2,
         length(model_voteshare_kpd_comm_fullsample_simple$residuals)/2,
         length(model_voteshare_kpd_comm_matched_simple$residuals)/2,
         length(model_voteshare_kpd_fullsample_simple[[2]]$residuals)/2,
         length(model_voteshare_kpd_matched_simple[[2]]$residuals)/2,
         length(model_voteshare_kpd_fullsample_simple[[3]]$residuals)/2,
         length(model_voteshare_kpd_matched_simple[[3]]$residuals)/2,
         length(model_voteshare_kpd_fullsample_simple[[4]]$residuals)/2,
         length(model_voteshare_kpd_matched_simple[[4]]$residuals)/2) %>% round
varnames_long <- c("\\cmidrule(r){2-3}\\cmidrule(r){4-5}\\cmidrule(r){6-7}\\cmidrule(r){8-9}\\cmidrule(r){10-11}Time trend", "Base rate difference", "Exposure, 10km", "(Intercept)")
election_names <- c("Sep 1930", "Sep 1930 (mun.)", "Jul 1932", "Nov 1932", "Mar 1933")
effect_models_tex <- stargazer(model_voteshare_kpd_list, 
                               dep.var.caption = "", #dep.var.caption = "KPD/Thälmann vote share", 
                               omit.table.layout = "d",
                               covariate.labels = varnames_long, 
                               model.numbers = FALSE, 
                               keep=c("Constant", "^timeXexposure", "exposure", "time"),
                               omit.stat = c("rsq", "res.dev", "ser", "n", "f"),
                               no.space = TRUE,
                               df = FALSE, 
                               se = model_voteshare_kpd_ses,
                               add.lines = list(c("Sample", rep(c("full", "matched"), 5)),
                                                c("Observations", obs)),
                               notes        = "Diff-in-diff models with number of actual voters as population weights. Clustered SEs shown. $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01", 
                               notes.append = FALSE,
                               column.labels = election_names, 
                               column.separate = c(2, 2, 2, 2, 2), 
                               font.size = "scriptsize",
                               title = paste0("Diff-in-diff estimates of exposure effects on KPD/Thälmann vote share, discarding control units with a previous Hitler or Goebbels visit.\\vspace{-.25cm}"), 
                               label = paste0("tab:kpd-voteshare-dd-control-no-prev-visit"),
                               out = paste0("../figures/tab-effect-voteshare-kpd-models-control-no-prev-visit.tex"))


# export turnout models
model_turnout_list <- list(model_turnout_fullsample_simple[[1]], model_turnout_matched_simple[[1]], 
                           model_turnout_comm_fullsample_simple, model_turnout_comm_matched_simple,
                           model_turnout_fullsample_simple[[2]], model_turnout_matched_simple[[2]], 
                           model_turnout_fullsample_simple[[3]], model_turnout_matched_simple[[3]],
                           model_turnout_fullsample_simple[[4]], model_turnout_matched_simple[[4]])
model_turnout_ses <- list(model_turnout_fullsample_simple[[1]]$clusterse[,2], model_turnout_matched_simple[[1]]$clusterse[,2],
                          model_turnout_comm_fullsample_simple$clusterse[,2], model_turnout_comm_matched_simple$clusterse[,2],
                          model_turnout_fullsample_simple[[2]]$clusterse[,2], model_turnout_matched_simple[[2]]$clusterse[,2], 
                          model_turnout_fullsample_simple[[3]]$clusterse[,2], model_turnout_matched_simple[[3]]$clusterse[,2],
                          model_turnout_fullsample_simple[[4]]$clusterse[,2], model_turnout_matched_simple[[4]]$clusterse[,2])
obs <- c(length(model_turnout_fullsample_simple[[1]]$residuals)/2,
         length(model_turnout_matched_simple[[1]]$residuals)/2,
         length(model_turnout_comm_fullsample_simple$residuals)/2,
         length(model_turnout_comm_matched_simple$residuals)/2,
         length(model_turnout_fullsample_simple[[2]]$residuals)/2,
         length(model_turnout_matched_simple[[2]]$residuals)/2,
         length(model_turnout_fullsample_simple[[3]]$residuals)/2,
         length(model_turnout_matched_simple[[3]]$residuals)/2,
         length(model_turnout_fullsample_simple[[4]]$residuals)/2,
         length(model_turnout_matched_simple[[4]]$residuals)/2) %>% round
varnames_long <- c("\\cmidrule(r){2-3}\\cmidrule(r){4-5}\\cmidrule(r){6-7}\\cmidrule(r){8-9}\\cmidrule(r){10-11}Time trend", "Base rate difference", "Exposure, 10km", "(Intercept)")
election_names <- c("Sep 1930", "Sep 1930 (mun.)", "Jul 1932", "Nov 1932", "Mar 1933")
effect_models_tex <- stargazer(model_turnout_list, 
                               dep.var.caption = "", #dep.var.caption = "Turnout", 
                               omit.table.layout = "d",
                               covariate.labels = varnames_long, 
                               model.numbers = FALSE, 
                               keep=c("Constant", "^timeXexposure", "exposure", "time"),
                               omit.stat = c("rsq", "res.dev", "ser", "n", "f"),
                               no.space = TRUE,
                               df = FALSE, 
                               se = model_turnout_ses,
                               add.lines = list(c("Sample", rep(c("full", "matched"), 5)),
                                                c("Observations", obs)),
                               notes        = "Diff-in-diff models with number of actual voters as population weights. Clustered SEs shown. $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01", 
                               notes.append = FALSE,
                               column.labels = election_names, 
                               column.separate = c(2, 2, 2, 2, 2), 
                               font.size = "scriptsize",
                               title = paste0("Diff-in-diff estimates of exposure effects on turnout, discarding control units with a previous Hitler or Goebbels visit.\\vspace{-.25cm}"), 
                               label = paste0("tab:turnout-dd-control-no-prev-visit"),
                               out = paste0("../figures/tab-effect-turnout-models-control-no-prev-visit.tex"))




## plot: coefficients of effect models ---------

pdf(file="../figures/effects_coefplot_control-no-prev-visit.pdf", height=3.5, width=8, family="URWTimes")
par(oma=c(.5,0,.5,0))
par(mar=c(3,7,3,1))
layout(matrix(1:3, 1, byrow = TRUE), widths=c(1.3, 1, 1), heights=c(2, 2, 2))
election_names <- rev(c("Sep 1930", "Sep 1930 (mun.)",  "Jul 1932", "Nov 1932", "Mar 1933"))
#colors <- brewer.pal(4,"BrBG")
colors <- c("darkgrey", "black")
## voteshare effect plot
plot(0, 0, xlim = c(-0.07, .07), ylim = c(-.3, 4.3), pch = 20, col = "white", xlab = "", ylab = "", yaxt = "n", xaxt = "n")
axis(2, at = 0:4, labels = F, tick = F)
text(y = 0:4, par("usr")[1], labels = election_names, srt = 0, pos = 2, xpd = TRUE)
axis(1, at = seq(-.06, .06, .03), labels = seq(-.06, .06, .03), tick = T)
mtext("NSDAP/Hitler vote share", side = 3, line = 1.2, outer = FALSE)
abline(v = 0, lty = 2)
abline(h = c(.5, 1.5, 2.5, 3.5), lty = 1, col = "darkgrey")
# 1930 election
y_par <- 4
mod_input <- model_voteshare_fullsample_simple[[1]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.15, x1 = mod$coef_hardhi, y1 = y_par+.15, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.15, x1 = mod$coef_weakhi, y1 = y_par+.15, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.15, pch = 20, col = colors[1])
mod_input <- model_voteshare_matched_simple[[1]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.15, x1 = mod$coef_hardhi, y1 = y_par-.15, length = 0, col = colors[2])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.15, x1 = mod$coef_weakhi, y1 = y_par-.15, length = 0, lwd = 2, col = colors[2])
points(mod$coef_point, y_par-.15, pch = 20, col = colors[2])
# 1930 election, municipal-level data
y_par <- 3
mod_input <- model_voteshare_comm_fullsample_simple
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.15, x1 = mod$coef_hardhi, y1 = y_par+.15, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.15, x1 = mod$coef_weakhi, y1 = y_par+.15, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.15, pch = 20, col = colors[1])
mod_input <- model_voteshare_comm_matched_simple
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.15, x1 = mod$coef_hardhi, y1 = y_par-.15, length = 0, col = colors[2])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.15, x1 = mod$coef_weakhi, y1 = y_par-.15, length = 0, lwd = 2, col = colors[2])
points(mod$coef_point, y_par-.15, pch = 20, col = colors[2])
# 1932/7 election
y_par <- 2
mod_input <- model_voteshare_fullsample_simple[[2]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.15, x1 = mod$coef_hardhi, y1 = y_par+.15, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.15, x1 = mod$coef_weakhi, y1 = y_par+.15, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.15, pch = 20, col = colors[1])
mod_input <- model_voteshare_matched_simple[[2]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.15, x1 = mod$coef_hardhi, y1 = y_par-.15, length = 0, col = colors[2])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.15, x1 = mod$coef_weakhi, y1 = y_par-.15, length = 0, lwd = 2, col = colors[2])
points(mod$coef_point, y_par-.15, pch = 20, col = colors[2])
# 1932/11 election
y_par <- 1
mod_input <- model_voteshare_fullsample_simple[[3]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.15, x1 = mod$coef_hardhi, y1 = y_par+.15, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.15, x1 = mod$coef_weakhi, y1 = y_par+.15, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.15, pch = 20, col = colors[1])
mod_input <- model_voteshare_matched_simple[[3]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.15, x1 = mod$coef_hardhi, y1 = y_par-.15, length = 0, col = colors[2])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.15, x1 = mod$coef_weakhi, y1 = y_par-.15, length = 0, lwd = 2, col = colors[2])
points(mod$coef_point, y_par-.15, pch = 20, col = colors[2])
# 1933/3 election
y_par <- 0
mod_input <- model_voteshare_fullsample_simple[[4]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.15, x1 = mod$coef_hardhi, y1 = y_par+.15, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.15, x1 = mod$coef_weakhi, y1 = y_par+.15, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.15, pch = 20, col = colors[1])
mod_input <- model_voteshare_matched_simple[[4]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.15, x1 = mod$coef_hardhi, y1 = y_par-.15, length = 0, col = colors[2])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.15, x1 = mod$coef_weakhi, y1 = y_par-.15, length = 0, lwd = 2, col = colors[2])
points(mod$coef_point, y_par-.15, pch = 20, col = colors[2])

## kpd voteshare effect plot
par(mar=c(3,0,3,1))
plot(0, 0, xlim = c(-0.07, .07), ylim = c(-.3, 4.3), pch = 20, col = "white", xlab = "", ylab = "", yaxt = "n", xaxt = "n")
axis(1, at = seq(-.06, .06, .03), labels = seq(-.06, .06, .03), tick = T)
mtext("KPD/Thälmann vote share", side = 3, line = 1.2, outer = FALSE)
abline(v = 0, lty = 2)
abline(h = c(.5, 1.5, 2.5, 3.5), lty = 1, col = "darkgrey")
# 1930 election
y_par <- 4
mod_input <- model_voteshare_kpd_fullsample_simple[[1]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.15, x1 = mod$coef_hardhi, y1 = y_par+.15, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.15, x1 = mod$coef_weakhi, y1 = y_par+.15, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.15, pch = 20, col = colors[1])
mod_input <- model_voteshare_kpd_matched_simple[[1]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.15, x1 = mod$coef_hardhi, y1 = y_par-.15, length = 0, col = colors[2])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.15, x1 = mod$coef_weakhi, y1 = y_par-.15, length = 0, lwd = 2, col = colors[2])
points(mod$coef_point, y_par-.15, pch = 20, col = colors[2])
# 1930 election, municipal-level data
y_par <- 3
mod_input <- model_voteshare_kpd_comm_fullsample_simple
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.15, x1 = mod$coef_hardhi, y1 = y_par+.15, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.15, x1 = mod$coef_weakhi, y1 = y_par+.15, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.15, pch = 20, col = colors[1])
mod_input <- model_voteshare_kpd_comm_matched_simple
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.15, x1 = mod$coef_hardhi, y1 = y_par-.15, length = 0, col = colors[2])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.15, x1 = mod$coef_weakhi, y1 = y_par-.15, length = 0, lwd = 2, col = colors[2])
points(mod$coef_point, y_par-.15, pch = 20, col = colors[2])
# 1932/7 election
y_par <- 2
mod_input <- model_voteshare_kpd_fullsample_simple[[2]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.15, x1 = mod$coef_hardhi, y1 = y_par+.15, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.15, x1 = mod$coef_weakhi, y1 = y_par+.15, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.15, pch = 20, col = colors[1])
mod_input <- model_voteshare_kpd_matched_simple[[2]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.15, x1 = mod$coef_hardhi, y1 = y_par-.15, length = 0, col = colors[2])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.15, x1 = mod$coef_weakhi, y1 = y_par-.15, length = 0, lwd = 2, col = colors[2])
points(mod$coef_point, y_par-.15, pch = 20, col = colors[2])
# 1932/11 election
y_par <- 1
mod_input <- model_voteshare_kpd_fullsample_simple[[3]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.15, x1 = mod$coef_hardhi, y1 = y_par+.15, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.15, x1 = mod$coef_weakhi, y1 = y_par+.15, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.15, pch = 20, col = colors[1])
mod_input <- model_voteshare_kpd_matched_simple[[3]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.15, x1 = mod$coef_hardhi, y1 = y_par-.15, length = 0, col = colors[2])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.15, x1 = mod$coef_weakhi, y1 = y_par-.15, length = 0, lwd = 2, col = colors[2])
points(mod$coef_point, y_par-.15, pch = 20, col = colors[2])
# 1933/3 election
y_par <- 0
mod_input <- model_voteshare_kpd_fullsample_simple[[4]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.15, x1 = mod$coef_hardhi, y1 = y_par+.15, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.15, x1 = mod$coef_weakhi, y1 = y_par+.15, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.15, pch = 20, col = colors[1])
mod_input <- model_voteshare_kpd_matched_simple[[4]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.15, x1 = mod$coef_hardhi, y1 = y_par-.15, length = 0, col = colors[2])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.15, x1 = mod$coef_weakhi, y1 = y_par-.15, length = 0, lwd = 2, col = colors[2])
points(mod$coef_point, y_par-.15, pch = 20, col = colors[2])
# model legend
text(.07, y_par+.15, labels = "full sample", col = colors[1], pos = 2)
text(.07, y_par-.15, labels = "matched sample", col = colors[2], pos = 2)

## turnout effect plot
par(mar=c(3,0,3,1))
plot(0, 0, xlim = c(-0.07, .07), ylim = c(-.3, 4.3), pch = 20, col = "white", xlab = "", ylab = "", yaxt = "n", xaxt = "n")
axis(1, at = seq(-.06, .06, .03), labels = seq(-.06, .06, .03), tick = T)
mtext("Turnout", side = 3, line = 1.2, outer = FALSE)
abline(v = 0, lty = 2)
abline(h = c(.5, 1.5, 2.5, 3.5), lty = 1, col = "darkgrey")
# 1930 election
y_par <- 4
mod_input <- model_turnout_fullsample_simple[[1]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.15, x1 = mod$coef_hardhi, y1 = y_par+.15, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.15, x1 = mod$coef_weakhi, y1 = y_par+.15, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.15, pch = 20, col = colors[1])
mod_input <- model_turnout_matched_simple[[1]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.15, x1 = mod$coef_hardhi, y1 = y_par-.15, length = 0, col = colors[2])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.15, x1 = mod$coef_weakhi, y1 = y_par-.15, length = 0, lwd = 2, col = colors[2])
points(mod$coef_point, y_par-.15, pch = 20, col = colors[2])
# 1930 election, municipal-level data
y_par <- 3
mod_input <- model_turnout_comm_fullsample_simple
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.15, x1 = mod$coef_hardhi, y1 = y_par+.15, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.15, x1 = mod$coef_weakhi, y1 = y_par+.15, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.15, pch = 20, col = colors[1])
mod_input <- model_turnout_comm_matched_simple
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.15, x1 = mod$coef_hardhi, y1 = y_par-.15, length = 0, col = colors[2])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.15, x1 = mod$coef_weakhi, y1 = y_par-.15, length = 0, lwd = 2, col = colors[2])
points(mod$coef_point, y_par-.15, pch = 20, col = colors[2])
# 1932/7 election
y_par <- 2
mod_input <- model_turnout_fullsample_simple[[2]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.15, x1 = mod$coef_hardhi, y1 = y_par+.15, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.15, x1 = mod$coef_weakhi, y1 = y_par+.15, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.15, pch = 20, col = colors[1])
mod_input <- model_turnout_matched_simple[[2]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.15, x1 = mod$coef_hardhi, y1 = y_par-.15, length = 0, col = colors[2])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.15, x1 = mod$coef_weakhi, y1 = y_par-.15, length = 0, lwd = 2, col = colors[2])
points(mod$coef_point, y_par-.15, pch = 20, col = colors[2])
# 1932/11 election
y_par <- 1
mod_input <- model_turnout_fullsample_simple[[3]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.15, x1 = mod$coef_hardhi, y1 = y_par+.15, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.15, x1 = mod$coef_weakhi, y1 = y_par+.15, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.15, pch = 20, col = colors[1])
mod_input <- model_turnout_matched_simple[[3]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.15, x1 = mod$coef_hardhi, y1 = y_par-.15, length = 0, col = colors[2])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.15, x1 = mod$coef_weakhi, y1 = y_par-.15, length = 0, lwd = 2, col = colors[2])
points(mod$coef_point, y_par-.15, pch = 20, col = colors[2])
# 1933/3 election
y_par <- 0
mod_input <- model_turnout_fullsample_simple[[4]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.15, x1 = mod$coef_hardhi, y1 = y_par+.15, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.15, x1 = mod$coef_weakhi, y1 = y_par+.15, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.15, pch = 20, col = colors[1])
mod_input <- model_turnout_matched_simple[[4]]
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par-.15, x1 = mod$coef_hardhi, y1 = y_par-.15, length = 0, col = colors[2])
arrows(x0 = mod$coef_weaklo, y0 = y_par-.15, x1 = mod$coef_weakhi, y1 = y_par-.15, length = 0, lwd = 2, col = colors[2])
points(mod$coef_point, y_par-.15, pch = 20, col = colors[2])
dev.off()

