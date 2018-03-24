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
c("../figures/propscores_coefplot.pdf",
  "../figures/tab-probit-models.tex",
  "../figures/propscores_jitterplot.pdf",
  "../figures/tab-balance1.tex",
  "../figures/tab-balance2.tex",
  "../figures/tab-effect-voteshare-models.tex",
  "../figures/tab-effect-voteshare-kpd-models.tex",
  "../figures/tab-effect-turnout-models.tex",
  "../figures/tab-effect-members-models.tex",
  "../figures/effects_coefplot.pdf")


## load packages and functions -------------------
source("packages.r")
source("functions.r")
source("functions-analysis.r")


## import prepared data --------------------------
load("county_df_pointdata_long.RData")
load("county_pres_df_pointdata_long.RData")
load("community_df_pointdata_long.RData")
varnames_tab <- read.csv("varnames_labels.csv", stringsAsFactors = FALSE, sep = ",")



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
#print(summary(model.out.comm))
print(model.out.comm$nn)




## plot: coefficient estimates of probit models -----------
varnames.long <- c(varnames_tab$varnames_long[match(c("l_p_hitl", "l_visit_10km", "l_p_nsdap", "memberst", "wkr_comp_last", "wkv_comp_last", "wbht", "airfields_dist_min100km", "goebbels_10km"), varnames_tab$varnames)], "(Intercept)") %>% rev()
colors <- brewer.pal(6,"BrBG")
colors[4] <- "#969696"

# re-run models
model.out.probit.plot.list <- list()
covar_names <- c("goebbels_10km", "airfields_dist_min100km", "wbht", "wkv_comp_last","wkr_comp_last",  "memberst", "l_p_nsdap", "l_visit_10km")
model_formula <- paste("visit_10km ~ ", paste(covar_names, collapse=" + "),sep = "") %>% as.formula
covar_names_pres <- c("goebbels_10km", "airfields_dist_min100km",  "wbht",  "memberst", "l_p_hitl", "l_visit_10km")
model_formula_pres <- paste("visit_10km ~ ", paste(covar_names_pres, collapse=" + "),sep = "") %>% as.formula
covar_names_comm <- c("l_visit_10km", "l_p_nsdap", "memberst", "wkr_comp_last", "wkv_comp_last", "wbht", "airfields_dist_min100km", "goebbels_10km")
model_formula_comm <- paste("visit_10km ~ ", paste(covar_names_comm, collapse=" + "),sep = "") %>% as.formula
set.seed(42)
for (i in 1:4) {
  model.out.probit.plot.list[[i]] <- glm(model_formula, data = county_df_long[county_df_long$election == i+1,], family=binomial(link="probit"))
}		
model.out.probit.plot.list[[5]] <- glm(model_formula_pres, data = county_pres_df_long[county_pres_df_long$election == 7,], family=binomial(link="probit"))
model.out.probit.plot.list[[6]] <- glm(model_formula_comm, data = community_df_long[community_df_long$election == 2,], family=binomial(link="probit"))
names(model.out.probit.plot.list[[6]]$coefficients) <- c("(Intercept)", "l_visit_10km", "l_p_nsdap", "memberst", "wkr_comp_last", "wkv_comp_last", "wbht", "airfields_dist_min100km", "goebbels_10kmTRUE")

# re-sort models
model.out.probit.plot.list <- list(model.out.probit.plot.list[[1]], model.out.probit.plot.list[[6]], model.out.probit.plot.list[[5]], model.out.probit.plot.list[[2]], model.out.probit.plot.list[[3]], model.out.probit.plot.list[[4]])

pdf(file="../figures/propscores_coefplot.pdf", height=7, width=10, family="URWTimes")
par(oma=c(0,0,0,0))
par(mar=c(2,2,2.5,.5))
par(xaxs = "i", yaxs = "i")
#par(xpd=TRUE)
coefplot2(rev(model.out.probit.plot.list),
          col = colors,
          xlim = c(-8,8),
          varnames = varnames.long,
          intercept = TRUE,
          main = "",
          h.axis = FALSE,
          top.axis = FALSE,
          cex.axis = 0,
          mar = c(2,14,2.5,.5), # margins
          spacing = .1,
          legend = TRUE,
          legend.args = list(x = 4, y = 3, inset = .02, legend = c("Sep 1930", "Sep 1930 (mun.)", "Apr 1932 (P)", "Jul 1932", "Nov 1932", "Mar 1933"), horiz = FALSE, cex = .8, bg = "white")
)
abline(v = seq(-8,8,2), lty = 5, col = "darkgrey")
axis(1, seq(-8,8,2), seq(-8,8,2))
axis(3, seq(-8,8,2), seq(-8,8,2))
dev.off()


## table: probit models -------------
model.out.list[[5]] <- model.out.pres
model.out.list[[6]] <- model.out.comm
model.out.probit.list[[5]] <- model.out.probit.pres
model.out.probit.list[[6]] <- model.out.probit.comm
names(model.out.probit.list[[6]]$coefficients) <- c("(Intercept)", "wkr_comp_last", "wkv_comp_last", "memberst","airfields_dist_min100km",  "wbht", "l_p_nsdap",  "l_visit_10km",  "goebbels_10kmTRUE")

model.out.probit.table.list <- list(model.out.probit.list[[1]], model.out.probit.list[[6]], model.out.probit.list[[5]], model.out.probit.list[[2]], model.out.probit.list[[3]], model.out.probit.list[[4]])

r2_mcfadden <- sapply(model.out.probit.table.list, R2probit) %>% round(2)
varnames.long <- c(varnames_tab$varnames_long[match(c("wkr_comp_last", "wkv_comp_last", "memberst",  "airfields_dist_min100km", "wbht", "l_p_nsdap", "l_p_hitl", "l_visit_10km", "goebbels_10kmTRUE"), varnames_tab$varnames)], "(Intercept)")
varnames.long[1] <- paste0("\\cmidrule(r){2-2}\\cmidrule(r){3-3}\\cmidrule(r){4-4}\\cmidrule(r){5-5}\\cmidrule(r){6-6}\\cmidrule(r){7-7}", varnames.long[1])
probit.models.tex <- stargazer(model.out.probit.table.list, dep.var.caption = "", covariate.labels = varnames.long, 
                               no.space = TRUE, add.lines = list(c("Mc-Fadden's Pseudo R2", r2_mcfadden)), omit.table.layout = "d#", column.labels = c("Sep 1930", "\\specialcell{Sep 1930 \\\\(mun.)}", "Apr 1932 (P)", "Jul 1932", "Nov 1932", "Mar 1933"), column.sep.width = "1pt", font.size = "footnotesize", table.placement = "t!", title="Probit estimates of Hitler appearances by election. Standard errors in parentheses.\\label{tab:probit-models}", out = "../figures/tab-probit-models.tex")
probit.models.tex <- stargazer(model.out.probit.table.list, dep.var.caption = "", covariate.labels = varnames.long, 
                               no.space = TRUE, add.lines = list(c("Mc-Fadden's Pseudo R2", r2_mcfadden), type = "html"), omit.table.layout = "d#", column.labels = c("Sep 1930", "\\specialcell{Sep 1930 \\\\(mun.)}", "Apr 1932 (P)", "Jul 1932", "Nov 1932", "Mar 1933"), column.sep.width = "1pt", font.size = "footnotesize", table.placement = "t!", title="Probit estimates of Hitler appearances by election. Standard errors in parentheses.\\label{tab:probit-models}", out = "../figures/tab-probit-models.html")



## visualize propensity scores -----------------

# plot propensity scores by matching and treatment status
pdf(file="../figures/propscores_jitterplot.pdf", height=6, width=8, family="URWTimes")
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




## check covariate balance after matching -----------------------------------------

tab_list <- list()
election_names <- c("\\textit{Sep 1930}", "\\textit{Sep 1930 (mun.)}", "\\textit{Apr 1932 (P)}", "\\textit{Jul 1932}", "\\textit{Nov 1932}", "\\textit{Mar 1933}")
# collect balance statistics
for (i in 1:6) {
# get summary data
tab <- data.frame(means.diff.before = summary(model.out.list[[i]])$sum.all$`Mean Diff`,
                  means.diff.after = summary(model.out.list[[i]])$sum.matched$`Mean Diff`,
                  means.diff.improvement = summary(model.out.list[[i]])$reduction$`Mean Diff.`
						 )
rownames(tab) <- rownames(summary(model.out.list[[i]])$sum.all)
tab <- round(tab, 2)
tab$name <- rownames(tab)
tab_list[[i]] <- tab
}

# fix rownames issue
rownames(tab_list[[5]]) <- str_replace(rownames(tab_list[[5]]), "l_p_hitl", "l_p_nsdap")

# combine into one table
tab_list <- list(tab_list[[1]], tab_list[[6]], tab_list[[5]], tab_list[[2]], tab_list[[3]], tab_list[[4]])
tab_tex <- do.call("cbind.fill", c(list(fill = ""), tab_list))
rownames(tab_tex) <- tab_tex$name

# fix pres election variables issue
tab_tex[4:11,9:11] <- tab_tex[2:9,9:11]
tab_tex[2:3,9:11] <- ""
tab_tex <- tab_tex[,-c(4,8,12,16,20,24)]

# apply new varnames
get_varnames <- varnames_tab$varnames_long[match(rownames(tab_tex), varnames_tab$varnames, nomatch = 0)] %>% char()
rownames(tab_tex) <- get_varnames

# drop line with exact matching variable
tab_tex <- tab_tex[rownames(tab_tex)!= "Observation in no-matching zone",]
tab_tex <- tab_tex[rownames(tab_tex)!= "No Goebbels appearance",]

# split table
tab_tex1 <- tab_tex[,1:9]
tab_tex2 <- tab_tex[,10:18]

# add fancy multicol names
addtorow1 <- list()
addtorow1$pos <- list(0)
# addtorow$command <- paste0(paste0('& & & & \\multicolumn{6}{c}{Empirical quantile measures}', collapse=''), '\\\\',
# paste0('\\cmidrule(r){5-10} & \\multicolumn{3}{c}{Mean difference} &  \\multicolumn{3}{c}{Mean difference} & \\multicolumn{3}{c}{Maximum difference}', collapse=''), '\\\\',
# paste0('\\cmidrule(r){2-4} \\cmidrule(r){5-7} \\cmidrule(r){8-10}\\textit{Variable names} & Before & After & \\% Impr. & Before & After & \\% Impr. & Before & After & \\% Impr.', collapbse=''), '\\\\')
addtorow1$command <- paste0(paste0(' & \\multicolumn{3}{c}{Sep 1930} &  \\multicolumn{3}{c}{Sep 1930 (mun.)} & \\multicolumn{3}{c}{Apr 1932 (P)}', collapse=''), '\\\\',
                           paste0('\\cmidrule(r){2-4} \\cmidrule(r){5-7} \\cmidrule(r){8-10}\\textit{Variable names} & Before & After & \\% Impr. & Before & After & \\% Impr. & Before & After & \\% Impr.', collapse=''), '\\\\')

addtorow2 <- list()
addtorow2$pos <- list(0)
# addtorow$command <- paste0(paste0('& & & & \\multicolumn{6}{c}{Empirical quantile measures}', collapse=''), '\\\\',
# paste0('\\cmidrule(r){5-10} & \\multicolumn{3}{c}{Mean difference} &  \\multicolumn{3}{c}{Mean difference} & \\multicolumn{3}{c}{Maximum difference}', collapse=''), '\\\\',
# paste0('\\cmidrule(r){2-4} \\cmidrule(r){5-7} \\cmidrule(r){8-10}\\textit{Variable names} & Before & After & \\% Impr. & Before & After & \\% Impr. & Before & After & \\% Impr.', collapbse=''), '\\\\')
addtorow2$command <- paste0(paste0(' & \\multicolumn{3}{c}{July 1932} &  \\multicolumn{3}{c}{November 1932} & \\multicolumn{3}{c}{Mar 1933}', collapse=''), '\\\\',
                            paste0('\\cmidrule(r){2-4} \\cmidrule(r){5-7} \\cmidrule(r){8-10}\\textit{Variable names} & Before & After & \\% Impr. & Before & After & \\% Impr. & Before & After & \\% Impr.', collapse=''), '\\\\')

# print tables
cols_align <- c("l", "r", rep("r", ncol(tab_tex1)-1))
tab_tex1 <- sapply(tab_tex1, function(x) num(char(x))) 
rownames(tab_tex1) <- rownames(tab_tex)
xtab1 <- xtable(tab_tex1, digits = c(0,rep(c(2,2,0), 3)), align = cols_align, caption = "Propensity score and covariate balance before and after matching. Mean differences on variables reported.\\label{tab:balance1}")
print(xtab1, booktabs = TRUE, size = "scriptsize", caption.placement = "top", table.placement = "t!", add.to.row=addtorow1,  include.rownames=TRUE, include.colnames = FALSE, sanitize.text.function = identity, file = "../figures/tab-balance1.tex")
print(xtab1, booktabs = TRUE, size = "scriptsize", caption.placement = "top", table.placement = "t!", add.to.row=addtorow1,  include.rownames=TRUE, include.colnames = FALSE, sanitize.text.function = identity, file = "../figures/tab-balance1.html", type = "html")


cols_align <- c("l", "r", rep("r", ncol(tab_tex2)-1))
tab_tex2 <- sapply(tab_tex2, function(x) num(char(x))) 
rownames(tab_tex2) <- rownames(tab_tex)
xtab2 <- xtable(tab_tex2, digits = c(0,rep(c(2,2,0), 3)), align = cols_align, caption = "Propensity score and covariate balance before and after matching, \\textit{continued}. Mean differences on variables reported.\\label{tab:balance2}")
print(xtab2, booktabs = TRUE, size = "scriptsize", caption.placement = "top", table.placement = "t!", add.to.row=addtorow2,  include.rownames=TRUE, include.colnames = FALSE, sanitize.text.function = identity, file = "../figures/tab-balance2.tex")
print(xtab2, booktabs = TRUE, size = "scriptsize", caption.placement = "top", table.placement = "t!", add.to.row=addtorow2,  include.rownames=TRUE, include.colnames = FALSE, sanitize.text.function = identity, file = "../figures/tab-balance2.html", type = "html")


## plot: pre-treatment trends -----------

# import prepared data
load("elections_all_df_pointdata_long.RData")
elections_df_long <- filter(elections_df_long, election >= 1 &  election <= 5)

# select variables
elections_df_long <- dplyr::select(elections_df_long, election, lfnr, agglvl, name,  pop, wb, gs, p_nsdap, p_kpd, p_hitl, p_turnout, visit_10km, d_p_nsdap, d_p_kpd, d_p_hitl, d_p_turnout)

# create lead visit variable; important to identify relevant exposure cases
elections_df_long <- 
  elections_df_long %>%
  group_by(lfnr) %>%
  mutate(l1.visit_10km = lead(visit_10km, 1), l2.visit_10km = lead(visit_10km, 2))

# select only matched data; compute summary statistics
match.data.long.list <- list()
match.data.group.list <- list()
match.data.sum.list <- list()
for (i in 1:4) {
  match.data.long.list[[i]] <- elections_df_long[elections_df_long$lfnr %in% match.data.list[[i]]$lfnr,]
  match.data.group.list[[i]] <-  group_by(match.data.long.list[[i]], election)
  match.data.sum.list[[i]] <- summarize(match.data.group.list[[i]], 
                                  p_nsdap_untreated_avg = mean(p_nsdap[visit_10km != 1], na.rm = TRUE),
                                  p_nsdap_treated_avg = mean(p_nsdap[visit_10km == 1], na.rm = TRUE),
                                  p_nsdap_untreated_avg_1 = mean(p_nsdap[l1.visit_10km != 1], na.rm = TRUE),
                                  p_nsdap_treated_avg_1 = mean(p_nsdap[l1.visit_10km == 1], na.rm = TRUE),
                                  p_nsdap_untreated_avg_2 = mean(p_nsdap[l2.visit_10km != 1], na.rm = TRUE),
                                  p_nsdap_treated_avg_2 = mean(p_nsdap[l2.visit_10km == 1], na.rm = TRUE),
                                  d_p_nsdap_untreated_avg = mean(d_p_nsdap[visit_10km != 1], na.rm = TRUE),
                                  d_p_nsdap_treated_avg = mean(d_p_nsdap[visit_10km == 1], na.rm = TRUE),
                                  d_p_nsdap_untreated_avg_1 = mean(d_p_nsdap[l1.visit_10km != 1], na.rm = TRUE),
                                  d_p_nsdap_treated_avg_1 = mean(d_p_nsdap[l1.visit_10km == 1], na.rm = TRUE),
                                  d_p_nsdap_untreated_avg_2 = mean(d_p_nsdap[l2.visit_10km != 1], na.rm = TRUE),
                                  d_p_nsdap_treated_avg_2 = mean(d_p_nsdap[l2.visit_10km == 1], na.rm = TRUE))
}


pdf(file="../figures/pretreatment_trends.pdf", height=10, width=7, family="URWTimes")
par(oma=c(0,0,0,0))
par(mar=c(3,4,5,3))
par(mfrow=c(3,1))
par(xaxs = "i", yaxs = "i")
election_names <-  c("May 1928", "Sep 1930", "Jul 1932", "Nov 1932", "Mar 1933")
for (i in 2:4) {
  # select relevant elections
  dat <- match.data.long.list[[i]]
  dat <- filter(dat, election <= i+1, election >= i-1)
  election_names_filtered <- election_names[(i-1):(i+1)]
  # empty plot, axes
  plot(dat$election-(i-2), dat$p_nsdap, cex=0, axes=F, ann = F, ylim=c(0,.8), xlim = c(.95, 3.05))
  axis(1, las=0, at = seq(1,3,1), election_names_filtered, cex=.8)
  axis(1, las=0, at = c(1.5,2.5), c("[pre-treatment period]", "[treatment period]"), cex=.8, tick = F)
  axis(2, las=1, at=seq(0,.8,.2), label=seq(0,.8,.2), cex=.8)
  axis(4, las=1, at=seq(0,.8,.2), label=seq(0,.8,.2), cex=.8)
  title(main = paste("Matched units for", election_names_filtered[3], "election"), ylab="NSDAP vote share", xlab="Election")
  # write average trends
  text(1.5, .72, labels = paste("Average trend, treatment group:", (match.data.sum.list[[i]]$p_nsdap_treated_avg_1[i] - match.data.sum.list[[i]]$p_nsdap_treated_avg_2[i-1]) %>% round(2)), col = "red")
  text(1.5, .67, labels = paste("Average trend, control group:", (match.data.sum.list[[i]]$p_nsdap_untreated_avg_1[i] - match.data.sum.list[[i]]$p_nsdap_untreated_avg_2[i-1]) %>% round(2)), col = "black")
  text(2.5, .72, labels = paste("Average trend, treatment group:", (match.data.sum.list[[i]]$p_nsdap_treated_avg[i+1] - match.data.sum.list[[i]]$p_nsdap_treated_avg_1[i]) %>% round(2)), col = "red")
  text(2.5, .67, labels = paste("Average trend, control group:", (match.data.sum.list[[i]]$p_nsdap_untreated_avg[i+1] - match.data.sum.list[[i]]$p_nsdap_untreated_avg_1[i]) %>% round(2)), col = "black")
  abline(h=seq(0, 1, .2), lty = 2, col = "darkgrey")
  # county lines, exposed units
  dat <- arrange(dat, lfnr, election)
  lines(dat$election[(dat$election == i+1 & dat$visit_10km == 1) | (dat$election == i &dat$l1.visit_10km == 1) | (dat$election == i-1 & dat$l2.visit_10km == 1)]-(i-2),
        dat$p_nsdap[(dat$election == i+1 & dat$visit_10km == 1) | (dat$election == i &dat$l1.visit_10km == 1) | (dat$election == i-1 & dat$l2.visit_10km == 1)],
        type = "b", lwd=1, col=rgb(.9,0,0, alpha=0.2))
  # county lines, control units
   lines(dat$election[-(dat$election == i+1 & dat$visit_10km == 1) | (dat$election == i &dat$l1.visit_10km == 1) | (dat$election == i-1 & dat$l2.visit_10km == 1)]-(i-2),
        dat$p_nsdap[!((dat$election == i+1 & dat$visit_10km == 1) | (dat$election == i &dat$l1.visit_10km == 1) | (dat$election == i-1 & dat$l2.visit_10km == 1))],
        type = "b", lwd=1, col=rgb(0,0,0, alpha=0.1))
  # average lines
  match.data.sum.list[[i]] <- filter(match.data.sum.list[[i]], election <= i+1, election >= i-1)
  lines(match.data.sum.list[[i]]$election-(i-2), 
        c(match.data.sum.list[[i]]$p_nsdap_untreated_avg_2[1],
          match.data.sum.list[[i]]$p_nsdap_untreated_avg_1[2],
          match.data.sum.list[[i]]$p_nsdap_untreated_avg[3]), type = "b", lwd = 3, lty = 1, col="black")
  lines(match.data.sum.list[[i]]$election-(i-2), 
        c(match.data.sum.list[[i]]$p_nsdap_treated_avg_2[1],
          match.data.sum.list[[i]]$p_nsdap_treated_avg_1[2],
          match.data.sum.list[[i]]$p_nsdap_treated_avg[3]), type = "b", lwd = 3, lty = 1, col="red")
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
  dat_match$combweights <- dat_match$wbht  * dat_match$weights
  model_turnout_matched_simple[[i]] <- lm(model_turnout_formula_simple, data =  dat_match, weights = dat_match$combweights)
  model_turnout_matched_simple[[i]]$clusterse <- runClusterRobustOLS(model_turnout_matched_simple[[i]], dat_match$lfnr)
}

# model NSDAP membership rates
model_members_formula_simple <- paste("p_members ~ time + exposure + timeXexposure",sep = "") %>% as.formula
model_members_fullsample_simple <- list()
model_members_matched_simple <- list()
for (i in 1:4) {
  # full sample, lm simple model with voting population weights (i.e., only number of voters is considered)
  dat_full <- getDatFull("county_df_long", election = i+1, treatment = "visit_10km")
  model_members_fullsample_simple[[i]] <- lm(model_members_formula_simple, data =  dat_full, weights = dat_full$gs)
  model_members_fullsample_simple[[i]]$clusterse <- runClusterRobustOLS(model_members_fullsample_simple[[i]], dat_full$lfnr)
  # 1:1 matching, lm simple model with voting population weights
  dat_match <- match.data.list[[i]]
  dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "county_df_long", election = i+1, treatment = "visit_10km")
  dat_match$combweights <- dat_match$gs  * dat_match$weights
  model_members_matched_simple[[i]] <- lm(model_members_formula_simple, data =  dat_match,  weights = dat_match$combweights)
  model_members_matched_simple[[i]]$clusterse <- runClusterRobustOLS(model_members_matched_simple[[i]], dat_match$lfnr)
}

# model Hitler vote share, presidential election
model_voteshare_pres_formula_simple <- paste("p_hitl ~ time + exposure + timeXexposure",sep = "") %>% as.formula
# full sample, lm simple model with voting population weights (i.e., only number of voters is considered)
dat_full <- getDatFull("county_pres_df_long", election = 7, treatment = "visit_10km")
model_voteshare_pres_fullsample_simple <- lm(model_voteshare_pres_formula_simple, data =  dat_full, weights = dat_full$gs)
model_voteshare_pres_fullsample_simple$clusterse <- runClusterRobustOLS(model_voteshare_pres_fullsample_simple, dat_full$lfnr)
# 1:1 matching, lm simple model with voting population weights
dat_match <- match.data.pres
dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "county_pres_df_long", election = 7, treatment = "visit_10km")
dat_match$combweights <- dat_match$gs  * dat_match$weights
model_voteshare_pres_matched_simple <- lm(model_voteshare_pres_formula_simple, data =  dat_match, weights = dat_match$combweights)
model_voteshare_pres_matched_simple$clusterse <- runClusterRobustOLS(model_voteshare_pres_matched_simple, dat_match$lfnr)

# model Thaelmann vote share, presidential election
model_voteshare_thae_pres_formula_simple <- paste("p_thae ~ time + exposure + timeXexposure",sep = "") %>% as.formula
# full sample, lm simple model with voting population weights (i.e., only number of voters is considered)
dat_full <- getDatFull("county_pres_df_long", election = 7, treatment = "visit_10km")
model_voteshare_thae_pres_fullsample_simple <- lm(model_voteshare_thae_pres_formula_simple, data =  dat_full, weights = dat_full$gs)
model_voteshare_thae_pres_fullsample_simple$clusterse <- runClusterRobustOLS(model_voteshare_thae_pres_fullsample_simple, dat_full$lfnr)
# 1:1 matching, lm simple model with voting population weights
dat_match <- match.data.pres
dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "county_pres_df_long", election = 7, treatment = "visit_10km")
dat_match$combweights <- dat_match$gs  * dat_match$weights
model_voteshare_thae_pres_matched_simple <- lm(model_voteshare_thae_pres_formula_simple, data =  dat_match, weights = dat_match$combweights)
model_voteshare_thae_pres_matched_simple$clusterse <- runClusterRobustOLS(model_voteshare_thae_pres_matched_simple, dat_match$lfnr)

# model turnout, presidential election
model_turnout_pres_formula_simple <- paste("p_turnout ~ time + exposure + timeXexposure",sep = "") %>% as.formula
# full sample, lm simple model with voting population weights (i.e., only number of voters is considered)
dat_full <- getDatFull("county_pres_df_long", election = 7, treatment = "visit_10km")
model_turnout_pres_fullsample_simple <- lm(model_turnout_pres_formula_simple, data =  dat_full, weights = dat_full$wbht)
model_turnout_pres_fullsample_simple$clusterse <- runClusterRobustOLS(model_turnout_pres_fullsample_simple, dat_full$lfnr)
# 1:1 matching, lm simple model with voting population weights
dat_match <- match.data.pres
dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "county_pres_df_long", election = 7, treatment = "visit_10km")
dat_match$combweights <- dat_match$wbht  * dat_match$weights
model_turnout_pres_matched_simple <- lm(model_turnout_pres_formula_simple, data =  dat_match, weights = dat_match$combweights)
model_turnout_pres_matched_simple$clusterse <- runClusterRobustOLS(model_turnout_pres_matched_simple, dat_match$lfnr)

# model NSDAP vote share, 1930 election, municipal data
model_voteshare_comm_formula_simple <- paste("p_nsdap ~ time + exposure + timeXexposure",sep = "") %>% as.formula
# full sample, lm simple model with voting population weights (i.e., only number of voters is considered)
dat_full <- getDatFull("community_df_long", election = 2, treatment = "visit_10km")
model_voteshare_comm_fullsample_simple <- lm(model_voteshare_comm_formula_simple, data =  dat_full, weights = dat_full$gs)
model_voteshare_comm_fullsample_simple$clusterse <- runClusterRobustOLS(model_voteshare_comm_fullsample_simple, dat_full$lfnr)
# 1:1 matching, lm simple model with voting population weights
dat_match <- match.data.comm
dat_match <- getDatMatch(dat.match = "dat_match", dat.orig = "community_df_long", election = 2, treatment = "visit_10km")
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
dat_match$combweights <- dat_match$wbht  * dat_match$weights
model_turnout_comm_matched_simple <- lm(model_turnout_comm_formula_simple, data =  dat_match, weights = dat_match$combweights)
model_turnout_comm_matched_simple$clusterse <- runClusterRobustOLS(model_turnout_comm_matched_simple, dat_match$lfnr)


  
  
## tables: diff-in-diff models -----------------------

# export NSDAP vote share models
model_voteshare_list <- list(model_voteshare_fullsample_simple[[1]], model_voteshare_matched_simple[[1]], 
                             model_voteshare_comm_fullsample_simple, model_voteshare_comm_matched_simple,
                             model_voteshare_pres_fullsample_simple, model_voteshare_pres_matched_simple,
                             model_voteshare_fullsample_simple[[2]], model_voteshare_matched_simple[[2]], 
                             model_voteshare_fullsample_simple[[3]], model_voteshare_matched_simple[[3]],
                             model_voteshare_fullsample_simple[[4]], model_voteshare_matched_simple[[4]])
model_voteshare_ses <- list(model_voteshare_fullsample_simple[[1]]$clusterse[,2], model_voteshare_matched_simple[[1]]$clusterse[,2],
                            model_voteshare_comm_fullsample_simple$clusterse[,2], model_voteshare_comm_matched_simple$clusterse[,2],
                            model_voteshare_pres_fullsample_simple$clusterse[,2], model_voteshare_pres_matched_simple$clusterse[,2],
                            model_voteshare_fullsample_simple[[2]]$clusterse[,2], model_voteshare_matched_simple[[2]]$clusterse[,2], 
                            model_voteshare_fullsample_simple[[3]]$clusterse[,2], model_voteshare_matched_simple[[3]]$clusterse[,2],
                            model_voteshare_fullsample_simple[[4]]$clusterse[,2], model_voteshare_matched_simple[[4]]$clusterse[,2])
obs <- c(length(model_voteshare_fullsample_simple[[1]]$residuals)/2,
         length(model_voteshare_matched_simple[[1]]$residuals)/2,
         length(model_voteshare_comm_fullsample_simple$residuals)/2,
         length(model_voteshare_comm_matched_simple$residuals)/2,
         length(model_voteshare_pres_fullsample_simple$residuals)/2,
         length(model_voteshare_pres_matched_simple$residuals)/2,
         length(model_voteshare_fullsample_simple[[2]]$residuals)/2,
         length(model_voteshare_matched_simple[[2]]$residuals)/2,
         length(model_voteshare_fullsample_simple[[3]]$residuals)/2,
         length(model_voteshare_matched_simple[[3]]$residuals)/2,
         length(model_voteshare_fullsample_simple[[4]]$residuals)/2,
         length(model_voteshare_matched_simple[[4]]$residuals)/2) %>% round
varnames_long <- c("\\cmidrule(r){2-3}\\cmidrule(r){4-5}\\cmidrule(r){6-7}\\cmidrule(r){8-9}\\cmidrule(r){10-11}\\cmidrule(r){12-13}Time trend", "Base rate difference", "Exposure, 10km", "(Intercept)")
election_names <- c("Sep 1930", "Sep 1930 (mun.)", "Apr 1932 (P)", "Jul 1932", "Nov 1932", "Mar 1933")
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
add.lines = list(c("Sample", rep(c("full", "matched"), 6)),
                 c("Observations", obs)),
notes        = "Diff-in-diff models with number of actual voters as population weights. Clustered SEs shown. $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01", 
notes.append = FALSE,
column.labels = election_names, 
column.separate = c(2, 2, 2, 2, 2, 2), 
font.size = "scriptsize",
title = paste0("Effects of exposure to Hitler appearance on NSDAP/Hitler vote share.\\vspace{-.25cm}"), 
label = paste0("tab:nsdap-voteshare-dd"),
out = paste0("../figures/tab-effect-voteshare-models.tex"))


# export KPD vote share models
model_voteshare_kpd_list <- list(model_voteshare_kpd_fullsample_simple[[1]], model_voteshare_kpd_matched_simple[[1]], 
                             model_voteshare_kpd_comm_fullsample_simple, model_voteshare_kpd_comm_matched_simple,
                             model_voteshare_thae_pres_fullsample_simple, model_voteshare_thae_pres_matched_simple,
                             model_voteshare_kpd_fullsample_simple[[2]], model_voteshare_kpd_matched_simple[[2]], 
                             model_voteshare_kpd_fullsample_simple[[3]], model_voteshare_kpd_matched_simple[[3]],
                             model_voteshare_kpd_fullsample_simple[[4]], model_voteshare_kpd_matched_simple[[4]])
model_voteshare_kpd_ses <- list(model_voteshare_kpd_fullsample_simple[[1]]$clusterse[,2], model_voteshare_kpd_matched_simple[[1]]$clusterse[,2],
                            model_voteshare_kpd_comm_fullsample_simple$clusterse[,2], model_voteshare_kpd_comm_matched_simple$clusterse[,2],
                            model_voteshare_thae_pres_fullsample_simple$clusterse[,2], model_voteshare_thae_pres_matched_simple$clusterse[,2],
                            model_voteshare_kpd_fullsample_simple[[2]]$clusterse[,2], model_voteshare_kpd_matched_simple[[2]]$clusterse[,2], 
                            model_voteshare_kpd_fullsample_simple[[3]]$clusterse[,2], model_voteshare_kpd_matched_simple[[3]]$clusterse[,2],
                            model_voteshare_kpd_fullsample_simple[[4]]$clusterse[,2], model_voteshare_kpd_matched_simple[[4]]$clusterse[,2])
obs <- c(length(model_voteshare_kpd_fullsample_simple[[1]]$residuals)/2,
         length(model_voteshare_kpd_matched_simple[[1]]$residuals)/2,
         length(model_voteshare_kpd_comm_fullsample_simple$residuals)/2,
         length(model_voteshare_kpd_comm_matched_simple$residuals)/2,
         length(model_voteshare_thae_pres_fullsample_simple$residuals)/2,
         length(model_voteshare_thae_pres_matched_simple$residuals)/2,
         length(model_voteshare_kpd_fullsample_simple[[2]]$residuals)/2,
         length(model_voteshare_kpd_matched_simple[[2]]$residuals)/2,
         length(model_voteshare_kpd_fullsample_simple[[3]]$residuals)/2,
         length(model_voteshare_kpd_matched_simple[[3]]$residuals)/2,
         length(model_voteshare_kpd_fullsample_simple[[4]]$residuals)/2,
         length(model_voteshare_kpd_matched_simple[[4]]$residuals)/2) %>% round
varnames_long <- c("\\cmidrule(r){2-3}\\cmidrule(r){4-5}\\cmidrule(r){6-7}\\cmidrule(r){8-9}\\cmidrule(r){10-11}\\cmidrule(r){12-13}Time trend", "Base rate difference", "Exposure, 10km", "(Intercept)")
election_names <- c("Sep 1930", "Sep 1930 (mun.)", "Apr 1932 (P)", "Jul 1932", "Nov 1932", "Mar 1933")
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
                               add.lines = list(c("Sample", rep(c("full", "matched"), 6)),
                                                c("Observations", obs)),
                               notes        = "Diff-in-diff models with number of actual voters as population weights. Clustered SEs shown. $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01", 
                               notes.append = FALSE,
                               column.labels = election_names, 
                               column.separate = c(2, 2, 2, 2, 2, 2), 
                               font.size = "scriptsize",
                               title = paste0("Effects of exposure to Hitler appearance on KPD/Thälmann vote share.\\vspace{-.25cm}"), 
                               label = paste0("tab:nsdap-voteshare-kpd-dd"),
                               out = paste0("../figures/tab-effect-voteshare-kpd-models.tex"))


# export turnout models
model_turnout_list <- list(model_turnout_fullsample_simple[[1]], model_turnout_matched_simple[[1]], 
                             model_turnout_comm_fullsample_simple, model_turnout_comm_matched_simple,
                             model_turnout_pres_fullsample_simple, model_turnout_pres_matched_simple,
                             model_turnout_fullsample_simple[[2]], model_turnout_matched_simple[[2]], 
                             model_turnout_fullsample_simple[[3]], model_turnout_matched_simple[[3]],
                             model_turnout_fullsample_simple[[4]], model_turnout_matched_simple[[4]])
model_turnout_ses <- list(model_turnout_fullsample_simple[[1]]$clusterse[,2], model_turnout_matched_simple[[1]]$clusterse[,2],
                            model_turnout_comm_fullsample_simple$clusterse[,2], model_turnout_comm_matched_simple$clusterse[,2],
                            model_turnout_pres_fullsample_simple$clusterse[,2], model_turnout_pres_matched_simple$clusterse[,2],
                            model_turnout_fullsample_simple[[2]]$clusterse[,2], model_turnout_matched_simple[[2]]$clusterse[,2], 
                            model_turnout_fullsample_simple[[3]]$clusterse[,2], model_turnout_matched_simple[[3]]$clusterse[,2],
                            model_turnout_fullsample_simple[[4]]$clusterse[,2], model_turnout_matched_simple[[4]]$clusterse[,2])
obs <- c(length(model_turnout_fullsample_simple[[1]]$residuals)/2,
         length(model_turnout_matched_simple[[1]]$residuals)/2,
         length(model_turnout_comm_fullsample_simple$residuals)/2,
         length(model_turnout_comm_matched_simple$residuals)/2,
         length(model_turnout_pres_fullsample_simple$residuals)/2,
         length(model_turnout_pres_matched_simple$residuals)/2,
         length(model_turnout_fullsample_simple[[2]]$residuals)/2,
         length(model_turnout_matched_simple[[2]]$residuals)/2,
         length(model_turnout_fullsample_simple[[3]]$residuals)/2,
         length(model_turnout_matched_simple[[3]]$residuals)/2,
         length(model_turnout_fullsample_simple[[4]]$residuals)/2,
         length(model_turnout_matched_simple[[4]]$residuals)/2) %>% round
varnames_long <- c("\\cmidrule(r){2-3}\\cmidrule(r){4-5}\\cmidrule(r){6-7}\\cmidrule(r){8-9}\\cmidrule(r){10-11}\\cmidrule(r){12-13}Time trend", "Base rate difference", "Exposure, 10km", "(Intercept)")
election_names <- c("Sep 1930", "Sep 1930 (mun.)", "Apr 1932 (P)", "Jul 1932", "Nov 1932", "Mar 1933")
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
                               add.lines = list(c("Sample", rep(c("full", "matched"), 6)),
                                                c("Observations", obs)),
                               notes        = "Diff-in-diff models with number of actual voters as population weights. Clustered SEs shown. $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01", 
                               notes.append = FALSE,
                               column.labels = election_names, 
                               column.separate = c(2, 2, 2, 2, 2, 2), 
                               font.size = "scriptsize",
                               title = paste0("Effects of exposure to Hitler appearance on turnout.\\vspace{-.25cm}"), 
                               label = paste0("tab:turnout-dd"),
                               out = paste0("../figures/tab-effect-turnout-models.tex"))


# export membership models
model_members_list <- list(model_members_fullsample_simple[[1]], model_members_matched_simple[[1]], 
                             model_members_fullsample_simple[[2]], model_members_matched_simple[[2]], 
                             model_members_fullsample_simple[[3]], model_members_matched_simple[[3]],
                             model_members_fullsample_simple[[4]], model_members_matched_simple[[4]])
model_members_ses <- list(model_members_fullsample_simple[[1]]$clusterse[,2], model_members_matched_simple[[1]]$clusterse[,2],
                            model_members_fullsample_simple[[2]]$clusterse[,2], model_members_matched_simple[[2]]$clusterse[,2], 
                            model_members_fullsample_simple[[3]]$clusterse[,2], model_members_matched_simple[[3]]$clusterse[,2],
                            model_members_fullsample_simple[[4]]$clusterse[,2], model_members_matched_simple[[4]]$clusterse[,2])
obs <- c(length(model_members_fullsample_simple[[1]]$residuals)/2,
         length(model_members_matched_simple[[1]]$residuals)/2,
         length(model_members_fullsample_simple[[2]]$residuals)/2,
         length(model_members_matched_simple[[2]]$residuals)/2,
         length(model_members_fullsample_simple[[3]]$residuals)/2,
         length(model_members_matched_simple[[3]]$residuals)/2,
         length(model_members_fullsample_simple[[4]]$residuals)/2,
         length(model_members_matched_simple[[4]]$residuals)/2) %>% round
varnames_long <- c("\\cmidrule(r){2-3}\\cmidrule(r){4-5}\\cmidrule(r){6-7}\\cmidrule(r){8-9}Time trend", "Base rate difference", "Exposure, 10km", "(Intercept)")
election_names <- c("Sep 1930", "Jul 1932", "Nov 1932", "Mar 1933")
effect_models_tex <- stargazer(model_members_list, 
                               dep.var.caption = "", #dep.var.caption = "NSDAP/Hitler vote share", 
                               omit.table.layout = "d",
                               covariate.labels = varnames_long, 
                               model.numbers = FALSE, 
                               keep=c("Constant", "^timeXexposure", "exposure", "time"),
                               omit.stat = c("rsq", "res.dev", "ser", "n", "f"),
                               no.space = TRUE,
                               df = FALSE, 
                               se = model_members_ses,
                               add.lines = list(c("Sample", rep(c("full", "matched"), 4)),
                                                c("Observations", obs)),
                               notes        = "DID models with number of actual voters as pop weights. Clustered SEs shown. $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01", 
                               notes.append = FALSE,
                               column.labels = election_names, 
                               column.separate = c(2, 2, 2, 2), 
                               font.size = "scriptsize",
                               title = paste0("Effects of exposure to Hitler appearance on NSDAP membership rates.\\vspace{-.25cm}"), 
                               label = paste0("tab:nsdap-members-dd"),
                               out = paste0("../figures/tab-effect-members-models.tex"))





## plot: coefficients of effect models ---------

pdf(file="../figures/effects_coefplot.pdf", height=3.5, width=8, family="URWTimes")
par(oma=c(.5,0,.5,0))
par(mar=c(3,7,3,1))
layout(matrix(1:3, 1, byrow = TRUE), widths=c(1.3, 1, 1), heights=c(2, 2, 2))
election_names <- rev(c("Sep 1930", "Sep 1930 (mun.)", "Apr 1932 (P)", "Jul 1932", "Nov 1932", "Mar 1933"))
#colors <- brewer.pal(4,"BrBG")
colors <- c("darkgrey", "black")
## voteshare effect plot
plot(0, 0, xlim = c(-0.07, .07), ylim = c(-.3, 5.3), pch = 20, col = "white", xlab = "", ylab = "", yaxt = "n", xaxt = "n")
axis(2, at = 0:5, labels = F, tick = F)
text(y = 0:5, par("usr")[1], labels = election_names, srt = 0, pos = 2, xpd = TRUE)
axis(1, at = seq(-.06, .06, .03), labels = seq(-.06, .06, .03), tick = T)
mtext("NSDAP/Hitler vote share", side = 3, line = 1.2, outer = FALSE)
abline(v = 0, lty = 2)
abline(h = c(.5, 1.5, 2.5, 3.5, 4.5), lty = 1, col = "darkgrey")
# 1930 election
y_par <- 5
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
y_par <- 4
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
# 1932(P) election
y_par <- 3
mod_input <- model_voteshare_pres_fullsample_simple
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.15, x1 = mod$coef_hardhi, y1 = y_par+.15, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.15, x1 = mod$coef_weakhi, y1 = y_par+.15, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.15, pch = 20, col = colors[1])
mod_input <- model_voteshare_pres_matched_simple
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
plot(0, 0, xlim = c(-0.07, .07), ylim = c(-.3, 5.3), pch = 20, col = "white", xlab = "", ylab = "", yaxt = "n", xaxt = "n")
axis(1, at = seq(-.06, .06, .03), labels = seq(-.06, .06, .03), tick = T)
mtext("KPD/Thälmann vote share", side = 3, line = 1.2, outer = FALSE)
abline(v = 0, lty = 2)
abline(h = c(.5, 1.5, 2.5, 3.5, 4.5), lty = 1, col = "darkgrey")
# 1930 election
y_par <- 5
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
y_par <- 4
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
# 1932(P) election
y_par <- 3
mod_input <- model_voteshare_thae_pres_fullsample_simple
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.15, x1 = mod$coef_hardhi, y1 = y_par+.15, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.15, x1 = mod$coef_weakhi, y1 = y_par+.15, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.15, pch = 20, col = colors[1])
mod_input <- model_voteshare_thae_pres_matched_simple
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
plot(0, 0, xlim = c(-0.07, .07), ylim = c(-.3, 5.3), pch = 20, col = "white", xlab = "", ylab = "", yaxt = "n", xaxt = "n")
axis(1, at = seq(-.06, .06, .03), labels = seq(-.06, .06, .03), tick = T)
mtext("Turnout", side = 3, line = 1.2, outer = FALSE)
abline(v = 0, lty = 2)
abline(h = c(.5, 1.5, 2.5, 3.5, 4.5), lty = 1, col = "darkgrey")
# 1930 election
y_par <- 5
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
y_par <- 4
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
# 1932(P) election
y_par <- 3
mod_input <- model_turnout_pres_fullsample_simple
mod <- getCoef("mod_input")
arrows(x0 = mod$coef_hardlo, y0 = y_par+.15, x1 = mod$coef_hardhi, y1 = y_par+.15, length = 0, col = colors[1])
arrows(x0 = mod$coef_weaklo, y0 = y_par+.15, x1 = mod$coef_weakhi, y1 = y_par+.15, length = 0, lwd = 2, col = colors[1])
points(mod$coef_point, y_par+.15, pch = 20, col = colors[1])
mod_input <- model_turnout_pres_matched_simple
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


