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
  "community_df_pointdata_long.RData",
  "nazi_wgs84.RData",
  "airfields_certainly_before_32_df.RData",
  "speeches_df.RData",
  "elections_geopolygons_df.RData",
  "icons/airport.png")

## exports
c("../figures/tab-summary-stats-1928.tex",
  "../figures/tab-summary-stats-1930.tex",
  "../figures/tab-summary-stats-1932r1.tex",
  "../figures/tab-summary-stats-1932r2.tex",
  "../figures/tab-summary-stats-1933.tex",
  "../figures/tab-summary-stats-1932p2.tex",
  "../figures/tab-summary-stats-1930comm.tex")



## load packages and functions -------------------------------
source("packages.r")
source("functions.r")


## table: variable summary statistics by election ------------------------------

load("county_df_pointdata_long.RData")
tab_summary <- dplyr::select(county_df_long, visit_5km, visit_10km, visit_25km, visit_50km, p_nsdap, p_turnout, wkr_comp_last, wkv_comp_last, wbht, memberst, airfields_dist_min100km, goebbels_10km, election) %>% dplyr::filter(election != 0)
election <- c("1928", "1930", "1932r1", "1932r2", "1933")
election_name <-  c("May 1928", "Sep 1930", "Jul 1932", "Nov 1932", "Mar 1933")
for (i in 2:5) {
  summary_table_tex <- stargazer(filter(tab_summary, election == i), omit = "election", covariate.labels = c("Appearance within 5km radius", "Appearance within 10km radius", "Appearance within 25km radius","Appearance within 50km radius", "Previous NSDAP vote share", "Turnout", "Competitiveness 1", "Competitiveness 2", "Number of eligibles", "Organizational strength", "Distance to nearest airfield", "Goebbels appearance"),style = "apsr", title=paste0("Summary statistics, ", election_name[i], " election"), label = paste0("tab:sumstats", election[i]), out = paste0("../figures/tab-summary-stats-", election[i], ".tex"))
}

load("county_pres_df_pointdata_long.RData")
tab_pres_summary <- dplyr::select(county_pres_df_long, visit_5km, visit_10km, visit_25km, visit_50km, p_nsdap, p_turnout, wbht, memberst, airfields_dist_min100km, goebbels_10km, election) %>% dplyr::filter(election != 6)
summary_pres_table_tex <- stargazer(tab_pres_summary, omit = "election", covariate.labels = c("Appearance within 5km radius", "Appearance within 10km radius", "Appearance within 25km radius","Appearance within 50km radius", "Previous NSDAP vote share", "Turnout", "Number of eligibles", "Organizational strength", "Distance to nearest airfield", "Goebbels appearance"),style = "apsr", title="Summary statistics, 1932 Presidential election (2nd round)", label = "tab:sumstats1932pres", out = paste0("../figures/tab-summary-stats-", "1932p2", ".tex"))

load("community_df_pointdata_long.RData")
tab_community_summary <- dplyr::select(community_df_long, visit_5km, visit_10km, visit_25km, visit_50km, p_nsdap, p_turnout, wkr_comp_last, wkv_comp_last, wbht, memberst, airfields_dist_min100km, goebbels_10km, election) %>% dplyr::filter(election != 1)
summary_pres_table_tex <- stargazer(tab_community_summary, omit = "election", covariate.labels = c("Appearance within 5km radius", "Appearance within 10km radius", "Appearance within 25km radius","Appearance within 50km radius", "Previous NSDAP vote share", "Turnout", "Competitiveness 1", "Competitiveness 2", "Number of eligibles", "Organizational strength", "Distance to nearest airfield", "Goebbels appearance"), style = "apsr", title="Summary statistics, 1930 election (municipality-level data)", label = "tab:sumstats1930comm", out = paste0("../figures/tab-summary-stats-", "1930comm", ".tex"))


### table: treatment assignment ------------------------

load("county_df_pointdata_long.RData")

# inspect number of cases by election
# function to count observations by treatment status
identifyTreatmentStatus <- function(elec, treatment_var) {
  nomatch_var <- paste0(treatment_var, "_nomatch")
  dat <- filter(county_df_long, election == elec)
  treated_unit <- table(dat[,treatment_var])[2]
  buffer_unit <- table(dat[,nomatch_var])[2]
  control_unit <- sum(table(dat[,treatment_var])) - treated_unit - buffer_unit
  nums <- cbind(treated_unit, buffer_unit, control_unit) %>% t
  return(nums)
}

# set up and fill data frame
tab <- matrix(ncol = 20, nrow = 4*3)
treatment_vars <- c("visit_5km", "visit_12w_5km", "visit_8w_5km", "visit_4w_5km", "visit_2w_5km",
                    "visit_10km", "visit_12w_10km", "visit_8w_10km", "visit_4w_10km", "visit_2w_10km",
                    "visit_25km", "visit_12w_25km", "visit_8w_25km", "visit_4w_25km", "visit_2w_25km",
                    "visit_50km", "visit_12w_50km", "visit_8w_50km", "visit_4w_50km", "visit_2w_50km")
row_num <- 1
col_num <- 1

for (i in 1:4) {
  for (j in treatment_vars) {
    tab[row_num:(row_num+2), col_num] <- identifyTreatmentStatus(i+1, j) %>% num()
    col_num <- col_num + 1
  }
  col_num <- 1
  row_num <- row_num + 3
}

# add row descriptions
election_column <- c("\\multirow{3}{*}{Sep 1930}", "", "", "\\multirow{3}{*}{Jul 1932}", "", "",  "\\multirow{3}{*}{Nov 1932}", "", "", "\\multirow{3}{*}{Mar 1933}", "", "")
treatment_status <- c("Exposed", "Buffer", "Control")
treatment_column <- rep(treatment_status, 4)
tab_tex <- cbind(election_column, treatment_column, tab)
tab_tex1 <- tab_tex[,1:12]
tab_tex2 <- tab_tex[,c(1,2, 13:22)]

# print table
addtorow <- list()
addtorow$pos <- list(0)
addtorow$command <- paste0(paste0(' & & \\multicolumn{5}{c}{5km radius of exposure} &  \\multicolumn{5}{c}{10km radius of exposure}', collapse=''), '\\\\',
                           paste0('\\cmidrule(r){3-7} \\cmidrule(r){8-12}\\textit{Election} & \\textit{Status} & \\textit{Full period} & \\ \\textit{12 weeks} & \\textit{8 weeks} & \\textit{4 weeks} & \\textit{2 weeks} & \\textit{Full period} & \\ \\textit{12 weeks} & \\textit{8 weeks} & \\textit{4 weeks} & \\textit{2 weeks}', collapbse=''), '\\\\')
cols_align <- c("l", "l", rep("r", ncol(tab_tex1)-1))
print(xtable(tab_tex1, align = cols_align, digits=2, caption = "Exposure, buffer, and control units, by election and exposure variable specification.\\label{tab:treatmentvars}"), booktabs = TRUE, size = "scriptsize", caption.placement = "top", table.placement = "h!", add.to.row=addtorow, hline.after = c(-1,0,3,6,9,nrow(tab_tex1)), include.rownames=FALSE, include.colnames = FALSE, sanitize.text.function = identity, file = "../figures/tab-treatmentvars.tex")

addtorow <- list()
addtorow$pos <- list(0)
addtorow$command <- paste0(paste0(' & & \\multicolumn{5}{c}{25km radius of exposure} &  \\multicolumn{5}{c}{50km radius of exposure}', collapse=''), '\\\\',
                           paste0('\\cmidrule(r){3-7} \\cmidrule(r){8-12}\\textit{Election} & \\textit{Status} & \\textit{Full period} & \\ \\textit{12 weeks} & \\textit{8 weeks} & \\textit{4 weeks} & \\textit{2 weeks} & \\textit{Full period} & \\ \\textit{12 weeks} & \\textit{8 weeks} & \\textit{4 weeks} & \\textit{2 weeks}', collapbse=''), '\\\\')
print(xtable(tab_tex2, align = cols_align, digits=2, caption = "Exposure, buffer, and control units, by election and exposure variable specification, \\textit{continued}.\\label{tab:treatmentvars2}"), booktabs = TRUE, size = "scriptsize", caption.placement = "top", table.placement = "h!", add.to.row=addtorow, hline.after = c(-1,0,3,6,9,nrow(tab_tex2)), include.rownames=FALSE, include.colnames = FALSE, sanitize.text.function = identity, file = "../figures/tab-treatmentvars2.tex")
  


### plot: locations of geo-coded communities, counties, and county boroughs -----------

# load map data
load("nazi_wgs84.RData")

pdf(file="../figures/map_communities_points.pdf", height=7.5, width=10, family="URWTimes")
par(oma=c(0,0,0,0))
par(mar=c(1.5,2,1.5,.5))
par(xaxs = "i", yaxs = "i")
# plot boundaries
plot(nazi_wgs84, col = "white", border = "darkgrey")
# plot points of geo-coded communities; weighted by population size
communities_sp  <- SpatialPoints(cbind(community_df_long$long, community_df_long$lat))
pop_weight <- scales::rescale(community_df_long$pop, to = c(.6, 6)) # vary point size with log population weight
pop_weight[is.na(pop_weight)] <- 1
points(coordinates(communities_sp)[,1], coordinates(communities_sp)[,2], pch = 20, cex = pop_weight, col = rgb(1, 133, 113, 150, max = 255))
dev.off()
nrow(coordinates(communities_sp))

pdf(file="../figures/map_counties_points.pdf", height=7.5, width=10, family="URWTimes")
par(oma=c(0,0,0,0))
par(mar=c(1.5,2,1.5,.5))
par(xaxs = "i", yaxs = "i")
# plot boundaries
plot(nazi_wgs84, col = "white", border = "darkgrey")
# plot points of geo-coded communities; weighted by population size
county_df_long <- filter(county_df_long, election ==2)
counties_sp  <- SpatialPoints(cbind(county_df_long$long, county_df_long$lat))
pop_weight <- scales::rescale(county_df_long$pop, to = c(.6, 6)) # vary point size with log population weight
pop_weight[is.na(pop_weight)] <- 1
points(coordinates(counties_sp)[,1], coordinates(counties_sp)[,2], pch = 20, cex = pop_weight, col = rgb(150, 80, 20, 150, max = 255))
dev.off()
nrow(coordinates(counties_sp))




### plot: maps of election returns -----------

# load map data
load("nazi_wgs84.RData")

# load airfields data
load("airfields_certainly_before_32_df.RData")
airfields_sp <- SpatialPoints(cbind(airfields_df$longitude, airfields_df$latitude))
projection <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4string(airfields_sp) <- CRS(projection)

# load speeches data
load("speeches_df.RData")
speeches_df$date <- as.Date(speeches_df$date)

# load geo dataframe
load("elections_geopolygons_df.RData")

# load images
airport <- readPNG('./icons/airport.png')

# compute differences in election returns
elections_geo_df$n309diff <- (elections_geo_df$n309nsda/elections_geo_df$n309pop) - (elections_geo_df$n285nsda/elections_geo_df$n285pop)
elections_geo_df$n327diff <- (elections_geo_df$n327nsda/elections_geo_df$n327pop) - (elections_geo_df$n309nsda/elections_geo_df$n309pop)
elections_geo_df$n32ndiff <- (elections_geo_df$n32nnsda/elections_geo_df$n32npop) - (elections_geo_df$n327nsda/elections_geo_df$n327pop)
elections_geo_df$n333diff <- (elections_geo_df$n333nsda/elections_geo_df$n333pop) - (elections_geo_df$n32nnsda/elections_geo_df$n32npop)

# merge election_geo_df with nazi_wgs84
members_geo_df <- merge(elections_geo_df, nazi_wgs84, all.y = TRUE, by.x = "krnr_loughlin", by.y = "KRNR")
members_geo_df <- members_geo_df[!is.na(members_geo_df$krnr_loughlin),]
members_geo_df <- members_geo_df[!duplicated(members_geo_df$krnr_loughlin), ]
members_geo_df <- members_geo_df[order(match(members_geo_df$krnr_loughlin,nazi_wgs84$KRNR)),]

nazi_wgs84$n309diff <- na.locf(members_geo_df$n309diff) # fill missings with last valid observation; only for display purposes (some districts are missing due to imperfect match of Falter with polygon data)
nazi_wgs84$n327diff <- na.locf(members_geo_df$n327diff)
nazi_wgs84$n32ndiff <- na.locf(members_geo_df$n32ndiff)
nazi_wgs84$n333diff <- na.locf(members_geo_df$n333diff)


## plot, 1930
pdf(file="../figures/map_nsdap_votesharediff_309.pdf", height=7.5, width=10, family="URWTimes")
#pdf(file="../figures/elect_returns_309_map_ger.pdf", height=7.5, width=10, family="URWTimes")
par(oma=c(0,0,0,0))
par(mar=c(1.5,2,1.5,.5))
par(xaxs = "i", yaxs = "i")
# plot boundaries
# plot values of continuous variable
nazi_wgs84$n309diff_cat <- findInterval(nazi_wgs84$n309diff, seq(-.20, .20, .04))
cats <- length(unique(nazi_wgs84$n309diff_cat))
cols <- brewer.pal(n = 11, name = "RdYlGn") # maximum of 11 categories
col_vec <- vector() # set colors
for (i in 1:nrow(nazi_wgs84)) {
  col_vec[i] <- cols[nazi_wgs84$n309diff_cat[i]]
}
plot(nazi_wgs84, col = col_vec, border = "darkgrey")
# plot speeches
#speeches_df_309 <- filter(speeches_df, date > ymd("1930-09-14") - weeks(12), date < "1930-09-14")
speeches_df_309 <- filter(speeches_df, date > ymd("1928-05-20"), date < "1930-09-14")
speeches_df_309_sp  <- SpatialPoints(cbind(speeches_df_309$lon, speeches_df_309$lat))
offset <- .15
dat <- speeches_df_309_sp
points(coordinates(dat)[,1], coordinates(dat)[,2], pch = 20, col = "red")
# add legend
colorbar.plot(x = 20, y = 47.5, strip.width = .1, seq_along(cols), col = cols,  adj.y = 0.8)
text(x = 17.5, y = 47.8, "Change in NSDAP vote share from past election", cex=.8, adj=0)
#text(x = 17.5, y = 47.8, "Veränderung des NSDAP-Wähleranteils", cex=.8, adj=0)
mtext("-20%", side = 1, outer = FALSE, at = 17.6, cex = .8)
mtext("-12%", side = 1, outer = FALSE, at = 18.55, cex = .8)
mtext("-4%", side = 1, outer = FALSE, at = 19.5, cex = .8)
mtext("+4%", side = 1, outer = FALSE, at = 20.45, cex = .8)
mtext("+12%", side = 1, outer = FALSE, at = 21.4, cex = .8)
mtext("+20%", side = 1, outer = FALSE, at = 22.3, cex = .8)
dev.off()

## plot 1932 1
pdf(file="../figures/map_nsdap_votesharediff_327.pdf", height=7.5, width=10, family="URWTimes")
#pdf(file="../figures/elect_returns_327_map_ger.pdf", height=7.5, width=10, family="URWTimes")
par(oma=c(0,0,0,0))
par(mar=c(1.5,2,1.5,.5))
par(xaxs = "i", yaxs = "i")
# plot boundaries
# plot values of continuous variable
nazi_wgs84$n327diff_cat <- findInterval(nazi_wgs84$n327diff, seq(-.20, .20, .04))
cats <- length(unique(nazi_wgs84$n327diff_cat))
cols <- brewer.pal(n = 11, name = "RdYlGn") # maximum of 11 categories

col_vec <- vector() # set colors
for (i in 1:nrow(nazi_wgs84)) {
  col_vec[i] <- cols[nazi_wgs84$n327diff_cat[i]]
}
plot(nazi_wgs84, col = col_vec, border = "darkgrey")
# plot speeches
speeches_df_327 <- filter(speeches_df, date > ymd("1930-09-14"), date < "1932-07-31")
speeches_df_327_sp  <- SpatialPoints(cbind(speeches_df_327$lon, speeches_df_327$lat))
offset <- .15
dat <- speeches_df_327_sp
points(coordinates(dat)[,1], coordinates(dat)[,2], pch = 20, col = "red")
# add legend
colorbar.plot(x = 20, y = 47.5, strip.width = .1, seq_along(cols), col = cols,  adj.y = 0.8)
text(x = 17.5, y = 47.8, "Change in NSDAP vote share from past election", cex=.8, adj=0)
#text(x = 17.5, y = 47.8, "Veränderung des NSDAP-Wähleranteils", cex=.8, adj=0)
mtext("-20%", side = 1, outer = FALSE, at = 17.6, cex = .8)
mtext("-12%", side = 1, outer = FALSE, at = 18.55, cex = .8)
mtext("-4%", side = 1, outer = FALSE, at = 19.5, cex = .8)
mtext("+4%", side = 1, outer = FALSE, at = 20.45, cex = .8)
mtext("+12%", side = 1, outer = FALSE, at = 21.4, cex = .8)
mtext("+20%", side = 1, outer = FALSE, at = 22.3, cex = .8)
dev.off()

## plot, 1932 2
pdf(file="../figures/map_nsdap_votesharediff_32n.pdf", height=7.5, width=10, family="URWTimes")
#pdf(file="../figures/elect_returns_32n_map_ger.pdf", height=7.5, width=10, family="URWTimes")
par(oma=c(0,0,0,0))
par(mar=c(1.5,2,1.5,.5))
par(xaxs = "i", yaxs = "i")
# plot boundaries
# plot values of continuous variable
nazi_wgs84$n32ndiff_cat <- findInterval(nazi_wgs84$n32ndiff, seq(-.20, .20, .04))
nazi_wgs84$n32ndiff_cat[nazi_wgs84$n32ndiff_cat==0] <- 1
cats <- length(unique(nazi_wgs84$n32ndiff_cat))
cols <- brewer.pal(n = 11, name = "RdYlGn") # maximum of 11 categories

col_vec <- vector() # set colors
for (i in 1:nrow(nazi_wgs84)) {
  col_vec[i] <- cols[nazi_wgs84$n32ndiff_cat[i]]
}
plot(nazi_wgs84, col = col_vec, border = "darkgrey")
# plot speeches
#speeches_df_32n <- filter(speeches_df, date > ymd("1932-11-06") - weeks(12), date < "1932-11-06")
speeches_df_32n <- filter(speeches_df, date > ymd("1932-07-31"), date < "1932-11-06")
speeches_df_32n_sp  <- SpatialPoints(cbind(speeches_df_32n$lon, speeches_df_32n$lat))
offset <- .15
dat <- speeches_df_32n_sp
points(coordinates(dat)[,1], coordinates(dat)[,2], pch = 20, col = "red")
# add legend
colorbar.plot(x = 20, y = 47.5, strip.width = .1, seq_along(cols), col = cols,  adj.y = 0.8)
text(x = 17.5, y = 47.8, "Change in NSDAP vote share from past election", cex=.8, adj=0)
#text(x = 17.5, y = 47.8, "Veränderung des NSDAP-Wähleranteils", cex=.8, adj=0)
mtext("-20%", side = 1, outer = FALSE, at = 17.6, cex = .8)
mtext("-12%", side = 1, outer = FALSE, at = 18.55, cex = .8)
mtext("-4%", side = 1, outer = FALSE, at = 19.5, cex = .8)
mtext("+4%", side = 1, outer = FALSE, at = 20.45, cex = .8)
mtext("+12%", side = 1, outer = FALSE, at = 21.4, cex = .8)
mtext("+20%", side = 1, outer = FALSE, at = 22.3, cex = .8)
dev.off()

## plot, 1933
pdf(file="../figures/map_nsdap_votesharediff_333.pdf", height=7.5, width=10, family="URWTimes")
#pdf(file="../figures/elect_returns_333_map_ger.pdf", height=7.5, width=10, family="URWTimes")
par(oma=c(0,0,0,0))
par(mar=c(1.5,2,1.5,.5))
par(xaxs = "i", yaxs = "i")
# plot boundaries
# plot values of continuous variable
nazi_wgs84$n333diff_cat <- findInterval(nazi_wgs84$n333diff, seq(-.20, .20, .04))
nazi_wgs84$n333diff_cat[nazi_wgs84$n333diff_cat==0] <- 1
cats <- length(unique(nazi_wgs84$n333diff_cat))
cols <- brewer.pal(n = 11, name = "RdYlGn") # maximum of 11 categories

col_vec <- vector() # set colors
for (i in 1:nrow(nazi_wgs84)) {
  col_vec[i] <- cols[nazi_wgs84$n333diff_cat[i]]
}
plot(nazi_wgs84, col = col_vec, border = "darkgrey")
# plot speeches
#speeches_df_333 <- filter(speeches_df, date > ymd("1933-03-05") - weeks(12), date < "1933-03-05")
speeches_df_333 <- filter(speeches_df, date > ymd("1932-11-06"), date < "1933-03-05")
speeches_df_333_sp  <- SpatialPoints(cbind(speeches_df_333$lon, speeches_df_333$lat))
offset <- .15
dat <- speeches_df_333_sp
points(coordinates(dat)[,1], coordinates(dat)[,2], pch = 20, col = "red")
# add legend
colorbar.plot(x = 20, y = 47.5, strip.width = .1, seq_along(cols), col = cols,  adj.y = 0.8)
text(x = 17.5, y = 47.8, "Change in NSDAP vote share from past election", cex=.8, adj=0)
#text(x = 17.5, y = 47.8, "Veränderung des NSDAP-Wähleranteils", cex=.8, adj=0)
mtext("-20%", side = 1, outer = FALSE, at = 17.6, cex = .8)
mtext("-12%", side = 1, outer = FALSE, at = 18.55, cex = .8)
mtext("-4%", side = 1, outer = FALSE, at = 19.5, cex = .8)
mtext("+4%", side = 1, outer = FALSE, at = 20.45, cex = .8)
mtext("+12%", side = 1, outer = FALSE, at = 21.4, cex = .8)
mtext("+20%", side = 1, outer = FALSE, at = 22.3, cex = .8)
dev.off()



## plot: timeline of election returns ---------------

# import prepared data
load("elections_all_df_pointdata_long.RData")

# select variables
elections_df_long <- dplyr::select(elections_df_long, election, wkv, wkr, krnr, lfnr, agglvl, name, lat, long, airfields_dist_min, members, nsdap, kpd, pop, wb, gs, p_nsdap, p_kpd, p_hitl, p_turnout, p_members, visit_5km, visit_10km, visit_25km, visit_50km)

# subset data; only counties
county_df_long <- filter(elections_df_long, str_detect(agglvl, "stadtkreise|KREISE M.GEMEINDEN >|KREISE O\\.GEMEINDEN")) 
county_df_long <- filter(county_df_long, election >= 1 &  election <= 5)

# add lead visit variable
county_df_long <- 
  county_df_long %>%
  group_by(lfnr) %>%
  mutate(l.visit_10km = lead(visit_10km, 1))

# add national-level statistics
county_df_long_el <-  group_by(county_df_long, election)
county_df_long_sum <- summarize(county_df_long_el, 
                                nsdap_nat = sum(nsdap, na.rm = TRUE) / sum(wb, na.rm = TRUE),
                                nsdap_untreated_nat = sum(nsdap[visit_10km != 1], na.rm = TRUE) / sum(wb[visit_10km != 1], na.rm = TRUE),
                                nsdap_treated_nat = sum(nsdap[visit_10km == 1], na.rm = TRUE) / sum(wb[visit_10km == 1], na.rm = TRUE),
                                
                                turnout_nat = sum(gs, na.rm = TRUE) / sum(wb, na.rm = TRUE))


pdf(file="../figures/elect_returns_timeline.pdf", height=6, width=8, family="URWTimes")
par(oma=c(0,0,0,0))
par(mar=c(3,4,1,3))
par(xaxs = "i", yaxs = "i")
election_names <-  c("May 1928", "Sep 1930", "Jul 1932", "Nov 1932", "Mar 1933")
plot(county_df_long$election, county_df_long$p_nsdap, cex=0, axes=F, ann = F, ylim=c(0,.8), xlim = c(.95, 5.05))
axis(1, las=0, at=seq(1,5,1), election_names, cex=.8)
axis(2, las=1, at=seq(0,.8,.2), label=seq(0,.8,.2), cex=.8)
axis(4, las=1, at=seq(0,.8,.2), label=seq(0,.8,.2), cex=.8)
title(ylab="NSDAP vote share", xlab="Election")
abline(h=seq(0, 1, .2), lty = 2, col = "darkgrey")
for (i in 1:4) {
  dat <- filter(county_df_long, election == i | election == i+1)
  for (i in unique(dat$lfnr)){
    lines(dat$election[dat$lfnr==i], dat$p_nsdap[dat$lfnr==i], type = "b", lwd=1, col=rgb(0,0,0, alpha=0.1))
    lines(dat$election[(dat$lfnr==i & dat$l.visit_10km == 1) | (dat$lfnr==i & dat$visit_10km == 1)], dat$p_nsdap[(dat$lfnr==i & dat$l.visit_10km == 1) | (dat$lfnr==i & dat$visit_10km == 1)], type = "b", lwd=1, col=rgb(.9,0,0, alpha=0.4))
  }
}
lines(county_df_long_sum$election, county_df_long_sum$nsdap_nat, type = "b", lwd = 3, lty = 5, col="white")
lines(county_df_long_sum$election, county_df_long_sum$nsdap_untreated_nat, type = "b", lwd = 3, lty = 5, col="black")
lines(county_df_long_sum$election, county_df_long_sum$nsdap_treated_nat, type = "b", lwd = 3, lty = 5, col="red")
dev.off()



