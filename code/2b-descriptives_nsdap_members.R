#----------------------------------------------------------------
# Examining a Most Likely Case for Strong Campaign Effects: 
# Hitler's Speeches and the Rise of the Nazi Party, 1927--1933
# Peter Selb (University of Konstanz)
# Simon Munzert (MZES, University of Mannheim)
# 2016
#----------------------------------------------------------------

## imports
c("members_df_wkr_wide.RData",
  "elections_geopolygons_df.RData",
  "map_oloughlin/nazi_wgs84.shp")

## exports
c("../figures/nsdap_membership_estimates.pdf",
  "../figures/tab-memberscount.tex",
  "../figures/nsdap_members_vs_election.pdf",
  "../figures/nsdap_members_309_map.pdf",
  "../figures/nsdap_members_327_map.pdf",
  "../figures/nsdap_members_32n_map.pdf",
  "../figures/nsdap_members_33_map.pdf")

## load packages and functions -------------------
source("packages.r")
source("functions.r")

# load data
load("members_df_wkr_wide.RData")


## plot: membership estimates, raw counts vs. population weight estimates ----
cor(members_df_wkr$n_raw, members_df_wkr$n_wgt)
pdf(file="../figures/nsdap_membership_estimates.pdf", height=5, width=5, family="URWTimes")
par(oma=c(1,1,0,0))
par(mar=c(4,4,1.5,.5))
plot(members_df_wkr$n_raw, members_df_wkr$n_wgt, xlab = "Raw sample counts", ylab = "Population weight-based estimates")
grid(col = "black", lty = "dotted", lwd = 2)
dev.off()


## table: cumulative membership estimates at election dates -----------------
election_dates <- ymd(c("1928-05-20", "1930-09-14", "1932-03-13", "1932-04-10", "1932-07-31", "1932-11-06", "1933-03-05"))
election_dates_df <- data.frame(election_dates, election_label = c("28", "30", "32p1", "32p2", "32r1", "32r2", "33"))

members_count <- c(
  sum(members_df_wkr_wide$n_wgt.28cum),
  sum(members_df_wkr_wide$n_wgt.30cum),  
  sum(members_df_wkr_wide$n_wgt.32p1cum),
  sum(members_df_wkr_wide$n_wgt.32p2cum),
  sum(members_df_wkr_wide$n_wgt.32r1cum),
  sum(members_df_wkr_wide$n_wgt.32r2cum),
  sum(members_df_wkr_wide$n_wgt.33cum)) %>% round()
natcount_tab <- data.frame(election_dates, members_count)

xtable <- function(x, ...) {
  for (i in which(sapply(x, function(y) !all(is.na(match(c("POSIXt","Date"),class(y))))))) x[[i]] <- as.character(x[[i]])
  xtable::xtable(x, ...)
}

colnames(natcount_tab) <- c("Election", "Estimate")
print(xtable(natcount_tab, align = c("l","l","r"), digits=0, caption = "Estimate of aggregate number of NSDAP members before Reichstag/Presidential elections, 1928-1933.\\label{tab:memberscount}"), booktabs = TRUE, size = "small", caption.placement = "top", table.placement = "t!", include.rownames=FALSE, file = "../figures/tab-memberscount.tex")


# plot: election results versus NSDAP membership rates ---------
load("elections_geopolygons_df.RData")
#elections_geo_df <- merge(elections_geo_df, members_df_wkr_wide, by = "krnr", all = TRUE)
elections_geo_df_sub <- select(elections_geo_df, krnr, name, n_wgt.32r1cum, n327nsda, n327pop)
elections_geo_df_sub$nsdap32_membershare <- elections_geo_df_sub$n_wgt.32r1cum / elections_geo_df_sub$n327pop
elections_geo_df_sub$nsdap32_voteshare <- elections_geo_df_sub$n327nsda / elections_geo_df_sub$n327pop

cor(elections_geo_df_sub$nsdap32_membershare, elections_geo_df_sub$nsdap32_voteshare, use = "pairwise.complete.obs")
pdf(file="../figures/nsdap_members_vs_election.pdf", height=5, width=5, family="URWTimes")
par(oma=c(1,1,0,0))
par(mar=c(4,4,1.5,.5))
plot(elections_geo_df_sub$nsdap32_membershare, elections_geo_df_sub$nsdap32_voteshare, xlab = "Estimated NSDAP membership, 1932", ylab = "NSDAP vote share at July 1932 Reichstag election")
grid(col = "black", lty = "dotted", lwd = 2)
dev.off()



# plot: geographical distribution of NSDAP membership rates -------

# import district polygon data
nazi_wgs84 <- readOGR("data_oloughlin/nazi_wgs84.shp", layer="nazi_wgs84")
projection <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
nazi_wgs84 <- sp::spTransform(nazi_wgs84, CRS(projection))
nazi_wgs84 <- nazi_wgs84[!duplicated(nazi_wgs84$KRNR), ]

# merge election data with geographic data
members_geo_df <- merge(elections_geo_df, nazi_wgs84, all.y = TRUE, by.x = "krnr_loughlin", by.y = "KRNR")
members_geo_df <- members_geo_df[!is.na(members_geo_df$krnr_loughlin),]
members_geo_df <- members_geo_df[!duplicated(members_geo_df$krnr_loughlin), ]
members_geo_df <- members_geo_df[order(match(members_geo_df$krnr_loughlin,nazi_wgs84$KRNR)),]
nazi_wgs84$n_wgt.28 <- na.locf(members_geo_df$n_wgt.28cum / members_geo_df$n285pop) # fill missings with last valid observation; only for display purposes (some districts are missing due to imperfect match of Falter with polygon data)
nazi_wgs84$n_wgt.30 <- na.locf(members_geo_df$n_wgt.30cum / members_geo_df$n309pop)
nazi_wgs84$n_wgt.32r1 <- na.locf(members_geo_df$n_wgt.32r1cum / members_geo_df$n327pop)
nazi_wgs84$n_wgt.32r2 <- na.locf(members_geo_df$n_wgt.32r2cum / members_geo_df$n32npop)
nazi_wgs84$n_wgt.33 <- na.locf(members_geo_df$n_wgt.33cum / members_geo_df$n333pop)

# plot 
pdf(file="../figures/map_nsdap_members_309.pdf", height=7.5, width=10, family="URWTimes")
par(oma=c(0,0,0,0))
par(mar=c(1.5,2,1.5,.5))
par(xaxs = "i", yaxs = "i")
# plot boundaries
# plot values of continuous variable
nazi_wgs84$members.30cum_cat <- findInterval(nazi_wgs84$n_wgt.30, c(0, 0.003, 0.006, 0.009, 0.012, 0.015, 0.018, 0.021, 0.030, 0.052))
cats <- length(unique(nazi_wgs84$members.30cum_cat))
cols <- brewer.pal(n = cats, name = "YlOrRd")

col_vec <- vector() # set colors
for (i in 1:nrow(nazi_wgs84)) {
  col_vec[i] <- cols[nazi_wgs84$members.30cum_cat[i]]
}
plot(nazi_wgs84, col = col_vec, border = "darkgrey")
# add legend
colorbar.plot(x = 20, y = 47.5, strip.width = .1, seq_along(cols), col = cols,  adj.y = 0.8)
text(x = 19.7, y = 47.8, "NSDAP membership", cex = 1.3)
mtext("0%", side = 1, outer = FALSE, at = 17.7, cex = 1.3)
mtext("1.2%", side = 1, outer = FALSE, at = 20, cex = 1.3)
mtext(">3%", side = 1, outer = FALSE, at = 22.2, cex = 1.3)
dev.off()


pdf(file="../figures/map_nsdap_members_327.pdf", height=7.5, width=10, family="URWTimes")
par(oma=c(0,0,0,0))
par(mar=c(1.5,2,1.5,.5))
par(xaxs = "i", yaxs = "i")
# plot boundaries
# plot values of continuous variable
nazi_wgs84$members.32r1cum_cat <- findInterval(nazi_wgs84$n_wgt.32r1, c(0, 0.003, 0.006, 0.009, 0.012, 0.015, 0.018, 0.021, 0.030, 0.052))
cats <- length(unique(nazi_wgs84$members.32r1cum_cat))
cols <- brewer.pal(n = cats, name = "YlOrRd")

col_vec <- vector() # set colors
for (i in 1:nrow(nazi_wgs84)) {
  col_vec[i] <- cols[nazi_wgs84$members.32r1cum_cat[i]]
}
plot(nazi_wgs84, col = col_vec, border = "darkgrey")
# add legend
colorbar.plot(x = 20, y = 47.5, strip.width = .1, seq_along(cols), col = cols,  adj.y = 0.8)
text(x = 19.7, y = 47.8, "NSDAP membership", cex = 1.3)
mtext("0%", side = 1, outer = FALSE, at = 17.7, cex = 1.3)
mtext("1.2%", side = 1, outer = FALSE, at = 20, cex = 1.3)
mtext(">3%", side = 1, outer = FALSE, at = 22.2, cex = 1.3)
dev.off()


pdf(file="../figures/map_nsdap_members_32n.pdf", height=7.5, width=10, family="URWTimes")
par(oma=c(0,0,0,0))
par(mar=c(1.5,2,1.5,.5))
par(xaxs = "i", yaxs = "i")
# plot boundaries
# plot values of continuous variable
nazi_wgs84$members.32r2cum_cat <- findInterval(nazi_wgs84$n_wgt.32r2, c(0, 0.003, 0.006, 0.009, 0.012, 0.015, 0.018, 0.021, 0.030, 0.052))
cats <- length(unique(nazi_wgs84$members.32r2cum_cat))
cols <- brewer.pal(n = cats, name = "YlOrRd")

col_vec <- vector() # set colors
for (i in 1:nrow(nazi_wgs84)) {
  col_vec[i] <- cols[nazi_wgs84$members.32r2cum_cat[i]]
}
plot(nazi_wgs84, col = col_vec, border = "darkgrey")
# add legend
colorbar.plot(x = 20, y = 47.5, strip.width = .1, seq_along(cols), col = cols,  adj.y = 0.8)
text(x = 19.7, y = 47.8, "NSDAP membership", cex = 1.3)
mtext("0%", side = 1, outer = FALSE, at = 17.7, cex = 1.3)
mtext("1.2%", side = 1, outer = FALSE, at = 20, cex = 1.3)
mtext(">3%", side = 1, outer = FALSE, at = 22.2, cex = 1.3)
dev.off()


pdf(file="../figures/map_nsdap_members_33.pdf", height=7.5, width=10, family="URWTimes")
par(oma=c(0,0,0,0))
par(mar=c(1.5,2,1.5,.5))
par(xaxs = "i", yaxs = "i")
# plot boundaries
# plot values of continuous variable
nazi_wgs84$members.33cum_cat <- findInterval(nazi_wgs84$n_wgt.33, c(0, 0.003, 0.006, 0.009, 0.012, 0.015, 0.018, 0.021, 0.030, 0.052))
cats <- length(unique(nazi_wgs84$members.33cum_cat))
cols <- brewer.pal(n = cats, name = "YlOrRd")

col_vec <- vector() # set colors
for (i in 1:nrow(nazi_wgs84)) {
  col_vec[i] <- cols[nazi_wgs84$members.33cum_cat[i]]
}
plot(nazi_wgs84, col = col_vec, border = "darkgrey")
# add legend
colorbar.plot(x = 20, y = 47.5, strip.width = .1, seq_along(cols), col = cols,  adj.y = 0.8)
text(x = 19.7, y = 47.8, "NSDAP membership", cex = 1.3)
mtext("0%", side = 1, outer = FALSE, at = 17.7, cex = 1.3)
mtext("1.2%", side = 1, outer = FALSE, at = 20, cex = 1.3)
mtext(">3%", side = 1, outer = FALSE, at = 22.2, cex = 1.3)
dev.off()




