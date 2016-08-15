#----------------------------------------------------------------
# Examining a Most Likely Case for Strong Campaign Effects: 
# Hitler's Speeches and the Rise of the Nazi Party, 1927--1933
# Peter Selb (University of Konstanz)
# Simon Munzert (MZES, University of Mannheim)
# 2016
#----------------------------------------------------------------


## imports
c("speeches_df.RData",
  "nazi_wgs84.RData",
  "goebbels_df.RData")

## exports
c("../figures/tab-audience.tex",
  "../figures/audience_estimates_scatterplot_matrix.pdf",
  "../figures/audience_estimates_density.pdf",
  "../figures/tab-speeches.tex", 
  "../figures/speeches_timeline.pdf",
  "../figures/speeches_locations_map.pdf",
  "../figures/speeches_locations_map_hitler_goebbels.pdf")



## load packages and functions -------------------------------
source("packages.r")
source("functions.r")

## load speeches data -------------------------
load("speeches_df.RData")


# table: summary statistics, audience of speeches ----------
sum_police <- summary(speeches_df$audience_police_num)
sum_press <- summary(speeches_df$audience_press_num)
sum_nspress <- summary(speeches_df$audience_nspress_num)
sum_imputed <- summary(speeches_df$audience_police_num_imputed)

totals <- c(sum(speeches_df$audience_police_num, na.rm = TRUE),
            sum(speeches_df$audience_press_num, na.rm = TRUE),
            sum(speeches_df$audience_nspress_num, na.rm = TRUE),
            sum(speeches_df$audience_police_num_imputed, na.rm = TRUE))
audience_tab <- rbind(sum_police, sum_press, sum_nspress, sum_imputed)
audience_tab <- cbind(audience_tab, totals)
rownames(audience_tab) <- c("Police reports", "Press", "Nazi press", "Police reports, imputed")
colnames(audience_tab) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.", "NA's", "Total")
print(xtable(t(apply(audience_tab, 1, prettyNum, big.mark = ",", scientific = F)), align = c("l","r","r", "r", "r", "r", "r", "r", "r"), digits=0, caption = "Summary statistics of reported audience size, by source.\\label{tab:audiencesumstats}"), booktabs = TRUE, size = "small", caption.placement = "top", table.placement = "t!", include.rownames=TRUE, hline.after = c(-1,0, 3, nrow(audience_tab)), file = "../figures/tab-audience.tex")

# sum of audience until Jul 1932 election (reported in main text)
sum(speeches_df$audience_police_num_imputed[speeches_df$date < "1932-07-31"], na.rm = TRUE)
table(is.na(speeches_df$audience_police_num_imputed[speeches_df$date < "1932-07-31"]))
# 3596505



# plot: compare audience size reports, scatterplot matrix ------------
speeches_log_df <- data.frame(police = log(speeches_df$audience_police_num), press = log(speeches_df$audience_press_num), nspress = log(speeches_df$audience_nspress_num))

names(speeches_log_df) <- c("Log(police reports)", "Log(press reports)", "Log(NS press reports)")

# plot
pdf(file="../figures/audience_estimates_scatterplot_matrix.pdf", height=6, width=6, family="URWTimes")
par(oma=c(0,0,0,0) + .7)
par(mar=c(4, 4,.5,.5))
par(xlim = c(6, 15), ylim = c(6, 15))
par(xaxs = "i", yaxs = "i")
my.xlim <- array(7, dim=c(3, 3, 2))
my.xlim[,,2] <- 12
my.ylim <- my.xlim
my.xlim[,,1] <- 7
my.pairs(speeches_log_df, lower.panel = my_line, upper.panel = panel.cor, xlim = my.xlim)
dev.off()      



# plot: compare audience size reports, density curves -------
pdf(file="../figures/audience_estimates_density.pdf", height=4, width=8, family="URWTimes")
par(oma=c(0,0,0,0) + .7)
par(mar=c(4, 4,.5,.5))
par(xaxs = "i", yaxs = "i")
plot(density(speeches_df$audience_police_num, na.rm = TRUE, bw = 5000), main = "", xlab = "Reported audience size", lty = 2, lwd = 2, col = "black", xlim = c(0, max(c(sum_police, sum_press, sum_nspress, sum_imputed))))
lines(density(speeches_df$audience_press_num, na.rm = TRUE, bw = 5000), lty = 3, lwd = 2, col = "black")
lines(density(speeches_df$audience_nspress_num, na.rm = TRUE, bw = 5000), lty = 4, lwd = 2, col = "black")
lines(density(speeches_df$audience_police_num_imputed, na.rm = TRUE, bw = 5000), lty = 1, lwd = 2, col = "black")
grid(col = "black")
legend("topright",
       c("Police reports", "Press", "Nazi press" ,"Police reports, imputed"),
       lty = c(2, 3, 4, 1),
       lwd = c(2.5, 2.5, 2.5, 2.5))
dev.off()



## table: speeches by election period ---------
speeches_df$election <- cut(as.Date(speeches_df$date),
                            breaks=ymd(c("1924-01-01", "1928-05-20", "1930-09-14", "1932-07-31", "1932-11-06", "1933-03-05")),
                            labels=c("May 1928", "Sep 1930", "Jul 1932", "Nov 1932", "May 1933"), right = FALSE)

speeches_num <- table(speeches_df$election)
speeches_with_count_num <- table(speeches_df$election[!is.na(speeches_df$audience_police_num_imputed)])

speeches_audience_sum <- tapply(speeches_df$audience_police_num_imputed, speeches_df$election, FUN = sum, na.rm= TRUE)
speeches_audience_mean <- tapply(speeches_df$audience_police_num_imputed, speeches_df$election, FUN = mean, na.rm= TRUE)

speeches_tab <- data.frame("N_speeches" = num(speeches_num),
                           "N_speeches, with count" = num(speeches_with_count_num),
                           "Audience, sum" = num(speeches_audience_sum),
                           "Audience, mean" = round(num(speeches_audience_mean)))
speeches_total <- c(nrow(speeches_df), length(speeches_df$election[!is.na(speeches_df$audience_police_num_imputed)]), sum(speeches_df$audience_police_num_imputed, na.rm = TRUE), round(mean(speeches_df$audience_police_num_imputed, na.rm = TRUE)))
speeches_tab = rbind(speeches_tab, speeches_total)

colnames(speeches_tab) <- c("$N_{\\text{speeches}}$", "$N^*_{\\text{speeches}}$", "$\\text{Sum}_{\\text{audience}}$","$\\text{Mean}_{\\text{audience}}$")
rownames(speeches_tab) <- c("May 1928", "Sep 1930", "Jul 1932", "Nov 1932", "May 1933", "Total")

print(xtable(t(apply(speeches_tab, 1, prettyNum, big.mark = ",", scientific = F)), align = c("r","r","r", "r", "r"), digits=0, caption = "Summary statistics for public speeches and audience counts, by election period. The column $N^*_{\\text{speeches}}$ summarizes speeches for which an estimate of the audience count is available.\\label{tab:speechessumstats}"), booktabs = TRUE, size = "small", caption.placement = "top", table.placement = "t!", hline.after = c(-1,0, 5, nrow(speeches_tab)), include.rownames=TRUE, sanitize.text.function = function(x){x}, file = "../figures/tab-speeches.tex")



### plot: timeline of speeches --------------

dat <- table(speeches_df$date)
speeches_count <- data.frame(date = ymd(char(rownames(dat))), count = num(dat))
min_date <- ymd("1927-03-09")
max_date <- ymd("1933-04-01")

# election dates
election_dates <- ymd(c("1919-01-19", "1920-06-06", "1924-05-04", "1924-12-07", "1928-05-20", "1930-09-14", "1932-07-31", "1932-11-06", "1933-03-05"))
election_dates <- ymd(c("1928-05-20", "1930-09-14", "1932-07-31", "1932-11-06", "1933-03-05"))

election_months <- months(election_dates, abbreviate = TRUE)
election_years <- year(election_dates)

pres_election_dates <- ymd(c("1932-03-13", "1932-04-10"))
pres_election_months <- months(pres_election_dates, abbreviate = TRUE)
pres_election_years <- year(pres_election_dates)


## plot
pdf(file="../figures/speeches_timeline.pdf", height=3, width=7.5, family="URWTimes")
par(oma=c(0,0,0,0))
par(mar=c(1.5,2.5,2.5,.5))
par(yaxs = "i", xaxs = "i")
# frame
plot(speeches_count$date, speeches_count$count, type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", ylim = c(0, 5.5), xlim = c(as.Date("1926-11-15"), max_date), cex.axis=.6, col = "white")
# grid
abline(v=seq(ymd("1927-01-01"), ymd("1933-01-01"), by = '1 year'), col="lightgray", lty="dashed")
# lines
lines(speeches_count$date, speeches_count$count, type = "h", col = rgb(0,0,0, alpha = .6), lwd = .5)
# election dates
#abline(v=election_dates, col="darkgrey", lwd = 2, lty="dashed")
#abline(v=pres_election_dates, col="darkgrey", lwd = 3, lty="dashed")
# text(election_dates - days(25), 4.9, label = paste(c("May", "Sep", "Jul", "Nov", "Mar"), election_years), cex = .8, adj = 1, srt = 90, col = "black")
# text(ymd("1932-03-13") - days(25), 4.9, label = paste("Mar/Apr 1932"), cex = .8, adj = 1, srt = 90, col = "black")
# axes
axis(1, at = seq(ymd("1927-01-01"), ymd("1933-01-01"), by = '1 year'), labels = year(seq(ymd("1927-01-01"), ymd("1933-01-01"), by = '1 year')), cex.axis =1, mgp=c(3, .3, 0), tck=-0.01)
axis(2, at = 2.5, labels = "Speeches per day", line = .5, tick = F, cex.axis =1)
lablist.y <- as.vector(c(0:5))
axis(2, at=seq(0, 5, by=1), labels = FALSE, tick = F)
text(y = seq(0, 5, by=1), par("usr")[1], labels = lablist.y, srt = 0, pos = 2, xpd = TRUE)
par(tcl = .5)
axis(3, at=election_dates, labels = FALSE, tick = T)
text(x = election_dates, par("usr")[4]+.55, labels = rep("R", 5),  pos = 1, xpd = TRUE)
axis(3, at=pres_election_dates, labels = FALSE, tick = T)
text(x = mean(pres_election_dates), par("usr")[4]+.55, labels = "P", pos = 1, xpd = TRUE)
axis(3, at = mean(c(ymd("1927-01-01"), ymd("1933-01-01"))), labels = "Elections (R = Reichstag, P = Presidential)", line = .25, tick = F, cex.axis =1)
box()
dev.off()


### plot: locations of speeches -----------

# load map data
load("nazi_wgs84.RData")
set.seed(1)
speeches_df$date <- as.Date(speeches_df$date)

pdf(file="../figures/map_speeches_locations.pdf", height=6.5, width=9, family="URWTimes")
par(oma=c(0,0,0,0))
par(mar=c(1.5,2,1.5,.5))
par(xaxs = "i", yaxs = "i")
# plot boundaries
plot(nazi_wgs84, col = "white", border = "darkgrey")
# plot speeches, 1928
speeches_df_sub <- filter(speeches_df, date < ymd("1928-05-20"))
speeches_df_sub_sp  <- SpatialPoints(cbind(speeches_df_sub$lon, speeches_df_sub$lat))
offset <- .15
dat <- speeches_df_sub_sp
points(jitter(coordinates(dat)[,1], 20), jitter(coordinates(dat)[,2], 20), pch = 20, col = rgb(215, 25, 28, max = 255))
# plot speeches, 1930
speeches_df_sub <- filter(speeches_df, date > ymd("1928-05-20"), date < ymd("1930-09-14"))
speeches_df_sub_sp  <- SpatialPoints(cbind(speeches_df_sub$lon, speeches_df_sub$lat))
offset <- .15
dat <- speeches_df_sub_sp
points(jitter(coordinates(dat)[,1], 20), jitter(coordinates(dat)[,2], 20), pch = 20, col = rgb(30, 215, 20, max = 255))
# plot speeches, 1932, 1
speeches_df_sub <- filter(speeches_df, date > ymd("1930-09-14"), date < ymd("1932-07-31"))
speeches_df_sub_sp  <- SpatialPoints(cbind(speeches_df_sub$lon, speeches_df_sub$lat))
offset <- .15
dat <- speeches_df_sub_sp
points(jitter(coordinates(dat)[,1], 20), jitter(coordinates(dat)[,2], 20), pch = 20, col = rgb(0, 0, 0, max = 255))
# plot speeches, 1932, 2
speeches_df_sub <- filter(speeches_df, date > ymd("1932-07-31"), date < ymd("1932-11-06"))
speeches_df_sub_sp  <- SpatialPoints(cbind(speeches_df_sub$lon, speeches_df_sub$lat))
offset <- .15
dat <- speeches_df_sub_sp
points(jitter(coordinates(dat)[,1], 20), jitter(coordinates(dat)[,2], 20), pch = 20, col = rgb(250, 140, 60, max = 255))
# plot speeches, 1933
speeches_df_sub <- filter(speeches_df, date > ymd("1932-11-06"), date < ymd("1933-03-05"))
speeches_df_sub_sp  <- SpatialPoints(cbind(speeches_df_sub$lon, speeches_df_sub$lat))
offset <- .15
dat <- speeches_df_sub_sp
points(jitter(coordinates(dat)[,1], 20), jitter(coordinates(dat)[,2], 20), pch = 20, col = rgb(44, 123, 182, max = 255))

# add legend
text(x = 16.9, y = 49, "Legend", cex=.8, adj=0, font = 3)
text(x = 17.2, y = 48.7, "Speeches between Mar 07, 1927 and May 19, 1928", cex=.8, adj=0)
points(x = 17, y = 48.7, pch = 20, col = rgb(215, 25, 28, max = 255))
text(x = 17.2, y = 48.5, "Speeches between May 21, 1928 and Sep 13, 1930", cex=.8, adj=0)
points(x = 17, y = 48.5, pch = 20, col = rgb(30, 215, 20, max = 255))
text(x = 17.2, y = 48.3, "Speeches between Sep 15, 1930 and Jul 30, 1932", cex=.8, adj=0)
points(x = 17, y = 48.3, pch = 20, col = rgb(0, 0, 0, max = 255))
text(x = 17.2, y = 48.1, "Speeches between Aug 01, 1932 and Nov 05, 1932", cex=.8, adj=0)
points(x = 17, y = 48.1, pch = 20, col = rgb(250, 140, 60, max = 255))
text(x = 17.2, y = 47.9, "Speeches between Nov 07, 1932 and Mar 04, 1933", cex=.8, adj=0)
points(x = 17, y = 47.9, pch = 20, col = rgb(44, 123, 182, max = 255))
dev.off()




### plot: locations of speeches, Hitler vs. Goebbels -----------

# load map data
load("goebbels_df.RData")
goebbels_df <- filter(goebbels_df, location != "Prag" & location != "Danzig")

pdf(file="../figures/map_speeches_locations_hitler_goebbels.pdf", height=6.5, width=9, family="URWTimes")
par(oma=c(0,0,0,0))
par(mar=c(1.5,2,1.5,.5))
par(xaxs = "i", yaxs = "i")
# plot boundaries
plot(nazi_wgs84, col = "white", border = "darkgrey")
# plot Hitler speeches
speeches_df_sp  <- SpatialPoints(cbind(speeches_df$lon, speeches_df$lat))
offset <- .15
set.seed(1)
points(jitter(coordinates(speeches_df_sp)[,1], 0), jitter(coordinates(speeches_df_sp)[,2], 0), pch = 20, col = rgb(0, 0, 0, max = 255))
# plot Goebbels speeches, 1930
goebbels_df_sp  <- SpatialPoints(cbind(goebbels_df$lon, goebbels_df$lat))
offset <- .15
set.seed(2)
points(jitter(coordinates(goebbels_df_sp)[,1], 0), jitter(coordinates(goebbels_df_sp)[,2], 0), pch = 1, col = rgb(215, 25, 28, max = 255))
# plot Hitler speeches, again (for overlay)
points(jitter(coordinates(speeches_df_sp)[,1], 0), jitter(coordinates(speeches_df_sp)[,2], 0), pch = 20, col = rgb(0, 0, 0, max = 255))
# add legend
text(x = 16.9, y = 48, "Legend", cex=.8, adj=0, font = 3)
text(x = 17.2, y = 47.7, "Hitler speeches", cex=.8, adj=0)
points(x = 17, y = 47.7, pch = 20, col = rgb(0, 0, 0, max = 255))
text(x = 17.2, y = 47.5, "Goebbels speeches", cex=.8, adj=0)
points(x = 17, y = 47.5, pch = 1, col = rgb(215, 25, 28, max = 255))
dev.off()



