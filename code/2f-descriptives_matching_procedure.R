#----------------------------------------------------------------
# Examining a Most Likely Case for Strong Campaign Effects: 
# Hitler's Speeches and the Rise of the Nazi Party, 1927--1933
# Peter Selb (University of Konstanz)
# Simon Munzert (MZES, University of Mannheim)
# 2016
#----------------------------------------------------------------

## imports
c("nazi_wgs84.RData",
  "elections_geopoints_spdf.RData",
  "speeches_df.RData")

## exports
c("../figures/map_matching_procedure.pdf")


## load packages and functions -------------------
source("packages.r")
source("functions.r")


## plot map of locations -------------------------

# load map data
load("nazi_wgs84.RData")

# load geo-referenced community point data
load("elections_geopoints_spdf.RData")

# load speeches data
load("speeches_df.RData")
speeches_sp <- SpatialPoints(cbind(speeches_df$lon, speeches_df$lat))
projection <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4string(speeches_sp) <- CRS(projection)
speeches_df_sep30 <- filter(speeches_df, date > "1928-05-20", date < "1930-09-14")

# county-level vs. community-level indicator
elections_geo_spdf$countylvl <- elections_geo_spdf$agglvl == "stadtkreise" | elections_geo_spdf$agglvl == "KREISE M.GEMEINDEN >" | elections_geo_spdf$agglvl == "KREISE O.GEMEINDEN >"
elections_geo_spdf$communitylvl <- elections_geo_spdf$agglvl == "stadtkreise" | elections_geo_spdf$agglvl == "GEMEINDEN AB 2000 E." | elections_geo_spdf$agglvl == "KREISE O.GEMEINDEN >" | elections_geo_spdf$agglvl == "RESTKREISE (GEM.< 20"


### plot locations of geo-coded communities -----------
pdf(file="../figures/map_matching_procedure.pdf", height=7.5, width=10, family="URWTimes")
par(oma=c(0,0,0,0))
par(mar=c(3,3,1.2,.5))
par(xaxs = "i", yaxs = "i")

# prepare coloers
# red_solid <- rgb(240, 60, 30, 255, max = 255)
# red_weak <- rgb(240, 60, 30, 50, max = 255)
# blue_solid <- rgb(30, 60, 220, 255, max = 255)
# blue_weak <- rgb(30, 60, 220, 50, max = 255)
# green_solid <- rgb(50, 190, 50, 255, max = 255)
# green_weak <- rgb(50, 190, 50, 50, max = 255)
# grey_solid <- rgb(90, 90, 90, 255, max = 255)
# grey_weak <- rgb(120, 120, 120, 80, max = 255)
# grey_weaker <- rgb(170, 170, 170, 50, max = 255)
blue_solid <- rgb(30, 30, 30, 255, max = 255)
blue_weak <- rgb(30, 30, 30, 90, max = 255)
green_solid <- rgb(140, 140, 140, 255, max = 255)
green_weak <- rgb(140, 140, 140, 50, max = 255)


# plot boundaries
plot(nazi_wgs84, col = "white", xlim = c(11.9, 13.5), ylim = c(50.35,51.1), border = "darkgrey", axes = TRUE)

# plot points of all geo-coded communities
points(elections_geo_spdf[elections_geo_spdf$communitylvl==TRUE,], pch = 20, cex = .8, col = "black")
points(elections_geo_spdf[elections_geo_spdf$countylvl==TRUE,], pch = 17, cex = 1.2, col = "black")

# plot radii around treatment
for (i in 1:nrow(speeches_df_sep30)) {
plotCircle(speeches_df_sep30$lon[i], speeches_df_sep30$lat[i], 10, col = blue_weak, border = blue_solid, lty = 1)
plotCircle(speeches_df_sep30$lon[i], speeches_df_sep30$lat[i], 20, col = green_weak, border = green_solid, lty = 1)
}

# plot points of treated communities
points(elections_geo_spdf[elections_geo_spdf$speech_sep30_10km==T & elections_geo_spdf$communitylvl==TRUE,], pch = 20, col = "white", cex = .8)
points(elections_geo_spdf[elections_geo_spdf$speech_sep30_10km==T & elections_geo_spdf$countylvl==TRUE,], pch = 17, col = "white", cex = 1.2)


# plot points of communities in no-matching zone
points(elections_geo_spdf[elections_geo_spdf$speech_sep30_10km_nomatch==T & elections_geo_spdf$communitylvl==TRUE,], pch = 20, cex = .8, col = green_solid)
points(elections_geo_spdf[elections_geo_spdf$speech_sep30_10km_nomatch==T & elections_geo_spdf$countylvl==TRUE,], pch = 17, cex = 1.2, col = green_solid)


# plot points of speeches
points(speeches_df_sep30$lon, speeches_df_sep30$lat, pch = 1, col = "white", cex = 2, lwd = 2)
text(speeches_df_sep30$lon, speeches_df_sep30$lat + .007, speeches_df_sep30$location, pos = 3, col = "white")

box()
dev.off()

