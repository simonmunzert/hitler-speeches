#----------------------------------------------------------------
# Examining a Most Likely Case for Strong Campaign Effects: 
# Hitler's Speeches and the Rise of the Nazi Party, 1927--1933
# Peter Selb (University of Konstanz)
# Simon Munzert (MZES, University of Mannheim)
# 2016
#----------------------------------------------------------------


## imports
c("data_geopoints/geopoints_df_cleaned.csv",
  "data_elections/ZA8013_Wahldaten.dta",
  "data_elections/RPWABS_new.dta",
  "airfields_certainly_before_32_df.RData",
  "speeches_df.RData",
  "goebbels_df.RData",
  "members_df_wkr_wide.RData")

## exports
c("elections_geopoints_df.RData",
  "elections_geopoints_spdf.RData",
  "elections_geopoints_df.dta")



## load packages and functions -------------------------------
source("packages.r")
source("functions.r")


## import Falter election data -----------

# import Reichstag election data
elections_df <- read.dta("data_elections/ZA8013_Wahldaten.dta", convert.factors = TRUE)

# import Presidential election data
elections_pres_df <- read.dta("data_elections/RPWABS_new.dta")
vars <- names(elections_pres_df)[str_detect(names(elections_pres_df), "323|324|c25pop5")]
elections_pres_df <- elections_pres_df[,vars]

# remove two duplicates on population size (=matching) variable
elections_pres_df <- elections_pres_df[!duplicated(elections_pres_df$c25pop5),]

# merge by 1932 county population variables (county ID variables are messed up)
elections_df <- merge(elections_df, elections_pres_df, all.x = TRUE, by.x = "n327pop", by.y = "c25pop5")


## merge election data with geographic data --------------------------

# import county/municipality geo-points data
geopoints_df <- read.csv2("data_geopoints/geopoints_df_cleaned.csv", header = TRUE,  quote = "", encoding = "UTF-8", stringsAsFactors = FALSE)
geopoints_df$long <- geopoints_df$long %>% char() %>% num()
geopoints_df$lat <- geopoints_df$lat %>% char() %>% num()
geopoints_df$lat_median <- geopoints_df$lat_median %>% char() %>% num()
geopoints_df$long_median <- geopoints_df$long_median %>% char() %>% num()

elections_geo_df <- merge(elections_df, geopoints_df, all = TRUE, by = c("krnr", "lfnr", "agglvl", "name"))

# set "-9" values to missing
elections_geo_df[elections_geo_df==-9] <- NA

# inspect geo-data
table(is.na(elections_geo_df$lat)) # check: how many observations are not geocoded?
elections_geo_df <- filter(elections_geo_df, !is.na(lat)) # delete observations with no geocode
elections_geo_df <- filter(elections_geo_df, lat > 40, long > 0) # delete observations with invalid geocode

# turn data into SpatialPointsDataFrame
elections_sp <- SpatialPoints(cbind(elections_geo_df$long, elections_geo_df$lat))
projection <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4string(elections_sp) <- CRS(projection)
elections_geo_spdf <- SpatialPointsDataFrame(elections_sp, elections_geo_df, proj4string = projection)
plot(elections_geo_spdf, pch = 20, cex = .5)




## import and process airfields data --------------
load("airfields_certainly_before_32_df.RData")

# retrieve SpatialPoints
airfields_sp <- SpatialPoints(cbind(airfields_df$longitude, airfields_df$latitude))
proj4string(airfields_sp) <- CRS(projection)

# compute distance between election unit geo-point and nearest airfield
airfields_dist <- gDistance(airfields_sp, elections_sp, byid = TRUE)

# compute distances in Kilometers to district centoids
dists <- rdist.earth(coordinates(airfields_sp), coordinates(elections_sp), miles = FALSE, R = 6378)

# identify minimum distance + nearest airfield
elections_geo_spdf$airfields_dist_min <- apply(dists, 2, min)
elections_geo_spdf$airfields_dist_which_min <- char(airfields_df$names[apply(airfields_dist, 1, which.min)])




## import and process speeches data --------------
load("speeches_df.RData")
speeches_df$date <- ymd(speeches_df$date)

# retrieve SpatialPoints
speeches_sp <- SpatialPoints(cbind(speeches_df$lon, speeches_df$lat))
proj4string(speeches_sp) <- CRS(projection)

# generate speech variable dummies - full range between elections, 12, 8, 4, 2 weeks before election
speeches_df_may28 <- filter(speeches_df, date > "1924-12-07", date < "1928-05-20")
speeches_df_sep30 <- filter(speeches_df, date > "1928-05-20", date < "1930-09-14")
speeches_df_jul32 <- filter(speeches_df, date > "1930-09-14", date < "1932-07-31")
speeches_df_nov32 <- filter(speeches_df, date > "1932-07-31", date < "1932-11-06")
speeches_df_mar33 <- filter(speeches_df, date > "1932-11-06", date < "1933-03-05")
speeches_df_mar32 <- filter(speeches_df, date > "1930-09-14", date < "1932-03-13")
speeches_df_apr32 <- filter(speeches_df, date > "1932-03-13", date < "1932-04-10")

speeches_df_may28_2w <- filter(speeches_df, date > ymd("1928-05-20") - weeks(2), date < "1928-05-20")
speeches_df_may28_4w <- filter(speeches_df, date > ymd("1928-05-20") - weeks(4), date < "1928-05-20")
speeches_df_may28_8w <- filter(speeches_df, date > ymd("1928-05-20") - weeks(8), date < "1928-05-20")
speeches_df_may28_12w <- filter(speeches_df, date > ymd("1928-05-20") - weeks(12), date < "1928-05-20")

speeches_df_sep30_2w <- filter(speeches_df, date > ymd("1930-09-14") - weeks(2), date < "1930-09-14")
speeches_df_sep30_4w <- filter(speeches_df, date > ymd("1930-09-14") - weeks(4), date < "1930-09-14")
speeches_df_sep30_8w <- filter(speeches_df, date > ymd("1930-09-14") - weeks(8), date < "1930-09-14")
speeches_df_sep30_12w <- filter(speeches_df, date > ymd("1930-09-14") - weeks(12), date < "1930-09-14")

speeches_df_jul32_2w <- filter(speeches_df, date > ymd("1932-07-31") - weeks(2), date < "1932-07-31")
speeches_df_jul32_4w <- filter(speeches_df, date > ymd("1932-07-31") - weeks(4), date < "1932-07-31")
speeches_df_jul32_8w <- filter(speeches_df, date > ymd("1932-07-31") - weeks(8), date < "1932-07-31")
speeches_df_jul32_12w <- filter(speeches_df, date > ymd("1932-07-31") - weeks(12), date < "1932-07-31")

speeches_df_nov32_2w <- filter(speeches_df, date > ymd("1932-11-06") - weeks(2), date < "1932-11-06")
speeches_df_nov32_4w <- filter(speeches_df, date > ymd("1932-11-06") - weeks(4), date < "1932-11-06")
speeches_df_nov32_8w <- filter(speeches_df, date > ymd("1932-11-06") - weeks(8), date < "1932-11-06")
speeches_df_nov32_12w <- filter(speeches_df, date > ymd("1932-11-06") - weeks(12), date < "1932-11-06")

speeches_df_mar33_2w <- filter(speeches_df, date > ymd("1933-03-05") - weeks(2), date < "1933-03-05")
speeches_df_mar33_4w <- filter(speeches_df, date > ymd("1933-03-05") - weeks(4), date < "1933-03-05")
speeches_df_mar33_8w <- filter(speeches_df, date > ymd("1933-03-05") - weeks(8), date < "1933-03-05")
speeches_df_mar33_12w <- filter(speeches_df, date > ymd("1933-03-05") - weeks(12), date < "1933-03-05")

speeches_df_mar32_2w <- filter(speeches_df, date > ymd("1932-03-13") - weeks(2), date < "1932-03-13")
speeches_df_mar32_4w <- filter(speeches_df, date > ymd("1932-03-13") - weeks(4), date < "1932-03-13")
speeches_df_mar32_8w <- filter(speeches_df, date > ymd("1932-03-13") - weeks(8), date < "1932-03-13")
speeches_df_mar32_12w <- filter(speeches_df, date > ymd("1932-03-13") - weeks(12), date < "1932-03-13")

speeches_df_apr32_2w <- filter(speeches_df, date > ymd("1932-04-10") - weeks(2), date < "1932-04-10")
speeches_df_apr32_4w <- filter(speeches_df, date > ymd("1932-04-10") - weeks(4), date < "1932-04-10")
speeches_df_apr32_8w <- filter(speeches_df, date > ymd("1932-04-10") - weeks(8), date < "1932-04-10")
speeches_df_apr32_12w <- filter(speeches_df, date > ymd("1932-04-10") - weeks(12), date < "1932-04-10")


# generate speech indicator in county-level data ("did hitler give a speech near the county (within 5km, 10km, 25km, 50km) in a given time period (full period, 2, 4, 8, 12 weeks before the election?")

# 1928, full period
mindist <- rdist.earth(coordinates(cbind(speeches_df_may28$lon, speeches_df_may28$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$speech_may28_5km <- mindist < 5
elections_geo_spdf$speech_may28_10km <- mindist < 10
elections_geo_spdf$speech_may28_25km <- mindist < 25
elections_geo_spdf$speech_may28_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$speech_may28_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$speech_may28_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$speech_may28_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$speech_may28_50km_nomatch <- mindist > 50 & mindist < 60

# 1930, full period
mindist <- rdist.earth(coordinates(cbind(speeches_df_sep30$lon, speeches_df_sep30$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$speech_sep30_5km <- mindist < 5
elections_geo_spdf$speech_sep30_10km <- mindist < 10
elections_geo_spdf$speech_sep30_25km <- mindist < 25
elections_geo_spdf$speech_sep30_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$speech_sep30_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$speech_sep30_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$speech_sep30_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$speech_sep30_50km_nomatch <- mindist > 50 & mindist < 60

# 1932/1, full period
mindist <- rdist.earth(coordinates(cbind(speeches_df_jul32$lon, speeches_df_jul32$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$speech_jul32_5km <- mindist < 5
elections_geo_spdf$speech_jul32_10km <- mindist < 10
elections_geo_spdf$speech_jul32_25km <- mindist < 25
elections_geo_spdf$speech_jul32_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$speech_jul32_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$speech_jul32_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$speech_jul32_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$speech_jul32_50km_nomatch <- mindist > 50 & mindist < 60

# 1932/2, full period
mindist <- rdist.earth(coordinates(cbind(speeches_df_nov32$lon, speeches_df_nov32$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$speech_nov32_5km <- mindist < 5
elections_geo_spdf$speech_nov32_10km <- mindist < 10
elections_geo_spdf$speech_nov32_25km <- mindist < 25
elections_geo_spdf$speech_nov32_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$speech_nov32_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$speech_nov32_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$speech_nov32_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$speech_nov32_50km_nomatch <- mindist > 50 & mindist < 60

# 1933, full period
mindist <- rdist.earth(coordinates(cbind(speeches_df_mar33$lon, speeches_df_mar33$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$speech_mar33_5km <- mindist < 5
elections_geo_spdf$speech_mar33_10km <- mindist < 10
elections_geo_spdf$speech_mar33_25km <- mindist < 25
elections_geo_spdf$speech_mar33_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$speech_mar33_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$speech_mar33_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$speech_mar33_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$speech_mar33_50km_nomatch <- mindist > 50 & mindist < 60

# 1932/p1, full period
mindist <- rdist.earth(coordinates(cbind(speeches_df_mar32$lon, speeches_df_mar32$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$speech_mar32_5km <- mindist < 5
elections_geo_spdf$speech_mar32_10km <- mindist < 10
elections_geo_spdf$speech_mar32_25km <- mindist < 25
elections_geo_spdf$speech_mar32_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$speech_mar32_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$speech_mar32_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$speech_mar32_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$speech_mar32_50km_nomatch <- mindist > 50 & mindist < 60

# 1932/p2, full period
mindist <- rdist.earth(coordinates(cbind(speeches_df_apr32$lon, speeches_df_apr32$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$speech_apr32_5km <- mindist < 5
elections_geo_spdf$speech_apr32_10km <- mindist < 10
elections_geo_spdf$speech_apr32_25km <- mindist < 25
elections_geo_spdf$speech_apr32_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$speech_apr32_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$speech_apr32_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$speech_apr32_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$speech_apr32_50km_nomatch <- mindist > 50 & mindist < 60


# 1928, restricted periods
mindist <- rdist.earth(coordinates(cbind(speeches_df_may28_2w$lon, speeches_df_may28_2w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$speech_may28_2w_5km <- mindist < 5
elections_geo_spdf$speech_may28_2w_10km <- mindist < 10
elections_geo_spdf$speech_may28_2w_25km <- mindist < 25
elections_geo_spdf$speech_may28_2w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$speech_may28_2w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$speech_may28_2w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$speech_may28_2w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$speech_may28_2w_50km_nomatch <- mindist > 50 & mindist < 60
mindist <- rdist.earth(coordinates(cbind(speeches_df_may28_4w$lon, speeches_df_may28_4w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$speech_may28_4w_5km <- mindist < 5
elections_geo_spdf$speech_may28_4w_10km <- mindist < 10
elections_geo_spdf$speech_may28_4w_25km <- mindist < 25
elections_geo_spdf$speech_may28_4w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$speech_may28_4w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$speech_may28_4w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$speech_may28_4w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$speech_may28_4w_50km_nomatch <- mindist > 50 & mindist < 60
mindist <- rdist.earth(coordinates(cbind(speeches_df_may28_8w$lon, speeches_df_may28_8w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$speech_may28_8w_5km <- mindist < 5
elections_geo_spdf$speech_may28_8w_10km <- mindist < 10
elections_geo_spdf$speech_may28_8w_25km <- mindist < 25
elections_geo_spdf$speech_may28_8w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$speech_may28_8w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$speech_may28_8w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$speech_may28_8w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$speech_may28_8w_50km_nomatch <- mindist > 50 & mindist < 60
mindist <- rdist.earth(coordinates(cbind(speeches_df_may28_12w$lon, speeches_df_may28_12w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$speech_may28_12w_5km <- mindist < 5
elections_geo_spdf$speech_may28_12w_10km <- mindist < 10
elections_geo_spdf$speech_may28_12w_25km <- mindist < 25
elections_geo_spdf$speech_may28_12w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$speech_may28_12w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$speech_may28_12w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$speech_may28_12w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$speech_may28_12w_50km_nomatch <- mindist > 50 & mindist < 60

# 1930, restricted periods
mindist <- rdist.earth(coordinates(cbind(speeches_df_sep30_2w$lon, speeches_df_sep30_2w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$speech_sep30_2w_5km <- mindist < 5
elections_geo_spdf$speech_sep30_2w_10km <- mindist < 10
elections_geo_spdf$speech_sep30_2w_25km <- mindist < 25
elections_geo_spdf$speech_sep30_2w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$speech_sep30_2w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$speech_sep30_2w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$speech_sep30_2w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$speech_sep30_2w_50km_nomatch <- mindist > 50 & mindist < 60
mindist <- rdist.earth(coordinates(cbind(speeches_df_sep30_4w$lon, speeches_df_sep30_4w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$speech_sep30_4w_5km <- mindist < 5
elections_geo_spdf$speech_sep30_4w_10km <- mindist < 10
elections_geo_spdf$speech_sep30_4w_25km <- mindist < 25
elections_geo_spdf$speech_sep30_4w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$speech_sep30_4w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$speech_sep30_4w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$speech_sep30_4w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$speech_sep30_4w_50km_nomatch <- mindist > 50 & mindist < 60
mindist <- rdist.earth(coordinates(cbind(speeches_df_sep30_8w$lon, speeches_df_sep30_8w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$speech_sep30_8w_5km <- mindist < 5
elections_geo_spdf$speech_sep30_8w_10km <- mindist < 10
elections_geo_spdf$speech_sep30_8w_25km <- mindist < 25
elections_geo_spdf$speech_sep30_8w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$speech_sep30_8w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$speech_sep30_8w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$speech_sep30_8w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$speech_sep30_8w_50km_nomatch <- mindist > 50 & mindist < 60
mindist <- rdist.earth(coordinates(cbind(speeches_df_sep30_12w$lon, speeches_df_sep30_12w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$speech_sep30_12w_5km <- mindist < 5
elections_geo_spdf$speech_sep30_12w_10km <- mindist < 10
elections_geo_spdf$speech_sep30_12w_25km <- mindist < 25
elections_geo_spdf$speech_sep30_12w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$speech_sep30_12w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$speech_sep30_12w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$speech_sep30_12w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$speech_sep30_12w_50km_nomatch <- mindist > 50 & mindist < 60

# 1932/1, restricted periods
mindist <- rdist.earth(coordinates(cbind(speeches_df_jul32_2w$lon, speeches_df_jul32_2w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$speech_jul32_2w_5km <- mindist < 5
elections_geo_spdf$speech_jul32_2w_10km <- mindist < 10
elections_geo_spdf$speech_jul32_2w_25km <- mindist < 25
elections_geo_spdf$speech_jul32_2w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$speech_jul32_2w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$speech_jul32_2w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$speech_jul32_2w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$speech_jul32_2w_50km_nomatch <- mindist > 50 & mindist < 60
mindist <- rdist.earth(coordinates(cbind(speeches_df_jul32_4w$lon, speeches_df_jul32_4w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$speech_jul32_4w_5km <- mindist < 5
elections_geo_spdf$speech_jul32_4w_10km <- mindist < 10
elections_geo_spdf$speech_jul32_4w_25km <- mindist < 25
elections_geo_spdf$speech_jul32_4w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$speech_jul32_4w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$speech_jul32_4w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$speech_jul32_4w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$speech_jul32_4w_50km_nomatch <- mindist > 50 & mindist < 60
mindist <- rdist.earth(coordinates(cbind(speeches_df_jul32_8w$lon, speeches_df_jul32_8w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$speech_jul32_8w_5km <- mindist < 5
elections_geo_spdf$speech_jul32_8w_10km <- mindist < 10
elections_geo_spdf$speech_jul32_8w_25km <- mindist < 25
elections_geo_spdf$speech_jul32_8w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$speech_jul32_8w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$speech_jul32_8w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$speech_jul32_8w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$speech_jul32_8w_50km_nomatch <- mindist > 50 & mindist < 60
mindist <- rdist.earth(coordinates(cbind(speeches_df_jul32_12w$lon, speeches_df_jul32_12w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$speech_jul32_12w_5km <- mindist < 5
elections_geo_spdf$speech_jul32_12w_10km <- mindist < 10
elections_geo_spdf$speech_jul32_12w_25km <- mindist < 25
elections_geo_spdf$speech_jul32_12w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$speech_jul32_12w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$speech_jul32_12w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$speech_jul32_12w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$speech_jul32_12w_50km_nomatch <- mindist > 50 & mindist < 60

# 1932/2, restricted periods
mindist <- rdist.earth(coordinates(cbind(speeches_df_nov32_2w$lon, speeches_df_nov32_2w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$speech_nov32_2w_5km <- mindist < 5
elections_geo_spdf$speech_nov32_2w_10km <- mindist < 10
elections_geo_spdf$speech_nov32_2w_25km <- mindist < 25
elections_geo_spdf$speech_nov32_2w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$speech_nov32_2w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$speech_nov32_2w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$speech_nov32_2w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$speech_nov32_2w_50km_nomatch <- mindist > 50 & mindist < 60
mindist <- rdist.earth(coordinates(cbind(speeches_df_nov32_4w$lon, speeches_df_nov32_4w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$speech_nov32_4w_5km <- mindist < 5
elections_geo_spdf$speech_nov32_4w_10km <- mindist < 10
elections_geo_spdf$speech_nov32_4w_25km <- mindist < 25
elections_geo_spdf$speech_nov32_4w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$speech_nov32_4w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$speech_nov32_4w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$speech_nov32_4w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$speech_nov32_4w_50km_nomatch <- mindist > 50 & mindist < 60
mindist <- rdist.earth(coordinates(cbind(speeches_df_nov32_8w$lon, speeches_df_nov32_8w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$speech_nov32_8w_5km <- mindist < 5
elections_geo_spdf$speech_nov32_8w_10km <- mindist < 10
elections_geo_spdf$speech_nov32_8w_25km <- mindist < 25
elections_geo_spdf$speech_nov32_8w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$speech_nov32_8w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$speech_nov32_8w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$speech_nov32_8w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$speech_nov32_8w_50km_nomatch <- mindist > 50 & mindist < 60
mindist <- rdist.earth(coordinates(cbind(speeches_df_nov32_12w$lon, speeches_df_nov32_12w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$speech_nov32_12w_5km <- mindist < 5
elections_geo_spdf$speech_nov32_12w_10km <- mindist < 10
elections_geo_spdf$speech_nov32_12w_25km <- mindist < 25
elections_geo_spdf$speech_nov32_12w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$speech_nov32_12w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$speech_nov32_12w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$speech_nov32_12w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$speech_nov32_12w_50km_nomatch <- mindist > 50 & mindist < 60

# 1933, restricted periods
mindist <- rdist.earth(coordinates(cbind(speeches_df_mar33_2w$lon, speeches_df_mar33_2w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$speech_mar33_2w_5km <- mindist < 5
elections_geo_spdf$speech_mar33_2w_10km <- mindist < 10
elections_geo_spdf$speech_mar33_2w_25km <- mindist < 25
elections_geo_spdf$speech_mar33_2w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$speech_mar33_2w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$speech_mar33_2w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$speech_mar33_2w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$speech_mar33_2w_50km_nomatch <- mindist > 50 & mindist < 60
mindist <- rdist.earth(coordinates(cbind(speeches_df_mar33_4w$lon, speeches_df_mar33_4w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$speech_mar33_4w_5km <- mindist < 5
elections_geo_spdf$speech_mar33_4w_10km <- mindist < 10
elections_geo_spdf$speech_mar33_4w_25km <- mindist < 25
elections_geo_spdf$speech_mar33_4w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$speech_mar33_4w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$speech_mar33_4w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$speech_mar33_4w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$speech_mar33_4w_50km_nomatch <- mindist > 50 & mindist < 60
mindist <- rdist.earth(coordinates(cbind(speeches_df_mar33_8w$lon, speeches_df_mar33_8w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$speech_mar33_8w_5km <- mindist < 5
elections_geo_spdf$speech_mar33_8w_10km <- mindist < 10
elections_geo_spdf$speech_mar33_8w_25km <- mindist < 25
elections_geo_spdf$speech_mar33_8w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$speech_mar33_8w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$speech_mar33_8w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$speech_mar33_8w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$speech_mar33_8w_50km_nomatch <- mindist > 50 & mindist < 60
mindist <- rdist.earth(coordinates(cbind(speeches_df_mar33_12w$lon, speeches_df_mar33_12w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$speech_mar33_12w_5km <- mindist < 5
elections_geo_spdf$speech_mar33_12w_10km <- mindist < 10
elections_geo_spdf$speech_mar33_12w_25km <- mindist < 25
elections_geo_spdf$speech_mar33_12w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$speech_mar33_12w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$speech_mar33_12w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$speech_mar33_12w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$speech_mar33_12w_50km_nomatch <- mindist > 50 & mindist < 60

# 1932/p1, restricted periods
mindist <- rdist.earth(coordinates(cbind(speeches_df_mar32_2w$lon, speeches_df_mar32_2w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$speech_mar32_2w_5km <- mindist < 5
elections_geo_spdf$speech_mar32_2w_10km <- mindist < 10
elections_geo_spdf$speech_mar32_2w_25km <- mindist < 25
elections_geo_spdf$speech_mar32_2w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$speech_mar32_2w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$speech_mar32_2w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$speech_mar32_2w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$speech_mar32_2w_50km_nomatch <- mindist > 50 & mindist < 60
mindist <- rdist.earth(coordinates(cbind(speeches_df_mar32_4w$lon, speeches_df_mar32_4w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$speech_mar32_4w_5km <- mindist < 5
elections_geo_spdf$speech_mar32_4w_10km <- mindist < 10
elections_geo_spdf$speech_mar32_4w_25km <- mindist < 25
elections_geo_spdf$speech_mar32_4w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$speech_mar32_4w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$speech_mar32_4w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$speech_mar32_4w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$speech_mar32_4w_50km_nomatch <- mindist > 50 & mindist < 60
mindist <- rdist.earth(coordinates(cbind(speeches_df_mar32_8w$lon, speeches_df_mar32_8w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$speech_mar32_8w_5km <- mindist < 5
elections_geo_spdf$speech_mar32_8w_10km <- mindist < 10
elections_geo_spdf$speech_mar32_8w_25km <- mindist < 25
elections_geo_spdf$speech_mar32_8w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$speech_mar32_8w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$speech_mar32_8w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$speech_mar32_8w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$speech_mar32_8w_50km_nomatch <- mindist > 50 & mindist < 60
mindist <- rdist.earth(coordinates(cbind(speeches_df_mar32_12w$lon, speeches_df_mar32_12w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$speech_mar32_12w_5km <- mindist < 5
elections_geo_spdf$speech_mar32_12w_10km <- mindist < 10
elections_geo_spdf$speech_mar32_12w_25km <- mindist < 25
elections_geo_spdf$speech_mar32_12w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$speech_mar32_12w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$speech_mar32_12w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$speech_mar32_12w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$speech_mar32_12w_50km_nomatch <- mindist > 50 & mindist < 60

# 1932/p2, restricted periods
mindist <- rdist.earth(coordinates(cbind(speeches_df_apr32_2w$lon, speeches_df_apr32_2w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$speech_apr32_2w_5km <- mindist < 5
elections_geo_spdf$speech_apr32_2w_10km <- mindist < 10
elections_geo_spdf$speech_apr32_2w_25km <- mindist < 25
elections_geo_spdf$speech_apr32_2w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$speech_apr32_2w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$speech_apr32_2w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$speech_apr32_2w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$speech_apr32_2w_50km_nomatch <- mindist > 50 & mindist < 60
mindist <- rdist.earth(coordinates(cbind(speeches_df_apr32_4w$lon, speeches_df_apr32_4w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$speech_apr32_4w_5km <- mindist < 5
elections_geo_spdf$speech_apr32_4w_10km <- mindist < 10
elections_geo_spdf$speech_apr32_4w_25km <- mindist < 25
elections_geo_spdf$speech_apr32_4w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$speech_apr32_4w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$speech_apr32_4w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$speech_apr32_4w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$speech_apr32_4w_50km_nomatch <- mindist > 50 & mindist < 60
mindist <- rdist.earth(coordinates(cbind(speeches_df_apr32_8w$lon, speeches_df_apr32_8w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$speech_apr32_8w_5km <- mindist < 5
elections_geo_spdf$speech_apr32_8w_10km <- mindist < 10
elections_geo_spdf$speech_apr32_8w_25km <- mindist < 25
elections_geo_spdf$speech_apr32_8w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$speech_apr32_8w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$speech_apr32_8w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$speech_apr32_8w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$speech_apr32_8w_50km_nomatch <- mindist > 50 & mindist < 60
mindist <- rdist.earth(coordinates(cbind(speeches_df_apr32_12w$lon, speeches_df_apr32_12w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$speech_apr32_12w_5km <- mindist < 5
elections_geo_spdf$speech_apr32_12w_10km <- mindist < 10
elections_geo_spdf$speech_apr32_12w_25km <- mindist < 25
elections_geo_spdf$speech_apr32_12w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$speech_apr32_12w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$speech_apr32_12w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$speech_apr32_12w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$speech_apr32_12w_50km_nomatch <- mindist > 50 & mindist < 60

# check if generated variables are plausible
plot(elections_geo_spdf, cex = .3, col = "grey")
points(elections_geo_spdf[elections_geo_spdf$speech_may28_10km==T,], pch = 20, col = "red", cex = 1)
points(speeches_df_may28$lon, speeches_df_may28$lat, pch = 20, col = "green")
points(elections_geo_spdf[elections_geo_spdf$speech_may28_10km_nomatch==T,], pch = 20, col = "blue")


														

# create cumulative audience count and speeches count variables (note: speeches with unavailable count information are coded as missing. If one community is targeted by several speeches and at least one of the speeches has missing audience information, the variable is coded as missing)
for (i in 1:nrow(elections_geo_spdf)) {
  # 1928, full period
  mindist <- rdist.earth(coordinates(cbind(speeches_df_may28$lon, speeches_df_may28$lat)), coordinates(elections_sp[i]), miles = FALSE, R = 6378)
  elections_geo_spdf$audience_count_may28_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_may28[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_may28_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_may28[.,] %>% nrow()
  elections_geo_spdf$audience_count_may28_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_may28[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_may28_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_may28[.,] %>% nrow()
  elections_geo_spdf$audience_count_may28_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_may28[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_may28_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_may28[.,] %>% nrow()
  elections_geo_spdf$audience_count_may28_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_may28[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")	
  elections_geo_spdf$speeches_count_may28_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_may28[.,] %>% nrow()
  # 1930, full period
  mindist <- rdist.earth(coordinates(cbind(speeches_df_sep30$lon, speeches_df_sep30$lat)), coordinates(elections_sp[i]), miles = FALSE, R = 6378)
  elections_geo_spdf$audience_count_sep30_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_sep30[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_sep30_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_sep30[.,] %>% nrow()
  elections_geo_spdf$audience_count_sep30_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_sep30[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_sep30_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_sep30[.,] %>% nrow()
  elections_geo_spdf$audience_count_sep30_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_sep30[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_sep30_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_sep30[.,] %>% nrow()
  elections_geo_spdf$audience_count_sep30_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_sep30[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")	
  elections_geo_spdf$speeches_count_sep30_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_sep30[.,] %>% nrow()
  # 1932/1, full period
  mindist <- rdist.earth(coordinates(cbind(speeches_df_jul32$lon, speeches_df_jul32$lat)), coordinates(elections_sp[i]), miles = FALSE, R = 6378)
  elections_geo_spdf$audience_count_jul32_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_jul32[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_jul32_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_jul32[.,] %>% nrow()
  elections_geo_spdf$audience_count_jul32_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_jul32[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_jul32_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_jul32[.,] %>% nrow()
  elections_geo_spdf$audience_count_jul32_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_jul32[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_jul32_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_jul32[.,] %>% nrow()
  elections_geo_spdf$audience_count_jul32_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_jul32[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")	
  elections_geo_spdf$speeches_count_jul32_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_jul32[.,] %>% nrow()
  # 1932/2, full period
  mindist <- rdist.earth(coordinates(cbind(speeches_df_nov32$lon, speeches_df_nov32$lat)), coordinates(elections_sp[i]), miles = FALSE, R = 6378)
  elections_geo_spdf$audience_count_nov32_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_nov32[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_nov32_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_nov32[.,] %>% nrow()
  elections_geo_spdf$audience_count_nov32_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_nov32[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_nov32_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_nov32[.,] %>% nrow()
  elections_geo_spdf$audience_count_nov32_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_nov32[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_nov32_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_nov32[.,] %>% nrow()
  elections_geo_spdf$audience_count_nov32_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_nov32[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")	
  elections_geo_spdf$speeches_count_nov32_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_nov32[.,] %>% nrow()
  # 1933, full period
  mindist <- rdist.earth(coordinates(cbind(speeches_df_mar33$lon, speeches_df_mar33$lat)), coordinates(elections_sp[i]), miles = FALSE, R = 6378)
  elections_geo_spdf$audience_count_mar33_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_mar33[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_mar33_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_mar33[.,] %>% nrow()
  elections_geo_spdf$audience_count_mar33_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_mar33[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_mar33_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_mar33[.,] %>% nrow()
  elections_geo_spdf$audience_count_mar33_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_mar33[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_mar33_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_mar33[.,] %>% nrow()
  elections_geo_spdf$audience_count_mar33_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_mar33[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")	
  elections_geo_spdf$speeches_count_mar33_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_mar33[.,] %>% nrow()
  # 1932/p1, full period
  mindist <- rdist.earth(coordinates(cbind(speeches_df_mar32$lon, speeches_df_mar32$lat)), coordinates(elections_sp[i]), miles = FALSE, R = 6378)
  elections_geo_spdf$audience_count_mar32_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_mar32[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_mar32_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_mar32[.,] %>% nrow()
  elections_geo_spdf$audience_count_mar32_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_mar32[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_mar32_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_mar32[.,] %>% nrow()
  elections_geo_spdf$audience_count_mar32_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_mar32[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_mar32_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_mar32[.,] %>% nrow()
  elections_geo_spdf$audience_count_mar32_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_mar32[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")	
  elections_geo_spdf$speeches_count_mar32_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_mar32[.,] %>% nrow()  
  # 1932/p2, full period
  mindist <- rdist.earth(coordinates(cbind(speeches_df_apr32$lon, speeches_df_apr32$lat)), coordinates(elections_sp[i]), miles = FALSE, R = 6378)
  elections_geo_spdf$audience_count_apr32_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_apr32[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_apr32_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_apr32[.,] %>% nrow()
  elections_geo_spdf$audience_count_apr32_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_apr32[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_apr32_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_apr32[.,] %>% nrow()
  elections_geo_spdf$audience_count_apr32_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_apr32[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_apr32_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_apr32[.,] %>% nrow()
  elections_geo_spdf$audience_count_apr32_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_apr32[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")	
  elections_geo_spdf$speeches_count_apr32_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_apr32[.,] %>% nrow()
  # 1928, restricted periods
  mindist <- rdist.earth(coordinates(cbind(speeches_df_may28_2w$lon, speeches_df_may28_2w$lat)), coordinates(elections_sp[i]), miles = FALSE, R = 6378)
  elections_geo_spdf$audience_count_may28_2w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_may28_2w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_may28_2w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_may28_2w[.,] %>% nrow()
  elections_geo_spdf$audience_count_may28_2w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_may28_2w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_may28_2w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_may28_2w[.,] %>% nrow()
  elections_geo_spdf$audience_count_may28_2w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_may28_2w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_may28_2w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_may28_2w[.,] %>% nrow()
  elections_geo_spdf$audience_count_may28_2w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_may28_2w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")	
  elections_geo_spdf$speeches_count_may28_2w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_may28_2w[.,] %>% nrow()
  mindist <- rdist.earth(coordinates(cbind(speeches_df_may28$lon, speeches_df_may28$lat)), coordinates(elections_sp[i]), miles = FALSE, R = 6378)
  elections_geo_spdf$audience_count_may28_4w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_may28_4w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_may28_4w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_may28_4w[.,] %>% nrow()
  elections_geo_spdf$audience_count_may28_4w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_may28_4w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_may28_4w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_may28_4w[.,] %>% nrow()
  elections_geo_spdf$audience_count_may28_4w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_may28_4w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_may28_4w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_may28_4w[.,] %>% nrow()
  elections_geo_spdf$audience_count_may28_4w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_may28_4w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")	
  elections_geo_spdf$speeches_count_may28_4w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_may28_4w[.,] %>% nrow()
  mindist <- rdist.earth(coordinates(cbind(speeches_df_may28_8w$lon, speeches_df_may28_8w$lat)), coordinates(elections_sp[i]), miles = FALSE, R = 6378)
  elections_geo_spdf$audience_count_may28_8w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_may28_8w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_may28_8w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_may28_8w[.,] %>% nrow()
  elections_geo_spdf$audience_count_may28_8w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_may28_8w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_may28_8w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_may28_8w[.,] %>% nrow()
  elections_geo_spdf$audience_count_may28_8w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_may28_8w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_may28_8w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_may28_8w[.,] %>% nrow()
  elections_geo_spdf$audience_count_may28_8w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_may28_8w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")	
  elections_geo_spdf$speeches_count_may28_8w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_may28_8w[.,] %>% nrow()
  mindist <- rdist.earth(coordinates(cbind(speeches_df_may28_12w$lon, speeches_df_may28_12w$lat)), coordinates(elections_sp[i]), miles = FALSE, R = 6378)
  elections_geo_spdf$audience_count_may28_12w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_may28_12w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_may28_12w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_may28_12w[.,] %>% nrow()
  elections_geo_spdf$audience_count_may28_12w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_may28_12w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_may28_12w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_may28_12w[.,] %>% nrow()
  elections_geo_spdf$audience_count_may28_12w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_may28_12w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_may28_12w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_may28_12w[.,] %>% nrow()
  elections_geo_spdf$audience_count_may28_12w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_may28_12w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")	
  elections_geo_spdf$speeches_count_may28_12w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_may28_12w[.,] %>% nrow()
  # 1930, restricted periods
  mindist <- rdist.earth(coordinates(cbind(speeches_df_sep30_2w$lon, speeches_df_sep30_2w$lat)), coordinates(elections_sp[i]), miles = FALSE, R = 6378)
  elections_geo_spdf$audience_count_sep30_2w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_sep30_2w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_sep30_2w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_sep30_2w[.,] %>% nrow()
  elections_geo_spdf$audience_count_sep30_2w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_sep30_2w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_sep30_2w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_sep30_2w[.,] %>% nrow()
  elections_geo_spdf$audience_count_sep30_2w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_sep30_2w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_sep30_2w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_sep30_2w[.,] %>% nrow()
  elections_geo_spdf$audience_count_sep30_2w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_sep30_2w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")	
  elections_geo_spdf$speeches_count_sep30_2w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_sep30_2w[.,] %>% nrow()
  mindist <- rdist.earth(coordinates(cbind(speeches_df_sep30$lon, speeches_df_sep30$lat)), coordinates(elections_sp[i]), miles = FALSE, R = 6378)
  elections_geo_spdf$audience_count_sep30_4w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_sep30_4w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_sep30_4w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_sep30_4w[.,] %>% nrow()
  elections_geo_spdf$audience_count_sep30_4w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_sep30_4w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_sep30_4w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_sep30_4w[.,] %>% nrow()
  elections_geo_spdf$audience_count_sep30_4w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_sep30_4w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_sep30_4w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_sep30_4w[.,] %>% nrow()
  elections_geo_spdf$audience_count_sep30_4w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_sep30_4w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")	
  elections_geo_spdf$speeches_count_sep30_4w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_sep30_4w[.,] %>% nrow()
  mindist <- rdist.earth(coordinates(cbind(speeches_df_sep30_8w$lon, speeches_df_sep30_8w$lat)), coordinates(elections_sp[i]), miles = FALSE, R = 6378)
  elections_geo_spdf$audience_count_sep30_8w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_sep30_8w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_sep30_8w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_sep30_8w[.,] %>% nrow()
  elections_geo_spdf$audience_count_sep30_8w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_sep30_8w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_sep30_8w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_sep30_8w[.,] %>% nrow()
  elections_geo_spdf$audience_count_sep30_8w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_sep30_8w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_sep30_8w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_sep30_8w[.,] %>% nrow()
  elections_geo_spdf$audience_count_sep30_8w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_sep30_8w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")	
  elections_geo_spdf$speeches_count_sep30_8w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_sep30_8w[.,] %>% nrow()
  mindist <- rdist.earth(coordinates(cbind(speeches_df_sep30_12w$lon, speeches_df_sep30_12w$lat)), coordinates(elections_sp[i]), miles = FALSE, R = 6378)
  elections_geo_spdf$audience_count_sep30_12w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_sep30_12w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_sep30_12w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_sep30_12w[.,] %>% nrow()
  elections_geo_spdf$audience_count_sep30_12w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_sep30_12w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_sep30_12w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_sep30_12w[.,] %>% nrow()
  elections_geo_spdf$audience_count_sep30_12w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_sep30_12w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_sep30_12w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_sep30_12w[.,] %>% nrow()
  elections_geo_spdf$audience_count_sep30_12w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_sep30_12w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")	
  elections_geo_spdf$speeches_count_sep30_12w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_sep30_12w[.,] %>% nrow()
  # 1932/1, restricted periods
  mindist <- rdist.earth(coordinates(cbind(speeches_df_jul32_2w$lon, speeches_df_jul32_2w$lat)), coordinates(elections_sp[i]), miles = FALSE, R = 6378)
  elections_geo_spdf$audience_count_jul32_2w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_jul32_2w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_jul32_2w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_jul32_2w[.,] %>% nrow()
  elections_geo_spdf$audience_count_jul32_2w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_jul32_2w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_jul32_2w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_jul32_2w[.,] %>% nrow()
  elections_geo_spdf$audience_count_jul32_2w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_jul32_2w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_jul32_2w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_jul32_2w[.,] %>% nrow()
  elections_geo_spdf$audience_count_jul32_2w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_jul32_2w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")	
  elections_geo_spdf$speeches_count_jul32_2w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_jul32_2w[.,] %>% nrow()
  mindist <- rdist.earth(coordinates(cbind(speeches_df_jul32$lon, speeches_df_jul32$lat)), coordinates(elections_sp[i]), miles = FALSE, R = 6378)
  elections_geo_spdf$audience_count_jul32_4w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_jul32_4w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_jul32_4w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_jul32_4w[.,] %>% nrow()
  elections_geo_spdf$audience_count_jul32_4w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_jul32_4w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_jul32_4w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_jul32_4w[.,] %>% nrow()
  elections_geo_spdf$audience_count_jul32_4w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_jul32_4w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_jul32_4w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_jul32_4w[.,] %>% nrow()
  elections_geo_spdf$audience_count_jul32_4w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_jul32_4w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")	
  elections_geo_spdf$speeches_count_jul32_4w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_jul32_4w[.,] %>% nrow()
  mindist <- rdist.earth(coordinates(cbind(speeches_df_jul32_8w$lon, speeches_df_jul32_8w$lat)), coordinates(elections_sp[i]), miles = FALSE, R = 6378)
  elections_geo_spdf$audience_count_jul32_8w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_jul32_8w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_jul32_8w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_jul32_8w[.,] %>% nrow()
  elections_geo_spdf$audience_count_jul32_8w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_jul32_8w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_jul32_8w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_jul32_8w[.,] %>% nrow()
  elections_geo_spdf$audience_count_jul32_8w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_jul32_8w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_jul32_8w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_jul32_8w[.,] %>% nrow()
  elections_geo_spdf$audience_count_jul32_8w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_jul32_8w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")	
  elections_geo_spdf$speeches_count_jul32_8w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_jul32_8w[.,] %>% nrow()
  mindist <- rdist.earth(coordinates(cbind(speeches_df_jul32_12w$lon, speeches_df_jul32_12w$lat)), coordinates(elections_sp[i]), miles = FALSE, R = 6378)
  elections_geo_spdf$audience_count_jul32_12w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_jul32_12w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_jul32_12w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_jul32_12w[.,] %>% nrow()
  elections_geo_spdf$audience_count_jul32_12w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_jul32_12w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_jul32_12w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_jul32_12w[.,] %>% nrow()
  elections_geo_spdf$audience_count_jul32_12w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_jul32_12w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_jul32_12w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_jul32_12w[.,] %>% nrow()
  elections_geo_spdf$audience_count_jul32_12w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_jul32_12w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")	
  elections_geo_spdf$speeches_count_jul32_12w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_jul32_12w[.,] %>% nrow()
  # 1932/2, restricted periods
  mindist <- rdist.earth(coordinates(cbind(speeches_df_nov32_2w$lon, speeches_df_nov32_2w$lat)), coordinates(elections_sp[i]), miles = FALSE, R = 6378)
  elections_geo_spdf$audience_count_nov32_2w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_nov32_2w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_nov32_2w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_nov32_2w[.,] %>% nrow()
  elections_geo_spdf$audience_count_nov32_2w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_nov32_2w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_nov32_2w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_nov32_2w[.,] %>% nrow()
  elections_geo_spdf$audience_count_nov32_2w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_nov32_2w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_nov32_2w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_nov32_2w[.,] %>% nrow()
  elections_geo_spdf$audience_count_nov32_2w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_nov32_2w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")	
  elections_geo_spdf$speeches_count_nov32_2w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_nov32_2w[.,] %>% nrow()
  mindist <- rdist.earth(coordinates(cbind(speeches_df_nov32$lon, speeches_df_nov32$lat)), coordinates(elections_sp[i]), miles = FALSE, R = 6378)
  elections_geo_spdf$audience_count_nov32_4w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_nov32_4w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_nov32_4w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_nov32_4w[.,] %>% nrow()
  elections_geo_spdf$audience_count_nov32_4w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_nov32_4w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_nov32_4w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_nov32_4w[.,] %>% nrow()
  elections_geo_spdf$audience_count_nov32_4w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_nov32_4w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_nov32_4w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_nov32_4w[.,] %>% nrow()
  elections_geo_spdf$audience_count_nov32_4w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_nov32_4w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")	
  elections_geo_spdf$speeches_count_nov32_4w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_nov32_4w[.,] %>% nrow()
  mindist <- rdist.earth(coordinates(cbind(speeches_df_nov32_8w$lon, speeches_df_nov32_8w$lat)), coordinates(elections_sp[i]), miles = FALSE, R = 6378)
  elections_geo_spdf$audience_count_nov32_8w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_nov32_8w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_nov32_8w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_nov32_8w[.,] %>% nrow()
  elections_geo_spdf$audience_count_nov32_8w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_nov32_8w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_nov32_8w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_nov32_8w[.,] %>% nrow()
  elections_geo_spdf$audience_count_nov32_8w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_nov32_8w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_nov32_8w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_nov32_8w[.,] %>% nrow()
  elections_geo_spdf$audience_count_nov32_8w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_nov32_8w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")	
  elections_geo_spdf$speeches_count_nov32_8w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_nov32_8w[.,] %>% nrow()
  mindist <- rdist.earth(coordinates(cbind(speeches_df_nov32_12w$lon, speeches_df_nov32_12w$lat)), coordinates(elections_sp[i]), miles = FALSE, R = 6378)
  elections_geo_spdf$audience_count_nov32_12w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_nov32_12w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_nov32_12w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_nov32_12w[.,] %>% nrow()
  elections_geo_spdf$audience_count_nov32_12w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_nov32_12w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_nov32_12w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_nov32_12w[.,] %>% nrow()
  elections_geo_spdf$audience_count_nov32_12w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_nov32_12w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_nov32_12w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_nov32_12w[.,] %>% nrow()
  elections_geo_spdf$audience_count_nov32_12w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_nov32_12w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")	
  elections_geo_spdf$speeches_count_nov32_12w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_nov32_12w[.,] %>% nrow()
  # 1933, restricted periods
  mindist <- rdist.earth(coordinates(cbind(speeches_df_mar33_2w$lon, speeches_df_mar33_2w$lat)), coordinates(elections_sp[i]), miles = FALSE, R = 6378)
  elections_geo_spdf$audience_count_mar33_2w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_mar33_2w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_mar33_2w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_mar33_2w[.,] %>% nrow()
  elections_geo_spdf$audience_count_mar33_2w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_mar33_2w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_mar33_2w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_mar33_2w[.,] %>% nrow()
  elections_geo_spdf$audience_count_mar33_2w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_mar33_2w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_mar33_2w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_mar33_2w[.,] %>% nrow()
  elections_geo_spdf$audience_count_mar33_2w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_mar33_2w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")	
  elections_geo_spdf$speeches_count_mar33_2w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_mar33_2w[.,] %>% nrow()
  mindist <- rdist.earth(coordinates(cbind(speeches_df_mar33$lon, speeches_df_mar33$lat)), coordinates(elections_sp[i]), miles = FALSE, R = 6378)
  elections_geo_spdf$audience_count_mar33_4w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_mar33_4w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_mar33_4w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_mar33_4w[.,] %>% nrow()
  elections_geo_spdf$audience_count_mar33_4w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_mar33_4w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_mar33_4w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_mar33_4w[.,] %>% nrow()
  elections_geo_spdf$audience_count_mar33_4w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_mar33_4w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_mar33_4w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_mar33_4w[.,] %>% nrow()
  elections_geo_spdf$audience_count_mar33_4w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_mar33_4w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")	
  elections_geo_spdf$speeches_count_mar33_4w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_mar33_4w[.,] %>% nrow()
  mindist <- rdist.earth(coordinates(cbind(speeches_df_mar33_8w$lon, speeches_df_mar33_8w$lat)), coordinates(elections_sp[i]), miles = FALSE, R = 6378)
  elections_geo_spdf$audience_count_mar33_8w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_mar33_8w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_mar33_8w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_mar33_8w[.,] %>% nrow()
  elections_geo_spdf$audience_count_mar33_8w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_mar33_8w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_mar33_8w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_mar33_8w[.,] %>% nrow()
  elections_geo_spdf$audience_count_mar33_8w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_mar33_8w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_mar33_8w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_mar33_8w[.,] %>% nrow()
  elections_geo_spdf$audience_count_mar33_8w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_mar33_8w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")	
  elections_geo_spdf$speeches_count_mar33_8w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_mar33_8w[.,] %>% nrow()
  mindist <- rdist.earth(coordinates(cbind(speeches_df_mar33_12w$lon, speeches_df_mar33_12w$lat)), coordinates(elections_sp[i]), miles = FALSE, R = 6378)
  elections_geo_spdf$audience_count_mar33_12w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_mar33_12w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_mar33_12w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_mar33_12w[.,] %>% nrow()
  elections_geo_spdf$audience_count_mar33_12w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_mar33_12w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_mar33_12w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_mar33_12w[.,] %>% nrow()
  elections_geo_spdf$audience_count_mar33_12w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_mar33_12w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_mar33_12w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_mar33_12w[.,] %>% nrow()
  elections_geo_spdf$audience_count_mar33_12w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_mar33_12w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")	
  elections_geo_spdf$speeches_count_mar33_12w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_mar33_12w[.,] %>% nrow()
# 1932/p1, restricted periods
  mindist <- rdist.earth(coordinates(cbind(speeches_df_mar32_2w$lon, speeches_df_mar32_2w$lat)), coordinates(elections_sp[i]), miles = FALSE, R = 6378)
  elections_geo_spdf$audience_count_mar32_2w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_mar32_2w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_mar32_2w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_mar32_2w[.,] %>% nrow()
  elections_geo_spdf$audience_count_mar32_2w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_mar32_2w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_mar32_2w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_mar32_2w[.,] %>% nrow()
  elections_geo_spdf$audience_count_mar32_2w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_mar32_2w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_mar32_2w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_mar32_2w[.,] %>% nrow()
  elections_geo_spdf$audience_count_mar32_2w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_mar32_2w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")	
  elections_geo_spdf$speeches_count_mar32_2w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_mar32_2w[.,] %>% nrow()
  mindist <- rdist.earth(coordinates(cbind(speeches_df_mar32$lon, speeches_df_mar32$lat)), coordinates(elections_sp[i]), miles = FALSE, R = 6378)
  elections_geo_spdf$audience_count_mar32_4w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_mar32_4w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_mar32_4w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_mar32_4w[.,] %>% nrow()
  elections_geo_spdf$audience_count_mar32_4w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_mar32_4w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_mar32_4w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_mar32_4w[.,] %>% nrow()
  elections_geo_spdf$audience_count_mar32_4w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_mar32_4w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_mar32_4w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_mar32_4w[.,] %>% nrow()
  elections_geo_spdf$audience_count_mar32_4w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_mar32_4w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")	
  elections_geo_spdf$speeches_count_mar32_4w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_mar32_4w[.,] %>% nrow()
  mindist <- rdist.earth(coordinates(cbind(speeches_df_mar32_8w$lon, speeches_df_mar32_8w$lat)), coordinates(elections_sp[i]), miles = FALSE, R = 6378)
  elections_geo_spdf$audience_count_mar32_8w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_mar32_8w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_mar32_8w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_mar32_8w[.,] %>% nrow()
  elections_geo_spdf$audience_count_mar32_8w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_mar32_8w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_mar32_8w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_mar32_8w[.,] %>% nrow()
  elections_geo_spdf$audience_count_mar32_8w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_mar32_8w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_mar32_8w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_mar32_8w[.,] %>% nrow()
  elections_geo_spdf$audience_count_mar32_8w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_mar32_8w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")	
  elections_geo_spdf$speeches_count_mar32_8w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_mar32_8w[.,] %>% nrow()
  mindist <- rdist.earth(coordinates(cbind(speeches_df_mar32_12w$lon, speeches_df_mar32_12w$lat)), coordinates(elections_sp[i]), miles = FALSE, R = 6378)
  elections_geo_spdf$audience_count_mar32_12w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_mar32_12w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_mar32_12w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_mar32_12w[.,] %>% nrow()
  elections_geo_spdf$audience_count_mar32_12w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_mar32_12w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_mar32_12w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_mar32_12w[.,] %>% nrow()
  elections_geo_spdf$audience_count_mar32_12w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_mar32_12w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_mar32_12w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_mar32_12w[.,] %>% nrow()
  elections_geo_spdf$audience_count_mar32_12w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_mar32_12w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")	
  elections_geo_spdf$speeches_count_mar32_12w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_mar32_12w[.,] %>% nrow()
  # 1932/p2, restricted periods
  mindist <- rdist.earth(coordinates(cbind(speeches_df_apr32_2w$lon, speeches_df_apr32_2w$lat)), coordinates(elections_sp[i]), miles = FALSE, R = 6378)
  elections_geo_spdf$audience_count_apr32_2w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_apr32_2w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_apr32_2w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_apr32_2w[.,] %>% nrow()
  elections_geo_spdf$audience_count_apr32_2w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_apr32_2w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_apr32_2w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_apr32_2w[.,] %>% nrow()
  elections_geo_spdf$audience_count_apr32_2w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_apr32_2w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_apr32_2w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_apr32_2w[.,] %>% nrow()
  elections_geo_spdf$audience_count_apr32_2w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_apr32_2w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")	
  elections_geo_spdf$speeches_count_apr32_2w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_apr32_2w[.,] %>% nrow()
  mindist <- rdist.earth(coordinates(cbind(speeches_df_apr32$lon, speeches_df_apr32$lat)), coordinates(elections_sp[i]), miles = FALSE, R = 6378)
  elections_geo_spdf$audience_count_apr32_4w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_apr32_4w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_apr32_4w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_apr32_4w[.,] %>% nrow()
  elections_geo_spdf$audience_count_apr32_4w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_apr32_4w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_apr32_4w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_apr32_4w[.,] %>% nrow()
  elections_geo_spdf$audience_count_apr32_4w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_apr32_4w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_apr32_4w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_apr32_4w[.,] %>% nrow()
  elections_geo_spdf$audience_count_apr32_4w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_apr32_4w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")	
  elections_geo_spdf$speeches_count_apr32_4w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_apr32_4w[.,] %>% nrow()
  mindist <- rdist.earth(coordinates(cbind(speeches_df_apr32_8w$lon, speeches_df_apr32_8w$lat)), coordinates(elections_sp[i]), miles = FALSE, R = 6378)
  elections_geo_spdf$audience_count_apr32_8w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_apr32_8w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_apr32_8w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_apr32_8w[.,] %>% nrow()
  elections_geo_spdf$audience_count_apr32_8w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_apr32_8w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_apr32_8w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_apr32_8w[.,] %>% nrow()
  elections_geo_spdf$audience_count_apr32_8w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_apr32_8w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_apr32_8w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_apr32_8w[.,] %>% nrow()
  elections_geo_spdf$audience_count_apr32_8w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_apr32_8w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")	
  elections_geo_spdf$speeches_count_apr32_8w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_apr32_8w[.,] %>% nrow()
  mindist <- rdist.earth(coordinates(cbind(speeches_df_apr32_12w$lon, speeches_df_apr32_12w$lat)), coordinates(elections_sp[i]), miles = FALSE, R = 6378)
  elections_geo_spdf$audience_count_apr32_12w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_apr32_12w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_apr32_12w_5km[i] <-  mindist %>% is_less_than(5) %>% as.vector %>% speeches_df_apr32_12w[.,] %>% nrow()
  elections_geo_spdf$audience_count_apr32_12w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_apr32_12w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_apr32_12w_10km[i] <-  mindist %>% is_less_than(10) %>% as.vector %>% speeches_df_apr32_12w[.,] %>% nrow()
  elections_geo_spdf$audience_count_apr32_12w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_apr32_12w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")
  elections_geo_spdf$speeches_count_apr32_12w_25km[i] <-  mindist %>% is_less_than(25) %>% as.vector %>% speeches_df_apr32_12w[.,] %>% nrow()
  elections_geo_spdf$audience_count_apr32_12w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_apr32_12w[.,] %>% use_series(audience_police_num_imputed) %>% sum(na.rm = TRUE)#  %>% recode("0 = NA")	
  elections_geo_spdf$speeches_count_apr32_12w_50km[i] <-  mindist %>% is_less_than(50) %>% as.vector %>% speeches_df_apr32_12w[.,] %>% nrow()
}





## import and process Goebbels speeches data --------------
load("goebbels_df.RData")
goebbels_df$date <- ymd(goebbels_df$date)

# retrieve SpatialPoints
goebbels_sp <- SpatialPoints(cbind(goebbels_df$lon, goebbels_df$lat))
proj4string(goebbels_sp) <- CRS(projection)

# generate goebbels variable dummies - full range between elections, 12, 8, 4, 2 weeks before election
goebbels_df_may28 <- filter(goebbels_df, date > "1924-12-07", date < "1928-05-20")
goebbels_df_sep30 <- filter(goebbels_df, date > "1928-05-20", date < "1930-09-14")
goebbels_df_jul32 <- filter(goebbels_df, date > "1930-09-14", date < "1932-07-31")
goebbels_df_nov32 <- filter(goebbels_df, date > "1932-07-31", date < "1932-11-06")
goebbels_df_mar33 <- filter(goebbels_df, date > "1932-11-06", date < "1933-03-05")
goebbels_df_mar32 <- filter(goebbels_df, date > "1930-09-14", date < "1932-03-13")
goebbels_df_apr32 <- filter(goebbels_df, date > "1932-03-13", date < "1932-04-10")

goebbels_df_may28_2w <- filter(goebbels_df, date > ymd("1928-05-20") - weeks(2), date < "1928-05-20")
goebbels_df_may28_4w <- filter(goebbels_df, date > ymd("1928-05-20") - weeks(4), date < "1928-05-20")
goebbels_df_may28_8w <- filter(goebbels_df, date > ymd("1928-05-20") - weeks(8), date < "1928-05-20")
goebbels_df_may28_12w <- filter(goebbels_df, date > ymd("1928-05-20") - weeks(12), date < "1928-05-20")

goebbels_df_sep30_2w <- filter(goebbels_df, date > ymd("1930-09-14") - weeks(2), date < "1930-09-14")
goebbels_df_sep30_4w <- filter(goebbels_df, date > ymd("1930-09-14") - weeks(4), date < "1930-09-14")
goebbels_df_sep30_8w <- filter(goebbels_df, date > ymd("1930-09-14") - weeks(8), date < "1930-09-14")
goebbels_df_sep30_12w <- filter(goebbels_df, date > ymd("1930-09-14") - weeks(12), date < "1930-09-14")

goebbels_df_jul32_2w <- filter(goebbels_df, date > ymd("1932-07-31") - weeks(2), date < "1932-07-31")
goebbels_df_jul32_4w <- filter(goebbels_df, date > ymd("1932-07-31") - weeks(4), date < "1932-07-31")
goebbels_df_jul32_8w <- filter(goebbels_df, date > ymd("1932-07-31") - weeks(8), date < "1932-07-31")
goebbels_df_jul32_12w <- filter(goebbels_df, date > ymd("1932-07-31") - weeks(12), date < "1932-07-31")

goebbels_df_nov32_2w <- filter(goebbels_df, date > ymd("1932-11-06") - weeks(2), date < "1932-11-06")
goebbels_df_nov32_4w <- filter(goebbels_df, date > ymd("1932-11-06") - weeks(4), date < "1932-11-06")
goebbels_df_nov32_8w <- filter(goebbels_df, date > ymd("1932-11-06") - weeks(8), date < "1932-11-06")
goebbels_df_nov32_12w <- filter(goebbels_df, date > ymd("1932-11-06") - weeks(12), date < "1932-11-06")

goebbels_df_mar33_2w <- filter(goebbels_df, date > ymd("1933-03-05") - weeks(2), date < "1933-03-05")
goebbels_df_mar33_4w <- filter(goebbels_df, date > ymd("1933-03-05") - weeks(4), date < "1933-03-05")
goebbels_df_mar33_8w <- filter(goebbels_df, date > ymd("1933-03-05") - weeks(8), date < "1933-03-05")
goebbels_df_mar33_12w <- filter(goebbels_df, date > ymd("1933-03-05") - weeks(12), date < "1933-03-05")

goebbels_df_mar32_2w <- filter(goebbels_df, date > ymd("1932-03-13") - weeks(2), date < "1932-03-13")
goebbels_df_mar32_4w <- filter(goebbels_df, date > ymd("1932-03-13") - weeks(4), date < "1932-03-13")
goebbels_df_mar32_8w <- filter(goebbels_df, date > ymd("1932-03-13") - weeks(8), date < "1932-03-13")
goebbels_df_mar32_12w <- filter(goebbels_df, date > ymd("1932-03-13") - weeks(12), date < "1932-03-13")

goebbels_df_apr32_2w <- filter(goebbels_df, date > ymd("1932-04-10") - weeks(2), date < "1932-04-10")
goebbels_df_apr32_4w <- filter(goebbels_df, date > ymd("1932-04-10") - weeks(4), date < "1932-04-10")
goebbels_df_apr32_8w <- filter(goebbels_df, date > ymd("1932-04-10") - weeks(8), date < "1932-04-10")
goebbels_df_apr32_12w <- filter(goebbels_df, date > ymd("1932-04-10") - weeks(12), date < "1932-04-10")


# generate goebbels indicator in community-level data ("did hitler give a goebbels near the community (within 5km, 10km, 25km, 50km) in a given time period (full period, 2, 4, 8, 12 weeks before the election?")

# 1928, full period
mindist <- rdist.earth(coordinates(cbind(goebbels_df_may28$lon, goebbels_df_may28$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$goebbels_may28_5km <- mindist < 5
elections_geo_spdf$goebbels_may28_10km <- mindist < 10
elections_geo_spdf$goebbels_may28_25km <- mindist < 25
elections_geo_spdf$goebbels_may28_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$goebbels_may28_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$goebbels_may28_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$goebbels_may28_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$goebbels_may28_50km_nomatch <- mindist > 50 & mindist < 60

# 1930, full period
mindist <- rdist.earth(coordinates(cbind(goebbels_df_sep30$lon, goebbels_df_sep30$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$goebbels_sep30_5km <- mindist < 5
elections_geo_spdf$goebbels_sep30_10km <- mindist < 10
elections_geo_spdf$goebbels_sep30_25km <- mindist < 25
elections_geo_spdf$goebbels_sep30_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$goebbels_sep30_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$goebbels_sep30_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$goebbels_sep30_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$goebbels_sep30_50km_nomatch <- mindist > 50 & mindist < 60

# 1932/1, full period
mindist <- rdist.earth(coordinates(cbind(goebbels_df_jul32$lon, goebbels_df_jul32$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$goebbels_jul32_5km <- mindist < 5
elections_geo_spdf$goebbels_jul32_10km <- mindist < 10
elections_geo_spdf$goebbels_jul32_25km <- mindist < 25
elections_geo_spdf$goebbels_jul32_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$goebbels_jul32_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$goebbels_jul32_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$goebbels_jul32_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$goebbels_jul32_50km_nomatch <- mindist > 50 & mindist < 60

# 1932/2, full period
mindist <- rdist.earth(coordinates(cbind(goebbels_df_nov32$lon, goebbels_df_nov32$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$goebbels_nov32_5km <- mindist < 5
elections_geo_spdf$goebbels_nov32_10km <- mindist < 10
elections_geo_spdf$goebbels_nov32_25km <- mindist < 25
elections_geo_spdf$goebbels_nov32_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$goebbels_nov32_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$goebbels_nov32_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$goebbels_nov32_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$goebbels_nov32_50km_nomatch <- mindist > 50 & mindist < 60

# 1933, full period
mindist <- rdist.earth(coordinates(cbind(goebbels_df_mar33$lon, goebbels_df_mar33$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$goebbels_mar33_5km <- mindist < 5
elections_geo_spdf$goebbels_mar33_10km <- mindist < 10
elections_geo_spdf$goebbels_mar33_25km <- mindist < 25
elections_geo_spdf$goebbels_mar33_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$goebbels_mar33_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$goebbels_mar33_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$goebbels_mar33_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$goebbels_mar33_50km_nomatch <- mindist > 50 & mindist < 60

# 1932/p1, full period
mindist <- rdist.earth(coordinates(cbind(goebbels_df_mar32$lon, goebbels_df_mar32$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$goebbels_mar32_5km <- mindist < 5
elections_geo_spdf$goebbels_mar32_10km <- mindist < 10
elections_geo_spdf$goebbels_mar32_25km <- mindist < 25
elections_geo_spdf$goebbels_mar32_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$goebbels_mar32_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$goebbels_mar32_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$goebbels_mar32_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$goebbels_mar32_50km_nomatch <- mindist > 50 & mindist < 60

# 1932/p2, full period
mindist <- rdist.earth(coordinates(cbind(goebbels_df_apr32$lon, goebbels_df_apr32$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$goebbels_apr32_5km <- mindist < 5
elections_geo_spdf$goebbels_apr32_10km <- mindist < 10
elections_geo_spdf$goebbels_apr32_25km <- mindist < 25
elections_geo_spdf$goebbels_apr32_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$goebbels_apr32_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$goebbels_apr32_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$goebbels_apr32_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$goebbels_apr32_50km_nomatch <- mindist > 50 & mindist < 60


# 1928, restricted periods
mindist <- rdist.earth(coordinates(cbind(goebbels_df_may28_2w$lon, goebbels_df_may28_2w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$goebbels_may28_2w_5km <- mindist < 5
elections_geo_spdf$goebbels_may28_2w_10km <- mindist < 10
elections_geo_spdf$goebbels_may28_2w_25km <- mindist < 25
elections_geo_spdf$goebbels_may28_2w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$goebbels_may28_2w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$goebbels_may28_2w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$goebbels_may28_2w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$goebbels_may28_2w_50km_nomatch <- mindist > 50 & mindist < 60
mindist <- rdist.earth(coordinates(cbind(goebbels_df_may28_4w$lon, goebbels_df_may28_4w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$goebbels_may28_4w_5km <- mindist < 5
elections_geo_spdf$goebbels_may28_4w_10km <- mindist < 10
elections_geo_spdf$goebbels_may28_4w_25km <- mindist < 25
elections_geo_spdf$goebbels_may28_4w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$goebbels_may28_4w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$goebbels_may28_4w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$goebbels_may28_4w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$goebbels_may28_4w_50km_nomatch <- mindist > 50 & mindist < 60
mindist <- rdist.earth(coordinates(cbind(goebbels_df_may28_8w$lon, goebbels_df_may28_8w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$goebbels_may28_8w_5km <- mindist < 5
elections_geo_spdf$goebbels_may28_8w_10km <- mindist < 10
elections_geo_spdf$goebbels_may28_8w_25km <- mindist < 25
elections_geo_spdf$goebbels_may28_8w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$goebbels_may28_8w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$goebbels_may28_8w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$goebbels_may28_8w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$goebbels_may28_8w_50km_nomatch <- mindist > 50 & mindist < 60
mindist <- rdist.earth(coordinates(cbind(goebbels_df_may28_12w$lon, goebbels_df_may28_12w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$goebbels_may28_12w_5km <- mindist < 5
elections_geo_spdf$goebbels_may28_12w_10km <- mindist < 10
elections_geo_spdf$goebbels_may28_12w_25km <- mindist < 25
elections_geo_spdf$goebbels_may28_12w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$goebbels_may28_12w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$goebbels_may28_12w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$goebbels_may28_12w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$goebbels_may28_12w_50km_nomatch <- mindist > 50 & mindist < 60

# 1930, restricted periods
mindist <- rdist.earth(coordinates(cbind(goebbels_df_sep30_2w$lon, goebbels_df_sep30_2w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$goebbels_sep30_2w_5km <- mindist < 5
elections_geo_spdf$goebbels_sep30_2w_10km <- mindist < 10
elections_geo_spdf$goebbels_sep30_2w_25km <- mindist < 25
elections_geo_spdf$goebbels_sep30_2w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$goebbels_sep30_2w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$goebbels_sep30_2w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$goebbels_sep30_2w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$goebbels_sep30_2w_50km_nomatch <- mindist > 50 & mindist < 60
mindist <- rdist.earth(coordinates(cbind(goebbels_df_sep30_4w$lon, goebbels_df_sep30_4w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$goebbels_sep30_4w_5km <- mindist < 5
elections_geo_spdf$goebbels_sep30_4w_10km <- mindist < 10
elections_geo_spdf$goebbels_sep30_4w_25km <- mindist < 25
elections_geo_spdf$goebbels_sep30_4w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$goebbels_sep30_4w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$goebbels_sep30_4w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$goebbels_sep30_4w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$goebbels_sep30_4w_50km_nomatch <- mindist > 50 & mindist < 60
mindist <- rdist.earth(coordinates(cbind(goebbels_df_sep30_8w$lon, goebbels_df_sep30_8w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$goebbels_sep30_8w_5km <- mindist < 5
elections_geo_spdf$goebbels_sep30_8w_10km <- mindist < 10
elections_geo_spdf$goebbels_sep30_8w_25km <- mindist < 25
elections_geo_spdf$goebbels_sep30_8w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$goebbels_sep30_8w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$goebbels_sep30_8w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$goebbels_sep30_8w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$goebbels_sep30_8w_50km_nomatch <- mindist > 50 & mindist < 60
mindist <- rdist.earth(coordinates(cbind(goebbels_df_sep30_12w$lon, goebbels_df_sep30_12w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$goebbels_sep30_12w_5km <- mindist < 5
elections_geo_spdf$goebbels_sep30_12w_10km <- mindist < 10
elections_geo_spdf$goebbels_sep30_12w_25km <- mindist < 25
elections_geo_spdf$goebbels_sep30_12w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$goebbels_sep30_12w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$goebbels_sep30_12w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$goebbels_sep30_12w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$goebbels_sep30_12w_50km_nomatch <- mindist > 50 & mindist < 60

# 1932/1, restricted periods
mindist <- rdist.earth(coordinates(cbind(goebbels_df_jul32_2w$lon, goebbels_df_jul32_2w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$goebbels_jul32_2w_5km <- mindist < 5
elections_geo_spdf$goebbels_jul32_2w_10km <- mindist < 10
elections_geo_spdf$goebbels_jul32_2w_25km <- mindist < 25
elections_geo_spdf$goebbels_jul32_2w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$goebbels_jul32_2w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$goebbels_jul32_2w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$goebbels_jul32_2w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$goebbels_jul32_2w_50km_nomatch <- mindist > 50 & mindist < 60
mindist <- rdist.earth(coordinates(cbind(goebbels_df_jul32_4w$lon, goebbels_df_jul32_4w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$goebbels_jul32_4w_5km <- mindist < 5
elections_geo_spdf$goebbels_jul32_4w_10km <- mindist < 10
elections_geo_spdf$goebbels_jul32_4w_25km <- mindist < 25
elections_geo_spdf$goebbels_jul32_4w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$goebbels_jul32_4w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$goebbels_jul32_4w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$goebbels_jul32_4w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$goebbels_jul32_4w_50km_nomatch <- mindist > 50 & mindist < 60
mindist <- rdist.earth(coordinates(cbind(goebbels_df_jul32_8w$lon, goebbels_df_jul32_8w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$goebbels_jul32_8w_5km <- mindist < 5
elections_geo_spdf$goebbels_jul32_8w_10km <- mindist < 10
elections_geo_spdf$goebbels_jul32_8w_25km <- mindist < 25
elections_geo_spdf$goebbels_jul32_8w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$goebbels_jul32_8w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$goebbels_jul32_8w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$goebbels_jul32_8w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$goebbels_jul32_8w_50km_nomatch <- mindist > 50 & mindist < 60
mindist <- rdist.earth(coordinates(cbind(goebbels_df_jul32_12w$lon, goebbels_df_jul32_12w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$goebbels_jul32_12w_5km <- mindist < 5
elections_geo_spdf$goebbels_jul32_12w_10km <- mindist < 10
elections_geo_spdf$goebbels_jul32_12w_25km <- mindist < 25
elections_geo_spdf$goebbels_jul32_12w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$goebbels_jul32_12w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$goebbels_jul32_12w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$goebbels_jul32_12w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$goebbels_jul32_12w_50km_nomatch <- mindist > 50 & mindist < 60

# 1932/2, restricted periods
mindist <- rdist.earth(coordinates(cbind(goebbels_df_nov32_2w$lon, goebbels_df_nov32_2w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$goebbels_nov32_2w_5km <- mindist < 5
elections_geo_spdf$goebbels_nov32_2w_10km <- mindist < 10
elections_geo_spdf$goebbels_nov32_2w_25km <- mindist < 25
elections_geo_spdf$goebbels_nov32_2w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$goebbels_nov32_2w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$goebbels_nov32_2w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$goebbels_nov32_2w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$goebbels_nov32_2w_50km_nomatch <- mindist > 50 & mindist < 60
mindist <- rdist.earth(coordinates(cbind(goebbels_df_nov32_4w$lon, goebbels_df_nov32_4w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$goebbels_nov32_4w_5km <- mindist < 5
elections_geo_spdf$goebbels_nov32_4w_10km <- mindist < 10
elections_geo_spdf$goebbels_nov32_4w_25km <- mindist < 25
elections_geo_spdf$goebbels_nov32_4w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$goebbels_nov32_4w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$goebbels_nov32_4w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$goebbels_nov32_4w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$goebbels_nov32_4w_50km_nomatch <- mindist > 50 & mindist < 60
mindist <- rdist.earth(coordinates(cbind(goebbels_df_nov32_8w$lon, goebbels_df_nov32_8w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$goebbels_nov32_8w_5km <- mindist < 5
elections_geo_spdf$goebbels_nov32_8w_10km <- mindist < 10
elections_geo_spdf$goebbels_nov32_8w_25km <- mindist < 25
elections_geo_spdf$goebbels_nov32_8w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$goebbels_nov32_8w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$goebbels_nov32_8w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$goebbels_nov32_8w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$goebbels_nov32_8w_50km_nomatch <- mindist > 50 & mindist < 60
mindist <- rdist.earth(coordinates(cbind(goebbels_df_nov32_12w$lon, goebbels_df_nov32_12w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$goebbels_nov32_12w_5km <- mindist < 5
elections_geo_spdf$goebbels_nov32_12w_10km <- mindist < 10
elections_geo_spdf$goebbels_nov32_12w_25km <- mindist < 25
elections_geo_spdf$goebbels_nov32_12w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$goebbels_nov32_12w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$goebbels_nov32_12w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$goebbels_nov32_12w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$goebbels_nov32_12w_50km_nomatch <- mindist > 50 & mindist < 60

# 1933, restricted periods
mindist <- rdist.earth(coordinates(cbind(goebbels_df_mar33_2w$lon, goebbels_df_mar33_2w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$goebbels_mar33_2w_5km <- mindist < 5
elections_geo_spdf$goebbels_mar33_2w_10km <- mindist < 10
elections_geo_spdf$goebbels_mar33_2w_25km <- mindist < 25
elections_geo_spdf$goebbels_mar33_2w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$goebbels_mar33_2w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$goebbels_mar33_2w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$goebbels_mar33_2w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$goebbels_mar33_2w_50km_nomatch <- mindist > 50 & mindist < 60
mindist <- rdist.earth(coordinates(cbind(goebbels_df_mar33_4w$lon, goebbels_df_mar33_4w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$goebbels_mar33_4w_5km <- mindist < 5
elections_geo_spdf$goebbels_mar33_4w_10km <- mindist < 10
elections_geo_spdf$goebbels_mar33_4w_25km <- mindist < 25
elections_geo_spdf$goebbels_mar33_4w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$goebbels_mar33_4w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$goebbels_mar33_4w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$goebbels_mar33_4w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$goebbels_mar33_4w_50km_nomatch <- mindist > 50 & mindist < 60
mindist <- rdist.earth(coordinates(cbind(goebbels_df_mar33_8w$lon, goebbels_df_mar33_8w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$goebbels_mar33_8w_5km <- mindist < 5
elections_geo_spdf$goebbels_mar33_8w_10km <- mindist < 10
elections_geo_spdf$goebbels_mar33_8w_25km <- mindist < 25
elections_geo_spdf$goebbels_mar33_8w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$goebbels_mar33_8w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$goebbels_mar33_8w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$goebbels_mar33_8w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$goebbels_mar33_8w_50km_nomatch <- mindist > 50 & mindist < 60
mindist <- rdist.earth(coordinates(cbind(goebbels_df_mar33_12w$lon, goebbels_df_mar33_12w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$goebbels_mar33_12w_5km <- mindist < 5
elections_geo_spdf$goebbels_mar33_12w_10km <- mindist < 10
elections_geo_spdf$goebbels_mar33_12w_25km <- mindist < 25
elections_geo_spdf$goebbels_mar33_12w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$goebbels_mar33_12w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$goebbels_mar33_12w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$goebbels_mar33_12w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$goebbels_mar33_12w_50km_nomatch <- mindist > 50 & mindist < 60

# 1932/p1, restricted periods
mindist <- rdist.earth(coordinates(cbind(goebbels_df_mar32_2w$lon, goebbels_df_mar32_2w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$goebbels_mar32_2w_5km <- mindist < 5
elections_geo_spdf$goebbels_mar32_2w_10km <- mindist < 10
elections_geo_spdf$goebbels_mar32_2w_25km <- mindist < 25
elections_geo_spdf$goebbels_mar32_2w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$goebbels_mar32_2w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$goebbels_mar32_2w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$goebbels_mar32_2w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$goebbels_mar32_2w_50km_nomatch <- mindist > 50 & mindist < 60
mindist <- rdist.earth(coordinates(cbind(goebbels_df_mar32_4w$lon, goebbels_df_mar32_4w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$goebbels_mar32_4w_5km <- mindist < 5
elections_geo_spdf$goebbels_mar32_4w_10km <- mindist < 10
elections_geo_spdf$goebbels_mar32_4w_25km <- mindist < 25
elections_geo_spdf$goebbels_mar32_4w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$goebbels_mar32_4w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$goebbels_mar32_4w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$goebbels_mar32_4w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$goebbels_mar32_4w_50km_nomatch <- mindist > 50 & mindist < 60
mindist <- rdist.earth(coordinates(cbind(goebbels_df_mar32_8w$lon, goebbels_df_mar32_8w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$goebbels_mar32_8w_5km <- mindist < 5
elections_geo_spdf$goebbels_mar32_8w_10km <- mindist < 10
elections_geo_spdf$goebbels_mar32_8w_25km <- mindist < 25
elections_geo_spdf$goebbels_mar32_8w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$goebbels_mar32_8w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$goebbels_mar32_8w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$goebbels_mar32_8w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$goebbels_mar32_8w_50km_nomatch <- mindist > 50 & mindist < 60
mindist <- rdist.earth(coordinates(cbind(goebbels_df_mar32_12w$lon, goebbels_df_mar32_12w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$goebbels_mar32_12w_5km <- mindist < 5
elections_geo_spdf$goebbels_mar32_12w_10km <- mindist < 10
elections_geo_spdf$goebbels_mar32_12w_25km <- mindist < 25
elections_geo_spdf$goebbels_mar32_12w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$goebbels_mar32_12w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$goebbels_mar32_12w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$goebbels_mar32_12w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$goebbels_mar32_12w_50km_nomatch <- mindist > 50 & mindist < 60

# 1932/p2, restricted periods
mindist <- rdist.earth(coordinates(cbind(goebbels_df_apr32_2w$lon, goebbels_df_apr32_2w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$goebbels_apr32_2w_5km <- mindist < 5
elections_geo_spdf$goebbels_apr32_2w_10km <- mindist < 10
elections_geo_spdf$goebbels_apr32_2w_25km <- mindist < 25
elections_geo_spdf$goebbels_apr32_2w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$goebbels_apr32_2w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$goebbels_apr32_2w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$goebbels_apr32_2w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$goebbels_apr32_2w_50km_nomatch <- mindist > 50 & mindist < 60
mindist <- rdist.earth(coordinates(cbind(goebbels_df_apr32_4w$lon, goebbels_df_apr32_4w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$goebbels_apr32_4w_5km <- mindist < 5
elections_geo_spdf$goebbels_apr32_4w_10km <- mindist < 10
elections_geo_spdf$goebbels_apr32_4w_25km <- mindist < 25
elections_geo_spdf$goebbels_apr32_4w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$goebbels_apr32_4w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$goebbels_apr32_4w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$goebbels_apr32_4w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$goebbels_apr32_4w_50km_nomatch <- mindist > 50 & mindist < 60
mindist <- rdist.earth(coordinates(cbind(goebbels_df_apr32_8w$lon, goebbels_df_apr32_8w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$goebbels_apr32_8w_5km <- mindist < 5
elections_geo_spdf$goebbels_apr32_8w_10km <- mindist < 10
elections_geo_spdf$goebbels_apr32_8w_25km <- mindist < 25
elections_geo_spdf$goebbels_apr32_8w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$goebbels_apr32_8w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$goebbels_apr32_8w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$goebbels_apr32_8w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$goebbels_apr32_8w_50km_nomatch <- mindist > 50 & mindist < 60
mindist <- rdist.earth(coordinates(cbind(goebbels_df_apr32_12w$lon, goebbels_df_apr32_12w$lat)), coordinates(elections_sp), miles = FALSE, R = 6378) %>% apply(2, min)
elections_geo_spdf$goebbels_apr32_12w_5km <- mindist < 5
elections_geo_spdf$goebbels_apr32_12w_10km <- mindist < 10
elections_geo_spdf$goebbels_apr32_12w_25km <- mindist < 25
elections_geo_spdf$goebbels_apr32_12w_50km <- mindist < 50
# add no-matching zones, 10km width
elections_geo_spdf$goebbels_apr32_12w_5km_nomatch <- mindist > 5 & mindist < 15
elections_geo_spdf$goebbels_apr32_12w_10km_nomatch <- mindist > 10 & mindist < 20
elections_geo_spdf$goebbels_apr32_12w_25km_nomatch <- mindist > 25 & mindist < 35
elections_geo_spdf$goebbels_apr32_12w_50km_nomatch <- mindist > 50 & mindist < 60

# check if generated variables are plausible
plot(elections_geo_spdf, cex = .3, col = "grey")
points(elections_geo_spdf[elections_geo_spdf$goebbels_may28_10km==T,], pch = 20, col = "red", cex = 1)
points(goebbels_df_may28$lon, goebbels_df_may28$lat, pch = 20, col = "green")
points(elections_geo_spdf[elections_geo_spdf$goebbels_may28_10km_nomatch==T,], pch = 20, col = "blue")





## import NSDAP membership data -------------------
load("members_df_wkr_wide.RData")

# merge with elections_geo_spdf
elections_geo_spdf <- merge(elections_geo_spdf, members_df_wkr_wide, by = "krnr", all.x = TRUE)



## export data frame ------------------------------
elections_geo_df <- as.data.frame(elections_geo_spdf)

# RData export
save(elections_geo_df, file = "elections_geopoints_df.RData")
save(elections_geo_spdf, file = "elections_geopoints_spdf.RData")


# Stata export
write.dta(elections_geo_df, file = "elections_geopoints_df.dta", version = 10)
