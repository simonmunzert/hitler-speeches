#----------------------------------------------------------------
# Examining a Most Likely Case for Strong Campaign Effects: 
# Hitler's Speeches and the Rise of the Nazi Party, 1927--1933
# Peter Selb (University of Konstanz)
# Simon Munzert (MZES, University of Mannheim)
# 2016
#----------------------------------------------------------------


## imports
c("data_geopoints/geopoints_df_outliers.csv")

## exports
c("data_geopoints/geopoints_df_cleaned.csv")



## load packages and functions -------------------------------
source("packages.r")
source("functions.r")


# import district/municipality geo-points data
geopoints_df <- read.csv2("data_geopoints/geopoints_df_outliers.csv", header = TRUE, encoding = "UTF-8", stringsAsFactors = FALSE)
geopoints_df$long <- geopoints_df$long %>% char() %>% num()
geopoints_df$lat <- geopoints_df$lat %>% char() %>% num()
geopoints_df$lat_median <- geopoints_df$lat_median %>% char() %>% num()
geopoints_df$long_median <- geopoints_df$long_median %>% char() %>% num()
names(geopoints_df)


# identify long/lat outlier
geopoints_df_group <- group_by(geopoints_df, krnr)
geopoints_df_sum <- summarise(geopoints_df_group, lat_median = median(lat), long_median = median(long))
geopoints_df$lat_median <- NULL
geopoints_df$long_median <- NULL
geopoints_df <- merge(geopoints_df, geopoints_df_sum, by = "krnr", all.x = TRUE)
geopoints_df$lat_outlier <- NULL
geopoints_df$long_outlier <- NULL
geopoints_df$lat_outlier <- ifelse(abs(geopoints_df$lat - geopoints_df$lat_median > .7), TRUE, FALSE)
geopoints_df$long_outlier <- ifelse(abs(geopoints_df$long - geopoints_df$long_median > .7), TRUE, FALSE)
table(geopoints_df$lat_outlier)
table(geopoints_df$long_outlier)


# assign median longitudes/latitudes to RESTKREISE units
table(geopoints_df$agglvl)
 geopoints_df$long[geopoints_df$agglvl == "RESTKREISE (GEM.< 20"] <- geopoints_df$long_median[geopoints_df$agglvl == "RESTKREISE (GEM.< 20"]
geopoints_df$lat[geopoints_df$agglvl == "RESTKREISE (GEM.< 20"] <- geopoints_df$lat_median[geopoints_df$agglvl == "RESTKREISE (GEM.< 20"]


# plot data
plot(geopoints_df$long, geopoints_df$lat, cex = .5) 


# export data
geopoints_df <- dplyr::select(geopoints_df, krnr, lfnr, agglvl, name, name_neu, lat, long, lat_median, long_median)
write.table(geopoints_df, file = "data_geopoints/geopoints_df_cleaned.csv", quote = FALSE, dec = ".", sep = ";", row.names = FALSE)





