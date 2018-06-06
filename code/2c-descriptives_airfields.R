#----------------------------------------------------------------
# Examining a Most Likely Case for Strong Campaign Effects: 
# Hitler's Speeches and the Rise of the Nazi Party, 1927--1933
# Peter Selb (University of Konstanz)
# Simon Munzert (MZES, University of Mannheim)
# 2016
#----------------------------------------------------------------

## imports
c("airfields_certainly_before_32_df.RData",
  "nazi_wgs84.RData",
  "speeches_df.RData",
  "elections_geo_df.RData"
  )

## exports
c("../figures/tab-airfields.tex",
  "../figures/map_airfields.pdf")


## load packages and functions -------------------
source("packages.r")
source("functions.r")

load("airfields_certainly_before_32_df.RData")

airfields_df$names[47] <- "Erfurt (Am Roten Berg)"
airfields_df$names[49] <- "Flugplatz Fürstenwalde"
airfields_df$names[51] <- "Köln-Butzweilerhof"
airfields_df$names[54] <- "München-Oberwiesenfeld"
airfields_df$names[55] <- "Stuttgart-Böblingen"
airfields_df$names[56] <- "Devau-Königsberg"
airfields_df$names[58] <- "Flughafen Giessen"
airfields_df$names[61] <- "Luftverkehrshafen Fürth"
airfields_df$names[69] <- "Schwerin-Görries"


## table: airfields overview ---------------------

airfields_latex <- select(airfields_df, names, date, latitude, longitude)
airfields_latex$names <- char(airfields_latex$names)
airfields_latex$names <- airfields_latex$names %>% str_replace("Flughafen |Flugplatz ", "") %>% str_replace(fixed("(Intl.)"), "")
airfields_latex$date <- char(airfields_latex$date)
airfields_latex <- dplyr::arrange(airfields_latex, names)

names(airfields_latex) <- c("Airfield", "Constructed in", "Latitude", "Longitude")
rws <- seq(10, nrow(airfields_latex), 10)

# addtorow <- list()
# addtorow$pos <- list(seq(0, nrow(airfields_latex), 50)[-1])
# addtorow$command <- c("\\bottomrule \n \\end{tabular}\n } \n \\end{table} \n
# \\begin{table}[h!] \n \\centering \n \\caption{Collected airfields in operation 1932, \\textit{continued.}}  \n {\\scriptsize \n \\begin{tabular}{p{2.8cm}rrrp{6cm}} \n \\toprule \n Airfield & Year of construction & Latitude & Longitude   \\\\ \n \\midrule \n")

#print(xtable(airfields_latex, align = c("l","p{2.8cm}","r","r", "r"), digits=4, caption = "Collected airfields in operation 1932.\\label{tab:airfields}"), booktabs = TRUE, size = "scriptsize", caption.placement = "top", table.placement = "h!", hline.after = c(-1, 0, rws, nrow(airfields_latex)),  include.rownames=FALSE,  file = "../figures/tab-airfields.tex")



## plot: map of airfield locations -------------------------

# load map data
load("nazi_wgs84.RData")

# make airfields data SpatialPoints object
airfields_sp <- SpatialPoints(cbind(airfields_df$longitude, airfields_df$latitude))
projection <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4string(airfields_sp) <- CRS(projection)

# load speeches data
load("speeches_df.RData")

# load geo dataframe
load("elections_geo_df.RData")

# load images
swastika <- readPNG('icons/swastika.png')
airport <- readPNG('icons/airport.png')



### plot location of airfields and distance measure -----------
pdf(file="../figures/map_airfields.pdf", height=7.5, width=10, family="URWTimes")
par(oma=c(0,0,0,0))
par(mar=c(1.5,2,1.5,.5))
par(xaxs = "i", yaxs = "i")
# plot boundaries
# plot values of continuous variable
nazi_wgs84$airfields_dist_min_cat <- findInterval(nazi_wgs84$airfields_dist_min, seq(5, 150, 20))
cats <- length(unique(nazi_wgs84$airfields_dist_min_cat))
cols <- brewer.pal(n = cats, name = "YlOrRd")

col_vec <- vector() # set colors
for (i in 1:nrow(nazi_wgs84)) {
  col_vec[i] <- cols[nazi_wgs84$airfields_dist_min_cat[i]+1]
}
plot(nazi_wgs84, col = col_vec, border = "darkgrey")
# plot airfields
offset <- .15
dat <- airfields_sp
rasterImage(airport, coordinates(dat)[,1]-offset, coordinates(dat)[,2]-offset+.05, coordinates(dat)[,1]+offset, coordinates(dat)[,2]+offset-.05)
# add legend

colorbar.plot(x = 20, y = 47.5, strip.width = .1, seq_along(cols), col = cols,  adj.y = 0.8)
text(x = 19.2, y = 47.8, "Distance to closest airfield")
mtext("0km", side = 1, outer = FALSE, at = 17.7, cex = .8)
mtext("75km", side = 1, outer = FALSE, at = 20, cex = .8)
mtext(">150km", side = 1, outer = FALSE, at = 22.1, cex = .8)
dev.off()

