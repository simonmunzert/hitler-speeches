# load packages

# install packages from CRAN
p_needed <- c(
  "grImport",
  "sp",
  "spdep",
  "magrittr",
  "lubridate",
  "MatchIt", # has to be loaded before dplyr because of select(", command in MASS package
  "plyr",
  "dplyr",
  "rgeos",
  "rgdal",
  "gpclib",
  "maptools",
  "ggmap",
  "RColorBrewer",
  "XML",
  "stringr",
  "rvest",
  "foreign",
  "spatstat",
  "FNN",
  "fields",
  "png",
  "xtable",
  #"XLConnect", error with XLConnectJars
  "zoo",
  "mi",
  "gdata",
  "qlcMatrix",
  "car",
  "broom",
  "coefplot",
  "coefplot2",
  "stargazer",
  "Zelig",
  "clusterSEs",
  "AER",
  "rowr",
  "pdftools",
  "ivpack",
  "haven",
  "coefplot2"
)
packages <- rownames(installed.packages())
p_to_install <- p_needed[!(p_needed %in% packages)]
if (length(p_to_install) > 0) {
  install.packages(p_to_install)
}
lapply(p_needed, require, character.only = TRUE)

#install.packages("coefplot2",repos="http://www.math.mcmaster.ca/bolker/R", type="source")
