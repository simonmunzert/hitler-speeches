#----------------------------------------------------------------
# Examining a Most Likely Case for Strong Campaign Effects: 
# Hitler's Speeches and the Rise of the Nazi Party, 1927--1933
# Peter Selb (University of Konstanz)
# Simon Munzert (MZES, University of Mannheim)
# 2016
#----------------------------------------------------------------

## imports
c("data_airfields/airfields_pb32_df_manual.csv")

## exports
c("airfields_scraped_df.RData",
  "data_airfields/airfields_pb32_df.csv",
  "airfields_certainly_before_32_df.RData")


## load packages and functions -------------------
source("packages.r")
source("functions.r")


## scrape airfield index lists from Wikipedia -----------

# store urls
url_airf <- "https://de.wikipedia.org/wiki/Liste_der_Verkehrslandepl%C3%A4tze_mit_IFR-Zulassung"
url_airf2 <- "https://de.wikipedia.org/wiki/Liste_der_Verkehrs-_und_Sonderlandepl%C3%A4tze_in_Deutschland"
url_base <- "https://de.wikipedia.org"

# download index pages
dir.create("data_airfields")
filepath_url_airf <- paste0("data_airfields/", basename(url_airf), ".html")
filepath_url_airf2 <- paste0("data_airfields/", basename(url_airf2), ".html")
download.file(url_airf, destfile = filepath_url_airf)
download.file(url_airf2, destfile = filepath_url_airf2)

# parse index urls 
url_airf_parsed <- read_html(filepath_url_airf, encoding = "utf8")
url_airf2_parsed <- read_html(filepath_url_airf2, encoding = "utf-8")

# url_airf: import names, links, and coordinates
airf_names <- html_nodes(url_airf_parsed, "td:nth-child(1) a") %>% html_text()
airf_links <- html_nodes(url_airf_parsed, "td:nth-child(1) a") %>% html_attr("href")
airf_coord <- rep(NA,length(airf_names)-1)
airf_runway <- rep(NA,length(airf_names)-1)
airf_runwaytype <- rep(NA,length(airf_names)-1)


# delete last line
airf_names <- airf_names[1:length(airf_names)-1]
airf_links <- airf_links[1:length(airf_links)-1]



# url_airf2: import names, links, and coordinates
airf_names <- c(airf_names, html_nodes(url_airf2_parsed, "td:nth-child(1) a") %>% html_text())
airf_links <- c(airf_links, html_nodes(url_airf2_parsed, "td:nth-child(1) a") %>% html_attr("href"))
airf_coord <- c(airf_coord, as.vector(readHTMLTable(url_airf2_parsed, stringsAsFactors = FALSE)[[2]][,10]))

html_nodes(url_airf2_parsed, "td:nth-child(10) a") %>% html_text()
  %>% str_extract("[[:digit:]]+×[[:digit:]]+")

airf_runway <- c(airf_runway, as.vector(readHTMLTable(url_airf2_parsed, stringsAsFactors = FALSE)[[2]][,7])) %>% str_extract("[[:digit:]]+×[[:digit:]]+")
airf_runwaytype <- c(airf_runwaytype, as.vector(readHTMLTable(url_airf2_parsed, stringsAsFactors = FALSE)[[2]][,9])) %>% str_extract("[[:upper:]]")

# sort vectors by airfield names
airf_order <- order(airf_names)
airf_names <- airf_names[airf_order]
airf_links <- airf_links[airf_order]
airf_coord <- airf_coord[airf_order]
airf_runway <- airf_runway[airf_order]
airf_runwaytype <- airf_runwaytype[airf_order]



## scrape airfield pages from Wikipedia -----------

# download available pages
airfield_keywords <- "[Aa]irport|[Ff]lug|[Ll]andeplatz"
airf_links_full <- paste0(url_base, airf_links[!str_detect(airf_links, "redlink") & str_detect(airf_links, airfield_keywords)])

for (i in seq_along(airf_links_full)) {
  if (!file.exists(paste0("data_airfields/", basename(airf_links_full[i]), ".html"))) {
  download.file(airf_links_full[i], destfile = paste0("data_airfields/", basename(airf_links_full[i]), ".html"))
  Sys.sleep(.2)
  }
}

# scrape pages, identify dates, coordinates, and comment
airf_date <- vector()
airf_latitude <- vector()
airf_longitude <- vector()
airf_com <- vector()

for(i in 1:length(airf_names)){
  
	# for redlinks: different method for coordinates
  	if(substr(airf_links[i],1,3)=="/w/") {
    		airf_latitude[i] <- as.numeric(as.character(substr(airf_coord[i],3,13)))
    		airf_longitude[i] <- as.numeric(as.character(substr(airf_coord[i],14,25)))  
    		airf_date[i] <- NA
		airf_com[i] <- "redlink" }
  
  	# for bluelinks
  	else{
    		flugplatz <- htmlParse(paste(url_base,airf_links[i],sep=""), encoding = "UTF-8")
    		flugplatz.table <- readHTMLTable(flugplatz, trim=TRUE)
  
    		if("Eröffnung" %in% flugplatz.table$"NULL"$V1) {
      			text <- as.vector(flugplatz.table$"NULL"$V2)[as.vector(flugplatz.table$"NULL"$V1)=="Eröffnung"] 

			years<-as.numeric(str_extract_all(text, "19[0-9][0-9]")[[1]])

      			if(length(years)!=0){
      				airf_date[i]<-min(as.numeric(years))
				airf_com[i]<-paste("found in table (", text, ")", sep="") }
      			else{
				airf_date[i]<-NA
				airf_com[i]<-paste("not found in table (", text, ")", sep="") } }
		else {
      			text<-paste(xpathSApply(flugplatz, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue), collapse=" ")
      
      			years<-as.numeric(str_extract_all(text, "19[0-9][0-9]")[[1]])	
      
      			if(length(years)!=0){
      				airf_date[i]<-min(as.numeric(years))
				airf_com[i]<-"smallest 19**-number found in article" }
      			else{
				airf_date[i]<-NA
				airf_com[i]<-"not found in article" } }
  
    		reg.lat <- "//span[@class='latitude']"
    		reg.long <- "//span[@class='longitude']"
    		airf_latitude[i] <- as.numeric(as.character(xpathSApply(flugplatz,reg.lat, xmlValue)[1]))
    		airf_longitude[i] <- as.numeric(as.character(xpathSApply(flugplatz,reg.long, xmlValue)[1]))
    	
if (is.na(airf_runway[i])) {
 text <- as.vector(flugplatz.table$"NULL"$V2)[seq_along(as.vector(flugplatz.table$"NULL"$V1))[str_detect(as.vector(flugplatz.table$"NULL"$V1), "Start- und Landebahn")] + 1]
 airf_runway[i] <- text %>% str_extract(".+m ")
 airf_runwaytype[i] <- text %>% str_extract("[[:upper:]].+$")
}	
} 
}

# generate data frame
airfields_df <- data.frame(airf_names, airf_links, airf_date, airf_latitude, airf_longitude, airf_com, airf_runway, airf_runwaytype)

# export airfield data frame
save(airfields_df, file = "airfields_scraped_df.RData")


## assemble collected data -----------------------

# load scraped data
load("airfields_scraped_df.RData")

# tidy runway variables
airfields_df$airf_runwaytype <- airfields_df$airf_runwaytype %>% str_replace("Asph.+", "A") %>% str_replace("Beton", "B") %>% str_replace("Gras", "G")
airfields_df$airf_runway <- airfields_df$airf_runway %>% str_replace_all("[m ]", "") %>% str_trim()
airfields_df$airf_runway_length <- airfields_df$airf_runway %>% str_extract("^[[:digit:]]+") %>% as.numeric()
airfields_df$airf_runway_width <- airfields_df$airf_runway %>% str_extract("[[:digit:]]+$") %>% as.numeric()
airfields_df$airf_runway_width <- ifelse(airfields_df$airf_runway_width >= 70, airfields_df$airf_runway_width/10, ifelse(airfields_df$airf_runway_width > 600, airfields_df$airf_runway_width/100, airfields_df$airf_runway_width))


# filter airports - certain or uncertain existence before 1932
airfields_cb32_df <- dplyr::filter(airfields_df, airf_date < 1932, substr(char(airf_com),1,1) == "f")
airfields_pb32_df <- dplyr::filter(airfields_df, airf_date < 1932, substr(char(airf_com),1,1) != "f")

# export list of probable airfields for manual research
write.table(airfields_pb32_df, dec = ".", sep = ";", file = "data_airfields/airfields_pb32_df.csv")

# remove duplicates
airfields_cb32_df <- airfields_cb32_df[!duplicated(cbind(airfields_cb32_df$airf_latitude, airfields_cb32_df$airf_longitude)),]

# add results from manual research
airfields_pb32_df_manual <- read.csv2("data_airfields/airfields_pb32_df_manual.csv")
airfields_pb32_df_yes <- airfields_pb32_df[airfields_pb32_df_manual$manual_research=="YES",]

airfields_manual <- read.csv2("data_airfields/airfields_manual_research.csv")

airfields_df <- rbind(airfields_cb32_df, airfields_pb32_df_yes, airfields_manual)
names(airfields_df) <- airfields_df %>% names() %>% str_replace(fixed(".limit."), "") %>% str_replace(fixed("airf_"), "")
airfields_df$latitude <- num(char(airfields_df$latitude))
airfields_df$longitude <- num(char(airfields_df$longitude))

# delete implausible airports (Islands etc.)
airfields_df <- airfields_df[!airfields_df$names== "Flughafen Saarbrücken (Intl.)",]
airfields_df <- airfields_df[!airfields_df$names== "Flughafen Sylt",]
airfields_df <- airfields_df[!airfields_df$names== "Langeoog",]
airfields_df <- airfields_df[!airfields_df$names== "Wyk auf Föhr",]
airfields_df <- airfields_df[!airfields_df$names== "Borkum",]
airfields_df <- airfields_df[!airfields_df$names== "Wangerooge",]
airfields_df <- airfields_df[!is.na(airfields_df$names),]

## export airfield data frame -------------------
save(airfields_df, file = "airfields_certainly_before_32_df.RData")


