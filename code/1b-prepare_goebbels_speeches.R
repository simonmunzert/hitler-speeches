#----------------------------------------------------------------
# Examining a Most Likely Case for Strong Campaign Effects: 
# Hitler's Speeches and the Rise of the Nazi Party, 1927--1933
# Peter Selb (University of Konstanz)
# Simon Munzert (MZES, University of Mannheim)
# 2016
#----------------------------------------------------------------


## imports
c("data_speeches/GoebbelsTagebuecher.pdf")

## exports
c("goebbels_df.RData",
  "./data_speeches/coordinatesGoogleAPIGoebbelsSpeeches.RData")



## load packages and functions -------------------------------
source("packages.r")
source("functions.r")

# import pdf
goebbels_txt_raw <- pdf_text("data_speeches/GoebbelsTagebuecher.pdf")

# select time span between April 14 1928 and March 7 1933
goebbels_txt <- goebbels_txt_raw[288:782] # gap between October 31 1926 and April 14 1928 (see footnote on page 282)

# delete headers
goebbels_txt <- goebbels_txt %>% str_replace("^.+?\\n", "")

# replace dates with dates + split identifier
goebbels_txt <- str_c("\n", goebbels_txt)
goebbels_txt <- goebbels_txt %>% str_replace_all("(\\n[[:digit:]]{1,2}.+?[[:digit:] ]{3,5}( \\(Kaiser.+\\))?\\n)", "SPLITTER\\1")
goebbels_txt[301] <- str_replace(goebbels_txt[301], "SPLITTER\\n40", " 40") # wrong split identifier removed

# collapse all into one vector, then split by dates
goebbels_txt_collapsed <- paste(goebbels_txt, collapse = " ")
goebbels_txt <- str_split(goebbels_txt_collapsed, "SPLITTER")  %>% unlist()
goebbels_txt <- goebbels_txt[-c(216:229)] # no diary entries but tables of content again

# delete invalid lines
goebbels_txt <- goebbels_txt[-1]
goebbels_txt[387] <- paste(goebbels_txt[387], goebbels_txt[388], collapse = " ")
goebbels_txt <- goebbels_txt[-388]

# extract dates
dates_vec <- str_extract(goebbels_txt, "[[:digit:]]{1,2}.+?[[:digit:]]{3,4}")
dates_vec <- str_replace_all(dates_vec, "l(?=9)", "1")
dates_vec <- str_replace_all(dates_vec, "IJezernber", "Dezember")
dates_vec <- str_replace_all(dates_vec, "\\(.+| (?=[[:digit:]])| ?\\. | (?=ugust)|428 +(?=[[:digit:]])", "")
day <- str_extract(dates_vec, "^[[:digit:]]{1,2}")
month <- str_extract(dates_vec, "[[:alpha:]]+") 
month <- recode(month, "'Januar'=1;'Februar'=2;'März'=3;'April'=4;'Mai'=5;'Juni'=6;'Juli'=7;'August'=8;'September'=9;'Oktober'=10;'November'=11;'Dezember'=12")
year <- str_extract(dates_vec, "[[:digit:]]{4}$")
year <- str_replace(year, "79", "19")
dates_vec <- ymd(paste(year, month, day, sep = "-"))

# format txts
goebbels_txt <- goebbels_txt %>% str_replace_all("\\n", " ")
goebbels_txt <- str_replace_all(goebbels_txt, "[[:digit:]]|,|:|;|\\(|\\)|»|«|\\[|\\]|/|\\\\", "")
goebbels_txt <- str_replace_all(goebbels_txt, "\\!|\\?", "\\.")
goebbels_txt <- tolower(goebbels_txt)

# detect keywords for speech held
regex <- "sprech|gesprochen|rede|kundgebung|ansprache|veranstaltung|vortrag"

# extract entries where keywords appear
speech_flag <- grepl(regex, goebbels_txt, ignore.case = TRUE)
goebbels_speeches <- goebbels_txt[speech_flag]

# extract dates with keyword appearance
dates_vec <- dates_vec[speech_flag]

# extract sections featuring keywords + neighboring sentences
str_extract_all(goebbels_speeches[1:50], "(.){0,90}(sprech|gesprochen|rede|kundgebung|ansprache|veranstaltung|vortrag)(.){0,90}")
str_extract_all(goebbels_speeches[51:100], "(.){0,90}(sprech|gesprochen|rede|kundgebung|ansprache|veranstaltung|vortrag)(.){0,90}")
str_extract_all(goebbels_speeches[101:150], "(.){0,90}(sprech|gesprochen|rede|kundgebung|ansprache|veranstaltung|vortrag)(.){0,90}")
str_extract_all(goebbels_speeches[151:200], "(.){0,90}(sprech|gesprochen|rede|kundgebung|ansprache|veranstaltung|vortrag)(.){0,90}")
str_extract_all(goebbels_speeches[201:250], "(.){0,90}(sprech|gesprochen|rede|kundgebung|ansprache|veranstaltung|vortrag)(.){0,90}")
str_extract_all(goebbels_speeches[251:300], "(.){0,90}(sprech|gesprochen|rede|kundgebung|ansprache|veranstaltung|vortrag)(.){0,90}")
str_extract_all(goebbels_speeches[301:350], "(.){0,90}(sprech|gesprochen|rede|kundgebung|ansprache|veranstaltung|vortrag)(.){0,90}")
str_extract_all(goebbels_speeches[351:421], "(.){0,90}(sprech|gesprochen|rede|kundgebung|ansprache|veranstaltung|vortrag)(.){0,90}")

## coding of speeches based on extracted sections
# speech yes/no
speech_50 <- c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE)
speech_100 <- c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE)
speech_150 <- c(TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
speech_200 <- c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)
speech_250 <- c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
speech_300 <- c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
speech_350 <- c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
speech_421 <- c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE)

# Goebbels speech yes/no
speech_goebbels_50 <- c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE)
speech_goebbels_100 <- c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE)
speech_goebbels_150 <- c(TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
speech_goebbels_200 <- c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)
speech_goebbels_250 <- c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
speech_goebbels_300 <- c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
speech_goebbels_350 <- c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
speech_goebbels_421 <- c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE)

# Hitler speech yes/no
speech_hitler_50 <- c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
speech_hitler_100 <- c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
speech_hitler_150 <- c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
speech_hitler_200 <- c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
speech_hitler_250 <- c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
speech_hitler_300 <- c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
speech_hitler_350 <- c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
speech_hitler_421 <- c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)

# other speech yes/no
speech_other_50 <- c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
speech_other_100 <- c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
speech_other_150 <- c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
speech_other_200 <- c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
speech_other_250 <- c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
speech_other_300 <- c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
speech_other_350 <- c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
speech_other_421 <- c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)

# if other speech == TRUE, who?
speech_other_who_50 <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Wagner", "Wagner", NA, NA, NA, NA, NA, NA, NA, NA, NA)
speech_other_who_100 <- c(NA, NA, NA, NA, NA, NA, NA, "Engel", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Wagner", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Göring", NA, NA, NA, NA, "Strasser", NA, NA, "Epp", NA, NA, NA, NA, NA, NA, NA, NA)
speech_other_who_150 <- c("Streicher", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Göring", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
speech_other_who_200 <- c(NA, "Feder", NA, NA, NA, NA, NA, NA, NA, NA, "Schlemm", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Göring", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Studentkowski, Wagner, Göring", NA, NA, NA, NA, NA, NA, NA, NA)
speech_other_who_250 <- c(NA, NA, NA, NA, NA, NA, "Göring, Strasser", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Frick", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Göring", NA, NA, NA, "Göring", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
speech_other_who_300 <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
speech_other_who_350 <- c(NA, NA, NA, NA, NA, NA, NA, NA, "Frick", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
speech_other_who_421 <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)

# public speech (not private speech in front of, e.g., NSDAP elites) yes/no
public_50 <- c(FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE)
public_100 <- c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE)
public_150 <- c(TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
public_200 <- c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)
public_250 <- c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
public_300 <- c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
public_350 <- c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
public_421 <- c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)

# location of appearance, i.e. town or city
location_50 <- c(NA, NA, "Berlin-Friedenau", NA, "Berlin-Friedrichshain", NA, "Berlin-Steglitz", "Berlin-Alexanderplat", "Berlin-Moabit", NA, "München", "Berlin-Steglitz, Berlin-Spandau", "Berlin-Neukölln", NA, "Berlin-Friedrichshain", NA, NA, NA, NA, NA, NA, NA, "Berlin", "Berlin", "Berlin-Neukölln", "Berlin-Wilmersdorf", NA, "Berlin", "Berlin", NA, NA, NA, NA, NA, NA, "Iselberg", NA, "München", NA, "München", "Wiesbaden", NA, NA, NA, "Berlin-Wilmersdorf", NA, "Berlin-Moabit", "Berlin-Friedenau", "Berlin", "München", NA, "Berlin", "Berlin", NA, "Berlin", NA, NA, "Berlin", "Berlin-Wilmersdorf", NA, "Berlin", "Berlin", "Leipzig", "Berlin", NA, NA, "Berlin", NA, NA, "Berlin-Friedenau", "Berlin-Spandau")
location_100 <- c("Halle", NA, "Freienwalde", NA, "Berlin-Moabit", NA, "Berlin-Steglitz", "Berlin", "Wiesbaden", "Frankfurt", "Berlin", NA, NA, NA, "Duisburg", NA, NA, "Berlin-Steglitz", NA, "Berlin-Charlottenburg", NA, "Berlin-Moabit", NA, "Berlin", NA, "Berlin", NA, "Berlin", NA, "Gransee", NA, NA, NA, NA, "Berlin", NA, NA, "Berlin", "Hamburg", NA, "Berlin", NA, NA, "Dresden", NA, NA, "Berlin-Neukölln", "Berlin-Friedrichshain", NA, NA, NA, "Dortmund", "Berlin", NA, NA, "Berlin-Friedrichshain", "Hamburg", "Ludwigslust", NA, "Nürnberg", "Berlin", "Berlin-Wilmersdorf", NA, "Berlin")
location_150 <- c("Berlin", NA, "Berlin", NA, NA, "Nürnberg", "Nürnberg", NA, NA, NA, "Berlin-Charlottenburg", NA, NA, "Berlin", NA, "Berlin", "Berlin", NA, "Breslau", "Frankfurt", "Mannheim", "Weinheim", NA, "Berlin", NA, "Berlin-Friedenau", "Berlin-Kaulsdorf", "Berlin-Spandau", "Darmstadt", NA, "Berlin", NA, NA, "Karlsruhe", "Lübeck", NA, "Berlin", "Berlin-Zehlendorf", NA, "Berlin", NA, NA, "Potsdam", "Frankfurt", "Hannover", NA, "Wiesbaden", "Leipzig", "Weidmannslust", "Berlin-Wedding", "Berlin-Friedrichshagen", FALSE, "Weimar", "Berlin", NA, NA, "Coburg", "Neustadt a. d. Hardt", "Berlin", NA, NA, NA, NA, NA, NA, NA, NA, NA)
location_200 <- c(NA, "Berlin", NA, NA, NA, NA, "Prag", NA, NA, NA, "Berlin", NA, NA, "Nürnberg", "Magdeburg", "Berlin", "Berlin", "Berlin", NA, NA, "Berlin", NA, NA, "Görlitz", NA, NA, NA, NA, NA, "Berlin-Tempelhof", NA, "Bonn", NA, "Berlin-Zehlendorf", NA, "Eutin", NA, "Berlin-Köpenick", "Berlin-Treptow", NA, NA, "Leipzig", NA, "Berlin_Steglitz", "Reinickendorf", NA, "Plauen", NA, NA, "Berlin-Südende", NA, "Berlin", "Berlin", NA, NA, NA, NA, NA, NA, "Berlin-Wilmersdorf", "Berlin")
location_250 <- c(NA , NA, NA, "Dresden", "Breslau", NA, "Berlin", "Berlin-Alexanderplatz", "Berlin", "Berlin", "Berlin", NA, NA, NA, NA, "Berlin", NA, NA, NA, "Weimar", NA, NA, NA, "Berlin", NA, NA, "Berlin", NA, NA, "Hamburg", "Danzig", NA, NA, "Berlin", NA, "Chemnitz", NA, "Berlin", "Berlin-Lichterfelde", NA, "Berlin", NA, "Berlin", "Berlin", NA, NA, NA, NA, NA, NA, NA, "Breslau", "Berlin", NA, NA, NA, NA, NA)
location_300 <- c("Weimar", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Berlin", NA, NA, "Danzig", "Dresden", "München", NA, NA, "München", NA, NA, NA, "Berlin", "Chemnitz", NA, NA, NA, NA, "Berlin", NA, "Braunschweig", "Mannheim", NA, "Berlin", NA, "Dessau", "Chemnitz", "Leipzig", "München", "Erfurt", NA, NA, NA, "Marburg", NA, NA, NA, "Dresden", "Hamburg", "Berlin", NA, NA, NA, NA, NA, NA, NA)
location_350 <- c("Berlin", "Berlin", NA, "Berlin", "Berlin", "Köln", "Berlin-Lichtenrade", "Berlin-Spandau", "Weimar", NA, NA, NA, NA, NA, NA, NA, "Weimar", NA, NA, NA, "Berlin", NA, NA, NA, NA, NA, "Berlin-Tegel", NA, NA, NA, NA, "Berlin", "Morien", "Nordenham", "Wildeshausen", "Jever", "Küstringen", "Berlin", "Parchim", "Wismar", "Neukloster", "Waren", "Berlin-Schöneberg", NA, NA, NA, "Wittenberg", "Berlin-Schlachtensee", "Berlin-Grünau", "Berlin-Franfurter-Allee", "Potsdam", "Berlin-Reinickendorf", "Berlin", NA, "Berlin", "Apolda", "Jena", "Gotha", NA, NA, NA)
location_421 <- c(NA, NA, "Berlin", NA, NA, NA, NA, "Berlin", "Berlin-Grunewald", "Berlin-Hermsdorf", "Berlin-Reinickendorf", NA, NA, NA, "Berlin", "Berlin-Gesundbrunnen", NA, "Berlin-Neukölln", NA, "Berlin-Gesundbrunnen", "Berlin-Tempelhof", "Berlin-Mariendorf", "Berlin-Südende", NA, NA, NA, NA, NA, NA, NA, "Berlin", "Weimar", NA,  "Eisenach", "Apolda", NA, "Arnstadt", "Blankenburg", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Berlin", NA, "Berlin", NA, "Alverdissen", "Lüdenhausen", "Almena", "Lippe", NA, NA, NA, "Berlin", NA, "Berlin", NA, NA, NA, NA, NA, NA, "Rostock", "Berlin", NA, NA, "Potsdam", NA, NA, "Berlin", "Berlin", NA, NA, "Dortmund", NA, "Berlin", NA, NA, NA, "Berlin")

# place of appearance
place_50 <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Franziskaner, Blüte, Bürgerbräu, Hackerkeller", NA, NA, NA, NA, NA, "Kriegervereinshaus", NA, NA, NA, NA, NA, NA, "Bockbrauerei", NA, NA, NA, "Reichstag, Saalbau", "Reichstag", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Bockbrauerei", NA, NA, NA, NA, NA, "Tiergartenhof", NA, NA, NA, NA, NA, NA, "Reichstag", NA, "Sportpalast", NA, NA, "Märchenbrunnen", NA, NA, NA, NA)
place_100 <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, "Zoo", "Kammersäle", NA, NA, NA, NA, NA, NA, NA, NA, "Karnevalssaal", NA, NA, NA, NA, NA, "Neue Welt", NA, "Kriegervereinshaus", NA, NA, NA, NA, NA, NA, "Tiergartenhof", NA, NA, "Bockbrauerei", "Zirkus Busch", NA, "Unionsbrauerei", NA, NA, NA, NA, NA, NA, "Friedrichshainer Saalbau", NA, NA, NA, NA, "Neue Welt", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
place_150 <- c("Neue Welt", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Phoebuspalast", NA, NA, "Neue Welt", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Kriegervereinshaus", NA, NA, NA, NA, NA, "Kriegervereinshaus", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Kriegervereinshaus", NA, NA, NA, NA, "Kriegsvereinshaus", NA, NA, NA, NA, NA, NA, NA, NA, NA)
place_200 <- c(NA, "Viktoriagarten", NA, NA, NA, NA, NA, NA, NA, NA, "Kriegsvereinshaus", NA, NA, NA, NA, "Rotes Haus", NA, "Bachsaal", NA, NA, "Kriegervereinshaus", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Zoo", NA, NA, NA, NA, NA, NA, NA, NA, NA, "Neue Welt", "Sportpalast", NA, NA, NA, NA, NA, NA, NA, "Burgkeller")
place_250 <- c(NA, NA, NA, NA, NA, NA, "Sportpalast", NA, NA, "Sportpalast", NA, NA, NA, NA, NA, NA, NA, NA, NA, "Nationaltheater", NA, NA, NA, "Kriegsvereinshaus", NA, NA, "Saalbau", NA, NA, NA, NA, NA, NA, "Sportpalast", NA, NA, NA, "Sportpalast", NA, NA, "Wittenbergplatz", NA, "Neue Welt", "Singakademie", NA, NA, NA, NA, NA, NA, NA, NA, "Reichstag", NA, NA, NA, NA , NA)
place_300 <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Sportpalast", NA, NA, NA, NA, NA, "Sportpalast", NA, NA, NA, NA, "Sportpalast", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Stadion", NA, "Sportpalast", NA, NA, NA, NA, NA, NA, NA)
place_350 <- c("Sportpalast", "Sportpalast", NA, NA, "Nationaler Club", NA, NA, NA, "Stadthalle", NA, NA, NA, NA, NA, NA, NA, "Th?ringische Halle", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Tennishallen", NA, NA, NA, NA, NA, "Tennishallen", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Sportpalast", NA, NA, NA, NA, NA, NA, NA, NA)
place_421 <- c(NA, NA, "Hasenheide", NA, NA, NA, NA, "Kriegsvereinshaus", NA, NA, NA, NA, NA, NA, "Kriegervereinshaus", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Tennishallen", "Weimarhalle", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Sportpalast", NA, "Lustgarten", NA, NA, NA, NA, NA, NA, NA, NA, "Hotel Atlas", NA, "Sportpalast", NA, NA, NA, NA, NA, NA, NA, "Reichskanzlei", NA, NA, NA, NA, NA, "Tennishalle", "Sportpalast", NA, NA, NA, NA, "Tennishallen", NA, NA, NA, "Sportpalast")

# Goebbel's guess of audience size
audience_size_50 <- c("voll", NA, NA, NA, NA, "überfüllt", NA, NA, NA, NA, "überfüllt", "überfüllt", "überfüllt", NA, NA, NA, "überfüllt", "übervoll", NA, "überfüllt", NA, NA, "überfüllt", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "voll", "überfüllt", NA, "masse", NA, "masse", NA, NA, "überfüllt", NA, NA, NA, NA, NA, NA, NA, NA, "überfüllt", NA, NA, "vieltausendköpfig", NA, NA, NA, NA)
audience_size_100 <- c(NA, NA, "überfüllt", NA, "überfüllt", NA, NA, NA, "überfüllt", "riesenversammlung", "überfüllt", NA, NA, NA, "überfüllt", NA, NA, NA, NA, NA, NA, "voller Saal", NA, NA, NA, NA, NA, NA, NA, "überfüllt", NA, NA, NA, NA, "voller Saal", NA, NA, NA, "überfüllt", NA, "überfüllt", NA, NA, "überfüllt", NA, NA, NA, "überfüllt", NA, NA, NA, NA, NA, NA, NA, NA, "überfüllt", "kleine Versammlung", NA, "überfüllt", NA, "überfüllt", NA, NA)
audience_size_150 <- c("überfüllt", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "gut besucht", NA, NA, "überfüllt", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "überfüllt", NA, "überfüllt", NA, NA, NA, NA, NA, "überfüllt", "überfüllt", NA, NA, NA, NA, "überfüllt", NA, NA, NA, "überfüllt", "überfüllt", NA, NA, "überfüllt", NA, "überfüllt", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
audience_size_200 <- c(NA, NA, NA, NA, NA, NA, "überfüllt", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "riesenversammlung", NA, NA, NA, NA, NA, "überfüllt", NA, NA, NA, "überfüllt", NA, NA, NA, NA, NA, NA, NA, NA, NA, "überfüllt", NA, NA, "überfüllt", NA, NA, NA, NA, "überfüllt", "überfüllt", NA, NA, NA, NA, NA, NA, "überfüllt", NA)
audience_size_250 <- c(NA, NA, NA, NA, "überfüllt", NA, NA, NA, NA, "überfüllt", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "überfüllt", NA, NA, NA, NA, NA, NA, NA, "überfüllt", NA, NA, NA, "überfüllt", NA, NA, NA, NA, NA, NA, NA, NA, NA, "überfüllt", NA, NA, NA, NA, NA)
audience_size_300 <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "überfüllt", "überfüllt", NA, NA, NA, "überfüllt", NA, NA, NA, NA, "überfüllt", NA, NA, NA, NA, NA, NA, "brechen voll", NA, NA, NA, NA, NA, NA, NA, "überfüllt", NA, NA, NA, NA, "überfüllt", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
audience_size_350 <- c("überfüllt", "überfüllt", NA, "überfüllt", NA, NA, "überfüllt", NA, NA, NA, NA, NA, NA, NA, NA, NA, "überfüllt", NA, NA, NA, "überfüllt", NA, NA, NA, NA, NA, "überfüllt", NA, NA, NA, NA, "überfüllt", "überfüllt", "überfüllt", "überfüllt", NA, NA, NA, NA, NA, NA, "30000 Menschen", NA, NA, NA, NA, "überfüllt", "überfüllt", "überfüllt", "überfüllt", NA, NA, NA, NA, NA, NA, "überfüllt", NA, NA, NA, NA)
audience_size_421 <- c(NA, NA, NA, NA, NA, NA, NA, "überfüllt", NA, "überfüllt", "überfüllt", NA, NA, NA, "überfüllt", "überfüllt", NA, "überfüllt", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "überfüllt", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "überfüllt", NA, "überfüllt", NA, NA, NA, NA, NA, NA, NA, "massen", NA, NA, NA, NA, NA, NA, "überfüllt", NA, NA, "überfüllt", NA, NA, NA, NA, NA, NA)

# speeches on dates refered to but not covered by extracted sections, yes/no
date_added_50 <- c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE)
date_added_100 <- c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)
date_added_150 <- c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
date_added_200 <- c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE)
date_added_250 <- c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
date_added_300 <- c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
date_added_350 <- c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
date_added_421 <- c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)

# dates of speeches referred to but not covered by extracted sections
add_dates_50 <- ymd(c("1928-04-29", "1928-05-09", "1928-05-18", "1928-06-08", "1928-06_13", "1928-06-21", "1928-06-28", "1928-08-28", "1928-08-31", "1928-09-02", "1928-09-03", "1928-09-24", "1928-09-28", "1928-10-02", "1928-10-02", "1928-10-12", "1928-10-25", "1928-11-14", "1928-11-14", "1928-11-23", "1928-11-29"))
add_dates_100 <- ymd(c("1928-12-03", "1928-12-05", "1928-12-11", "1928-12-12", "1928-12-13", "1929-02-06", "1929-03-08", "1929-03-14", "1929-04-05", "1929-04-16", "1929-04-29", "1929-06-16", "1929-06-17", "1929-06-21"))
add_dates_150 <- ymd(c("1929-06-28", "1929-07-05", "1929-09-01", "1929-09-6", "1929-09-17", "1929-09-18", "1929-09-28", "1929-10-09", "1929-10-15", "1929-10-18", "1929-11-12", "1929-11-13", "1929-11-15", "1929-11-16", "1929-11-21", "1929-11-27", "1929-11-28", "1929-12-06"))
add_dates_200 <- ymd(c("1930-01-17", "1930-02-14", "1930-05-01", "1930-05-21", "1930-05-23", "1930-06-13", "1930-06-16", "1930-06-21", "1930-06-28", "1930-06-30", "1930-08-14"))
add_dates_250 <- ymd(c("1930-08-27", "1930-09-10", "1930-10-12", "1930-10-17", "1930-10-28", "193-11-15", "1930-12-05", "1931-02-05"))
add_dates_300 <- ymd(c("1931-02-22", "1931-03-23", "1931-04-03", "1931-04-07", "1931-05-13", "1931-05-19", "1931-06-07", "1931-06-09"))
add_dates_350 <- ymd(c("1932-05-26", "1932-05-26", "1932-05-27", "1932-05-27", "1932-06-03", "1932-06-03", "1932-06-25", "1932-06-25", "1932-06-26", "1932-07-20", "1932-07-20"))
add_dates_421 <- ymd(c("1932-09-02", "1932-09-04", "1932-09-16", "1932-11-02", "1932-11-02", "1932-11-02", "1932-11-30", "1932-12-01", "1932-12-01", "1933-01-05", "1933-01-07", "1933-01-09", "1933-01-09", "1933-01-09", "1933-01-17", "1933-02-17"))

# prepare date vectors
dates_vec_50 <- sort(c(dates_vec[1:50], add_dates_50))
dates_vec_100 <- sort(c(dates_vec[51:100], add_dates_100))
dates_vec_150 <- sort(c(dates_vec[101:150], add_dates_150))
dates_vec_200 <- sort(c(dates_vec[151:200], add_dates_200))
dates_vec_250 <- sort(c(dates_vec[201:250], add_dates_250))
dates_vec_300 <- sort(c(dates_vec[251:300], add_dates_300))
dates_vec_350 <- sort(c(dates_vec[301:350], add_dates_350))
dates_vec_421 <- sort(c(dates_vec[351:421], add_dates_421))

# merge data
goebbels_df_50 <- data.frame(dates_vec_50, speech_50, speech_goebbels_50, speech_hitler_50, speech_other_50, speech_other_who_50, public_50, location_50, place_50, audience_size_50, date_added_50)
goebbels_df_100 <- data.frame(dates_vec_100, speech_100, speech_goebbels_100, speech_hitler_100, speech_other_100, speech_other_who_100, public_100, location_100, place_100, audience_size_100, date_added_100)
goebbels_df_150 <- data.frame(dates_vec_150, speech_150, speech_goebbels_150, speech_hitler_150, speech_other_150, speech_other_who_150, public_150, location_150, place_150, audience_size_150, date_added_150)
goebbels_df_200 <- data.frame(dates_vec_200, speech_200, speech_goebbels_200, speech_hitler_200, speech_other_200, speech_other_who_200, public_200, location_200, place_200, audience_size_200, date_added_200)
goebbels_df_250 <- data.frame(dates_vec_250, speech_250, speech_goebbels_250, speech_hitler_250, speech_other_250, speech_other_who_250, public_250, location_250, place_250, audience_size_250, date_added_250)
goebbels_df_300 <- data.frame(dates_vec_300, speech_300, speech_goebbels_300, speech_hitler_300, speech_other_300, speech_other_who_300, public_300, location_300, place_300, audience_size_300, date_added_300)
goebbels_df_350 <- data.frame(dates_vec_350, speech_350, speech_goebbels_350, speech_hitler_350, speech_other_350, speech_other_who_350, public_350, location_350, place_350, audience_size_350, date_added_350)
goebbels_df_421 <- data.frame(dates_vec_421, speech_421, speech_goebbels_421, speech_hitler_421, speech_other_421, speech_other_who_421, public_421, location_421, place_421, audience_size_421, date_added_421)

colnames(goebbels_df_50) <- c("date", "speech", "goebbels", "hitler", "other", "who", "public", "location", "place", "audience", "additional date")
colnames(goebbels_df_100) <- c("date", "speech", "goebbels", "hitler", "other", "who", "public", "location", "place", "audience", "additional date")
colnames(goebbels_df_150) <- c("date", "speech", "goebbels", "hitler", "other", "who", "public", "location", "place", "audience", "additional date")
colnames(goebbels_df_200) <- c("date", "speech", "goebbels", "hitler", "other", "who", "public", "location", "place", "audience", "additional date")
colnames(goebbels_df_250) <- c("date", "speech", "goebbels", "hitler", "other", "who", "public", "location", "place", "audience", "additional date")
colnames(goebbels_df_300) <- c("date", "speech", "goebbels", "hitler", "other", "who", "public", "location", "place", "audience", "additional date")
colnames(goebbels_df_350) <- c("date", "speech", "goebbels", "hitler", "other", "who", "public", "location", "place", "audience", "additional date")
colnames(goebbels_df_421) <- c("date", "speech", "goebbels", "hitler", "other", "who", "public", "location", "place", "audience", "additional date")

goebbels_df <- rbind(goebbels_df_50, goebbels_df_100, goebbels_df_150, goebbels_df_200, goebbels_df_250, goebbels_df_300, goebbels_df_350, goebbels_df_421)

# extract only Goebbels speeches
goebbels_df <- filter(goebbels_df, goebbels == TRUE, public == TRUE)


# geocode speeches
goebbels_df$location <- goebbels_df$location %>% str_replace("ö", "oe") %>% str_replace("ü", "ue") %>% str_replace("ä", "ae") %>% str_replace("ß", "ss")
if (!file.exists("./data_speeches/coordinatesGoogleAPIGoebbelsSpeeches.RData")){
  geocodes <- geocode(goebbels_df$location, source = "google") # this takes a while
  save(geocodes, file = "./data_speeches/coordinatesGoogleAPIGoebbelsSpeeches.RData")
} else {
  load("./data_speeches/coordinatesGoogleAPIGoebbelsSpeeches.RData")
}
goebbels_df <- cbind(goebbels_df, geocodes)
goebbels_df <- filter(goebbels_df, !is.na(lat))

# correct location entries
goebbels_df$lon[goebbels_df$location=="Almena"] <- 9.078611
goebbels_df$lat[goebbels_df$location=="Almena"] <- 52.105833
goebbels_df$lon[goebbels_df$location=="Morien"] <- 8.916625
goebbels_df$lat[goebbels_df$location=="Morien"] <- 53.155460

goebbels_df$berlin <- str_detect(goebbels_df$location, "Berlin")
table(goebbels_df$berlin) # more than half of his documented public speeches (110 out of 200) took place in Berlin
goebbels_df$berlin <- NULL

# save data frame
save(goebbels_df, file= "goebbels_df.RData")
