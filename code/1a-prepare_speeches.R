#----------------------------------------------------------------
# Examining a Most Likely Case for Strong Campaign Effects: 
# Hitler's Speeches and the Rise of the Nazi Party, 1927--1933
# Peter Selb (University of Konstanz)
# Simon Munzert (MZES, University of Mannheim)
# 2016
#----------------------------------------------------------------


## imports
c("./data_speeches/hitler-reden-schriften-anordnungen-kodierung-complete.xlsx")
  
## exports
c("speeches_df.RData",
  "./data_speeches/coordinatesGoogleAPISpeeches.RData")



## load packages and functions -------------------------------
source("packages.r")
source("functions.r")


## import speech data ----------------------------------------
speeches_df <- readWorksheet(loadWorkbook("./data_speeches/hitler-reden-schriften-anordnungen-kodierung-complete.xlsx"),sheet=1)
names(speeches_df)

# remove speeches that were held before gag order in Bavaria was remitted
speeches_df <- filter(speeches_df, date > ymd("1927-03-09"))

# delete invalid observations
speeches_df <- speeches_df[!(speeches_df$location == "Sommerfeld"),] # Sommerfeld in Austria


## descriptive statistics ------------------------------------
nrow(speeches_df) # 566 speeches encoded
table(speeches_df$meeting_type, useNA = "always") # 455 public, 111 private
speeches_df <-speeches_df[speeches_df$meeting_type!="geschlossen",] # exclude non-public appearances



## impute audience variables -----------------------------------------

# extract numeric information
speeches_df$audience_police_num <- str_extract(speeches_df$audience_police, "^[[:digit:]]+") %>% num()
speeches_df$audience_press_num <- str_extract(speeches_df$audience_press, "^[[:digit:]]+") %>% num()
speeches_df$audience_nspress_num <- str_extract(speeches_df$audience_nspress, "^[[:digit:]]+") %>% num()


# log-log regressions
summary(lm(log(audience_police_num) ~ log(audience_nspress_num), data = speeches_df))
summary(lm(log(audience_police_num) ~ log(audience_press_num), data = speeches_df))
summary(lm(log(audience_police_num) ~ log(audience_press_num) +  log(audience_nspress_num), data = speeches_df))
summary(lm(log(audience_nspress_num) ~ log(audience_police_num), data = speeches_df))


# model-based imputation, police audience variable

# compute log-log models, depending on data availability
model_police_1 <- lm(log(audience_police_num) ~ log(audience_press_num) + log(audience_nspress_num), data = speeches_df)
model_police_2 <- lm(log(audience_police_num) ~ log(audience_press_num), data = speeches_df)
model_police_3 <- lm(log(audience_police_num) ~ log(audience_nspress_num), data = speeches_df)

# draw predictions
speeches_df$audience_police_num_impute_1 <- round(exp(predict(model_police_1, speeches_df)))
speeches_df$audience_police_num_impute_2 <- round(exp(predict(model_police_2, speeches_df)))
speeches_df$audience_police_num_impute_3 <- round(exp(predict(model_police_3, speeches_df)))

# impute missing values
speeches_df$audience_police_num_imputed <- speeches_df$audience_police_num
speeches_df$audience_police_num_imputed[is.na(speeches_df$audience_police_num_imputed)] <- speeches_df$audience_police_num_impute_1[is.na(speeches_df$audience_police_num_imputed)]
speeches_df$audience_police_num_imputed[is.na(speeches_df$audience_police_num_imputed)] <- speeches_df$audience_police_num_impute_2[is.na(speeches_df$audience_police_num_imputed)]
speeches_df$audience_police_num_imputed[is.na(speeches_df$audience_police_num_imputed)] <- speeches_df$audience_police_num_impute_3[is.na(speeches_df$audience_police_num_imputed)]

hist(speeches_df$audience_police_num_imputed)
summary(speeches_df$audience_police_num_imputed)

hist(speeches_df$audience_press_num[is.na(speeches_df$audience_police_num)])
summary(speeches_df$audience_press_num[is.na(speeches_df$audience_police_num)])




## geocode speeches ------------------------------------
speeches_df$location <- speeches_df$location %>% str_replace("ö", "oe") %>% str_replace("ü", "ue") %>% str_replace("ä", "ae") %>% str_replace("ß", "ss")
if (!file.exists("./data_speeches/coordinatesGoogleAPISpeeches.RData")){
  geocodes <- geocode(speeches_df$location, source = "google") # this takes a while
  save(geocodes, file = "./data_speeches/coordinatesGoogleAPISpeeches.RData")
} else {
  load("./data_speeches/coordinatesGoogleAPISpeeches.RData")
}
speeches_df <- cbind(speeches_df, geocodes)


# correct location entries
speeches_df$lon[speeches_df$location=="Altona"] <- 9.933333
speeches_df$lat[speeches_df$location=="Altona"] <- 53.55
speeches_df$lon[str_detect(speeches_df$location,"Freiburg")] <- 7.849881
speeches_df$lat[str_detect(speeches_df$location,"Freiburg")] <- 47.994828
speeches_df$lon[speeches_df$location=="Gotha"] <- 10.709288
speeches_df$lat[speeches_df$location=="Gotha"] <- 50.94692
speeches_df$lon[speeches_df$location=="Idar"] <- 7.313056
speeches_df$lat[speeches_df$location=="Idar"] <- 49.711389


# select variables
save(speeches_df, file = "speeches_df.RData")


