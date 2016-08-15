#----------------------------------------------------------------
# Examining a Most Likely Case for Strong Campaign Effects: 
# Hitler's Speeches and the Rise of the Nazi Party, 1927--1933
# Peter Selb (University of Konstanz)
# Simon Munzert (MZES, University of Mannheim)
# 2016
#----------------------------------------------------------------

## imports
c("data_members/NSDAP_Mitglieder 1925_1933.dta")

## exports
c("members_df_wkr_wide.RData")


## load packages and functions -------------------
source("packages.r")
source("functions.r")

## sampling/archiving information ----------------------
# Use re-counts of boxes in Aug 1990 as basis: 4'956 (Blue registry), 2'526 (Green registry)
# Blue registry: approximately 35[cm]*45[cards per cm]=1'575 members per box
# Green registry: approximately 70[cm]*24.6[cards per cm]=1'722 members per box
# BERLIN TEAM
# (a) Blue registry,  before 1930:
#  - Sampling rate boxes: 1/25, 203 boxes sampled
# - 1/2 of each box considered
# - all pre-1930 members selected

# load data
members_df <- read.dta("data_members/NSDAP_Mitglieder 1925_1933.dta", convert.factors = FALSE)

# setup boxes
n_blue_ber <- 203
n_blue_min <- 511
n_green_ber <- 197
n_green_min <- 483
N_blue <- 4956 - 4956*.065 # 6.5% are Austrian members (Botz 1980; in Schneider-Haase 1991: 116)
N_green <- 2526 - 2526*.02 # 1.2% to 2.8% stems from Franconian regional registers (Schneider-Haase 1991: 116)
blue_size <- 1575
green_size <- 1722

# total estimate
(total_est_blue <- N_blue*blue_size)
(total_est_green <- N_green*green_size)

# "Insbesondere ist davon auszugehen, daß in dieser [BLAUEN] Kartei Bayern unterrepräsentativ vertreten ist, wenn man eigennützig handelnde Individuen unterstellt, die bei einer Vernichtung der Kartei erstmal ihre eigenen Karteikarten zerstörten." - Schneider-Haase 1991, 119

# "Umgekehrt ist für die GRÜNE Kartei anzunehmen, daß die Teilvernichtung dieser Kartei ganze Nachnamensfolgen betraf - diese Kartei war ja bereits ursprünglich alphabetisch geordnet. Da die Nachnamen mit Regionen korrelieren und diese wiederum mit einer typischen Sozialstruktur, ist auch hier von keiner Repräsentativität bzgl. der (unbekannten) Grundgesamtheit der NSDAP-Mitglieder auszugehen. Zudem wurde der Bestand der GRÜNEN Kartei, wie bereits angemerkt, augenscheinlich mit Beständen einer Regionalkartei des Raumes Franken vermischt. " - Schneider-Haase 1991, 119


## generating re-adjustment weights ----------------------------

# Kater's sample
year <- seq(25, 45, 1)
kater <- c(34, 32, 23, 43, 112, 361, 829, 905, 3502, 37, 223, 190, 4330, 314, 1231, 2217, 1054, 872, 749, 1196, 1)
kater_frac <- kater/18255
kater_df <- data.frame(year, kater, kater_frac)

# create weight variable
members_df <- members_df[members_df$kart !=3,]
members_df$wgt <- NA

# Berlin team, blue boxes
members_df$wgt[members_df$team == 1 & members_df$kart == 1 & members_df$ent1y == 25] <- ((n_blue_ber / N_blue)*.5)^-1 
members_df$wgt[members_df$team == 1 & members_df$kart == 1 & members_df$ent1y == 26] <- ((n_blue_ber / N_blue)*.5)^-1 
members_df$wgt[members_df$team == 1 & members_df$kart == 1 & members_df$ent1y == 27] <- ((n_blue_ber / N_blue)*.5)^-1 
members_df$wgt[members_df$team == 1 & members_df$kart == 1 & members_df$ent1y == 28] <- ((n_blue_ber / N_blue)*.5)^-1 
members_df$wgt[members_df$team == 1 & members_df$kart == 1 & members_df$ent1y == 29] <- ((n_blue_ber / N_blue)*.5)^-1 
members_df$wgt[members_df$team == 1 & members_df$kart == 1 & members_df$ent1y == 30] <- ((n_blue_ber / N_blue)*.5)^-1 * (kater_df$kater_frac[kater_df$year == 30] * blue_size/2 / 5)
members_df$wgt[members_df$team == 1 & members_df$kart == 1 & members_df$ent1y == 31] <- ((n_blue_ber / N_blue)*.5)^-1 * (kater_df$kater_frac[kater_df$year == 31] * blue_size/2 / 5)
members_df$wgt[members_df$team == 1 & members_df$kart == 1 & members_df$ent1y == 32] <- ((n_blue_ber / N_blue)*.5)^-1 * (kater_df$kater_frac[kater_df$year == 32] * blue_size/2 / 5)
members_df$wgt[members_df$team == 1 & members_df$kart == 1 & members_df$ent1y == 33] <- ((n_blue_ber / N_blue)*.5)^-1 * (kater_df$kater_frac[kater_df$year == 33] * blue_size/2 / 5) * .05 # according to Kater (1980) sample, only about 5% joined NSDAP in 1933 before march elections

# Minneapolis team, blue boxes
members_df$wgt[members_df$team == 2 & members_df$kart == 1 & members_df$ent1y == 25] <- ((n_blue_min / N_blue)*.5)^-1 
members_df$wgt[members_df$team == 2 & members_df$kart == 1 & members_df$ent1y == 26] <- ((n_blue_min / N_blue)*.5)^-1 
members_df$wgt[members_df$team == 2 & members_df$kart == 1 & members_df$ent1y == 27] <- ((n_blue_min / N_blue)*.5)^-1 
members_df$wgt[members_df$team == 2 & members_df$kart == 1 & members_df$ent1y == 28] <- ((n_blue_min / N_blue)*.5)^-1 
members_df$wgt[members_df$team == 2 & members_df$kart == 1 & members_df$ent1y == 29] <- ((n_blue_min / N_blue)*.5)^-1 
members_df$wgt[members_df$team == 2 & members_df$kart == 1 & members_df$ent1y == 30] <- ((n_blue_min / N_blue)*.5)^-1 * (kater_df$kater_frac[kater_df$year == 30] * blue_size/2 / 6)
members_df$wgt[members_df$team == 2 & members_df$kart == 1 & members_df$ent1y == 31] <- ((n_blue_min / N_blue)*.5)^-1 * (kater_df$kater_frac[kater_df$year == 31] * blue_size/2 / 6)
members_df$wgt[members_df$team == 2 & members_df$kart == 1 & members_df$ent1y == 32] <- ((n_blue_min / N_blue)*.5)^-1 * (kater_df$kater_frac[kater_df$year == 32] * blue_size/2 / 6)

# Berlin team, green boxes
members_df$wgt[members_df$team == 1 & members_df$kart == 2 & members_df$ent1y == 25] <- ((n_green_ber / N_green)*.5)^-1 
members_df$wgt[members_df$team == 1 & members_df$kart == 2 & members_df$ent1y == 26] <- ((n_green_ber / N_green)*.5)^-1 
members_df$wgt[members_df$team == 1 & members_df$kart == 2 & members_df$ent1y == 27] <- ((n_green_ber / N_green)*.5)^-1 
members_df$wgt[members_df$team == 1 & members_df$kart == 2 & members_df$ent1y == 28] <- ((n_green_ber / N_green)*.5)^-1 
members_df$wgt[members_df$team == 1 & members_df$kart == 2 & members_df$ent1y == 29] <- ((n_green_ber / N_green)*.5)^-1 
members_df$wgt[members_df$team == 1 & members_df$kart == 2 & members_df$ent1y == 30] <- ((n_green_ber / N_green)*.5)^-1 * (kater_df$kater_frac[kater_df$year == 30] * green_size/2 / 5)
members_df$wgt[members_df$team == 1 & members_df$kart == 2 & members_df$ent1y == 31] <- ((n_green_ber / N_green)*.5)^-1 * (kater_df$kater_frac[kater_df$year == 31] * green_size/2 / 5)
members_df$wgt[members_df$team == 1 & members_df$kart == 2 & members_df$ent1y == 32] <- ((n_green_ber / N_green)*.5)^-1 * (kater_df$kater_frac[kater_df$year == 32] * green_size/2 / 5)
members_df$wgt[members_df$team == 1 & members_df$kart == 2 & members_df$ent1y == 33] <- ((n_green_ber / N_green)*.5)^-1 * (kater_df$kater_frac[kater_df$year == 33] * green_size/2 / 5) * .05 # according to Kater (1980) sample, only about 5% joined NSDAP in 1933 before march elections

# Minneapolis team, green boxes
members_df$wgt[members_df$team == 2 & members_df$kart == 2 & members_df$ent1y == 25] <- ((n_green_min / N_green)*.5)^-1 
members_df$wgt[members_df$team == 2 & members_df$kart == 2 & members_df$ent1y == 26] <- ((n_green_min / N_green)*.5)^-1 
members_df$wgt[members_df$team == 2 & members_df$kart == 2 & members_df$ent1y == 27] <- ((n_green_min / N_green)*.5)^-1 
members_df$wgt[members_df$team == 2 & members_df$kart == 2 & members_df$ent1y == 28] <- ((n_green_min / N_green)*.5)^-1 
members_df$wgt[members_df$team == 2 & members_df$kart == 2 & members_df$ent1y == 29] <- ((n_green_min / N_green)*.5)^-1 
members_df$wgt[members_df$team == 2 & members_df$kart == 2 & members_df$ent1y == 30] <- ((n_green_min / N_green)*.5)^-1 * (kater_df$kater_frac[kater_df$year == 30] * green_size/2 / 6)
members_df$wgt[members_df$team == 2 & members_df$kart == 2 & members_df$ent1y == 31] <- ((n_green_min / N_green)*.5)^-1 * (kater_df$kater_frac[kater_df$year == 31] * green_size/2 / 6)
members_df$wgt[members_df$team == 2 & members_df$kart == 2 & members_df$ent1y == 32] <- ((n_green_min / N_green)*.5)^-1 * (kater_df$kater_frac[kater_df$year == 32] * green_size/2 / 6)

sum(members_df$wgt)

# calibrate weights according to their contribution to the gross sample
nobs_blue_ber <- nrow(members_df[members_df$team == 1 & members_df$kart == 1 & members_df$ent1y != 33,])
nobs_blue_min <- nrow(members_df[members_df$team == 2 & members_df$kart == 1 & members_df$ent1y != 33,])
nobs_green_ber <- nrow(members_df[members_df$team == 1 & members_df$kart == 2 & members_df$ent1y != 33,])
nobs_green_min <- nrow(members_df[members_df$team == 2 & members_df$kart == 2 & members_df$ent1y != 33,])
nobs_tot <- nrow(members_df)

nobs_blue_ber_1933 <- nrow(members_df[members_df$team == 1 & members_df$kart == 1 & members_df$ent1y == 33,])
nobs_green_ber_1933 <- nrow(members_df[members_df$team == 1 & members_df$kart == 2 & members_df$ent1y == 33,])
nobs_tot_1933 <- nrow(members_df[members_df$ent1y == 33,])

members_df$wgtcalib <- NA
members_df$wgtcalib[members_df$team == 1 & members_df$kart == 1 & members_df$ent1y != 33] <- members_df$wgt[members_df$team == 1 & members_df$kart == 1 & members_df$ent1y != 33] * (nobs_blue_ber/nobs_tot)
members_df$wgtcalib[members_df$team == 2 & members_df$kart == 1 & members_df$ent1y != 33] <- members_df$wgt[members_df$team == 2 & members_df$kart == 1 & members_df$ent1y != 33] * (nobs_blue_min/nobs_tot)
members_df$wgtcalib[members_df$team == 1 & members_df$kart == 2 & members_df$ent1y != 33] <- members_df$wgt[members_df$team == 1 & members_df$kart == 2 & members_df$ent1y != 33] * (nobs_green_ber/nobs_tot)
members_df$wgtcalib[members_df$team == 2 & members_df$kart == 2 & members_df$ent1y != 33] <- members_df$wgt[members_df$team == 2 & members_df$kart == 2 & members_df$ent1y != 33] * (nobs_green_min/nobs_tot)
members_df$wgtcalib[members_df$team == 1 & members_df$kart == 1 & members_df$ent1y == 33] <- members_df$wgt[members_df$team == 1 & members_df$kart == 1 & members_df$ent1y == 33] * (nobs_blue_ber_1933/nobs_tot_1933)
members_df$wgtcalib[members_df$team == 1 & members_df$kart == 2 & members_df$ent1y == 33] <- members_df$wgt[members_df$team == 1 & members_df$kart == 2 & members_df$ent1y == 33] * (nobs_green_ber_1933/nobs_tot_1933)
sum(members_df$wgtcalib)



# district calculations --------------

members_df$index <- 1

# by krnr
members_df_group  <- group_by(members_df, krnr)
members_df_wkr <- summarise(members_df_group, n_wgt = sum(wgtcalib), n_raw = sum(index))


# by krnr and election
election_dates <- ymd(c("1928-05-20", "1930-09-14", "1932-03-13", "1932-04-10", "1932-07-31", "1932-11-06", "1933-03-05"))
election_dates_df <- data.frame(election_dates, election_label = c("28", "30", "32p1", "32p2", "32r1", "32r2", "33"))

members_df$date <- paste0("19", members_df$ent1y, 
                          ifelse(str_length(members_df$ent1m) == 1, paste0("0", members_df$ent1m), members_df$ent1m),                       ifelse(str_length(members_df$ent1d) == 1, paste0("0", members_df$ent1d), members_df$ent1d))
members_df$date <- ymd(members_df$date)                

members_df$election <- ymd("2000-01-01")
members_df$election <-  ifelse(members_df$date < election_dates[1], election_dates[1],
                               ifelse(members_df$date < election_dates[2], election_dates[2],
                                      ifelse(members_df$date < election_dates[3], election_dates[3],
                                             ifelse(members_df$date < election_dates[4], election_dates[4],  
                                                    ifelse(members_df$date < election_dates[5], election_dates[5],                            
                                                           ifelse(members_df$date < election_dates[6], election_dates[6], 
                                                                  ifelse(members_df$date < election_dates[7], election_dates[7], NA)))))))                     
class(members_df$election) = c('POSIXt','POSIXct')
members_df <- members_df[!is.na(members_df$election),]
members_df <- merge(members_df, election_dates_df, by.x = "election", by.y = "election_dates", all = TRUE)

members_df_group  <- group_by(members_df, krnr, election_label)
members_df_wkr_elec <- summarise(members_df_group, n_wgt = sum(wgtcalib), n_raw = sum(index))

# compute cumulative membership counts, by year
members_df_wkr_elec <- as.data.frame(members_df_wkr_elec)
members_df_wkr_wide <- reshape(members_df_wkr_elec, v.names = c("n_wgt", "n_raw"), idvar = "krnr", timevar = "election_label", direction = "wide")
members_df_wkr_wide$n_wgt.33cum <- rowSums(select(members_df_wkr_wide, n_wgt.33, n_wgt.32r2, n_wgt.32r1, n_wgt.32p2, n_wgt.32p1, n_wgt.30, n_wgt.28), na.rm = TRUE)
members_df_wkr_wide$n_wgt.32r2cum <- rowSums(select(members_df_wkr_wide, n_wgt.32r2, n_wgt.32r1, n_wgt.32p2, n_wgt.32p1, n_wgt.30, n_wgt.28), na.rm = TRUE)
members_df_wkr_wide$n_wgt.32r1cum <- rowSums(select(members_df_wkr_wide, n_wgt.32r1, n_wgt.32p2, n_wgt.32p1, n_wgt.30, n_wgt.28), na.rm = TRUE)
members_df_wkr_wide$n_wgt.32p2cum <- rowSums(select(members_df_wkr_wide, n_wgt.32p2, n_wgt.32p1, n_wgt.30, n_wgt.28), na.rm = TRUE)
members_df_wkr_wide$n_wgt.32p1cum <- rowSums(select(members_df_wkr_wide, n_wgt.32p1, n_wgt.30, n_wgt.28), na.rm = TRUE)
members_df_wkr_wide$n_wgt.30cum <- rowSums(select(members_df_wkr_wide, n_wgt.30, n_wgt.28), na.rm = TRUE)
members_df_wkr_wide$n_wgt.28cum <- rowSums(select(members_df_wkr_wide, n_wgt.28), na.rm = TRUE)


## export data frame -------------------
save(members_df_wkr_wide, members_df_wkr, file = "members_df_wkr_wide.RData")

