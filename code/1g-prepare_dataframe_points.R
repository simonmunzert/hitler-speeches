#----------------------------------------------------------------
# Examining a Most Likely Case for Strong Campaign Effects: 
# Hitler's Speeches and the Rise of the Nazi Party, 1927--1933
# Peter Selb (University of Konstanz)
# Simon Munzert (MZES, University of Mannheim)
# 2016
#----------------------------------------------------------------


## imports
c("elections_geopoints_df.RData")

## exports
c("community_df_pointdata_long.RData",
  "county_df_pointdata_long.RData",
  "county_pres_df_pointdata_long.RData",
  "community_df_pointdata_long.dta",
  "county_df_pointdata_long.dta",
  "county_pres_df_pointdata_long.dta",
  "elections_all_df_pointdata_long.RData")


## load packages and functions -------------------------------
source("packages.r")
source("functions.r")


## import prepared data --------------------------
load("elections_geopoints_df.RData")
elections_df <- elections_geo_df

table(elections_df$election)
# elections and indices
# 0 = Reichstag election 1924
# 1 = Reichstag election 1928
# 2 = Reichstag election 1930
# 3 = Reichstag election 1932_1
# 4 = Reichstag election 1932_2
# 5 = Reichstag election 1933


# rename variables -------------------------------
varnames_n24d <- select(elections_df, starts_with("n24d")) %>% names()
varnames_n24d_replace <- varnames_n24d %>% str_replace("n24d", "") %>% str_c("0")
elections_df <- rename.vars(elections_df, varnames_n24d, varnames_n24d_replace)

varnames_n285 <- select(elections_df, starts_with("n285")) %>% names()
varnames_n285_replace <- varnames_n285 %>% str_replace("n285", "") %>% str_c("1")
elections_df <- rename.vars(elections_df, varnames_n285, varnames_n285_replace)

varnames_n309 <- select(elections_df, starts_with("n309")) %>% names()
varnames_n309_replace <- varnames_n309 %>% str_replace("n309", "") %>% str_c("2")
elections_df <- rename.vars(elections_df, varnames_n309, varnames_n309_replace)

varnames_n327 <- select(elections_df, starts_with("n327")) %>% names()
varnames_n327_replace <- varnames_n327 %>% str_replace("n327", "") %>% str_c("3")
elections_df <- rename.vars(elections_df, varnames_n327, varnames_n327_replace)

varnames_n32n <- select(elections_df, starts_with("n32n")) %>% names()
varnames_n32n_replace <- varnames_n32n %>% str_replace("n32n", "") %>% str_c("4")
elections_df <- rename.vars(elections_df, varnames_n32n, varnames_n32n_replace)

varnames_n333 <- select(elections_df, starts_with("n333")) %>% names()
varnames_n333_replace <- varnames_n333 %>% str_replace("n333", "") %>% str_c("5")
elections_df <- rename.vars(elections_df, varnames_n333, varnames_n333_replace)

varnames_n323 <- select(elections_df, starts_with("n323")) %>% names()
varnames_n323_replace <- varnames_n323 %>% str_replace("n323", "") %>% str_c("6")
elections_df <- rename.vars(elections_df, varnames_n323, varnames_n323_replace)

varnames_n324 <- select(elections_df, starts_with("n324")) %>% names()
varnames_n324_replace <- varnames_n324 %>% str_replace("n324", "") %>% str_c("7")
elections_df <- rename.vars(elections_df, varnames_n324, varnames_n324_replace)

varnames_z <- select(elections_df, starts_with("z")) %>% names()
varnames_z_replace <- varnames_z %>% str_replace("z", "") %>% str_c("zentrum",.) %>% str_replace("x", "")
elections_df <- rename.vars(elections_df, varnames_z, varnames_z_replace)

varnames_ddpx <- select(elections_df, starts_with("ddpx")) %>% names()
varnames_ddpx_replace <- varnames_ddpx %>% str_replace("ddpx", "") %>% str_c("ddp",.)
elections_df <- rename.vars(elections_df, varnames_ddpx, varnames_ddpx_replace)

varnames_nsda <- select(elections_df, starts_with("nsda")) %>% names()
varnames_nsda_replace <- varnames_nsda %>% str_replace("nsda", "") %>% str_c("nsdap",.)
elections_df <- rename.vars(elections_df, varnames_nsda, varnames_nsda_replace)

elections_df <- rename.vars(elections_df, "nsfb0", "nsdap0")

# import data.frame of to-be-renamed vars
varnames_df <- read.csv("varnames_df.csv", stringsAsFactors = FALSE)

elections_df <- rename.vars(elections_df, varnames_df$varnames_orig, varnames_df$varnames_replace)


## drop unneeded variables -----------------------------------
elections_df <- elections_df[,names(elections_df)[!str_detect(names(elections_df), "^n206|^n245|^n_|^coords|landw25|landw33|alos33")]]


## reshape data frame to long format -------------------------
v_names <- c("pop", "wb", "as", "us", "gs", "dnvp", "dvp", "zentrum", "ddp", "spd", "uspd", "kpd", "nsdap", "dust", "hind", "hitl", "thae", "wint",
             "visit_5km", "visit_10km", "visit_25km", "visit_50km",
             "visit_2w_5km", "visit_2w_10km", "visit_2w_25km", "visit_2w_50km",
             "visit_4w_5km", "visit_4w_10km", "visit_4w_25km", "visit_4w_50km",
             "visit_8w_5km", "visit_8w_10km", "visit_8w_25km", "visit_8w_50km",
             "visit_12w_5km", "visit_12w_10km", "visit_12w_25km", "visit_12w_50km",
             "visit_5km_nomatch", "visit_10km_nomatch", "visit_25km_nomatch", "visit_50km_nomatch",
             "visit_2w_5km_nomatch", "visit_2w_10km_nomatch", "visit_2w_25km_nomatch", "visit_2w_50km_nomatch",
             "visit_4w_5km_nomatch", "visit_4w_10km_nomatch", "visit_4w_25km_nomatch", "visit_4w_50km_nomatch",
             "visit_8w_5km_nomatch", "visit_8w_10km_nomatch", "visit_8w_25km_nomatch", "visit_8w_50km_nomatch",
             "visit_12w_5km_nomatch", "visit_12w_10km_nomatch", "visit_12w_25km_nomatch", "visit_12w_50km_nomatch",			 
             "esize_5km", "esize_10km", "esize_25km", "esize_50km",
             "esize_2w_5km", "esize_2w_10km", "esize_2w_25km", "esize_2w_50km",
             "esize_4w_5km", "esize_4w_10km", "esize_4w_25km", "esize_4w_50km",
             "esize_8w_5km", "esize_8w_10km", "esize_8w_25km", "esize_8w_50km",
             "esize_12w_5km", "esize_12w_10km", "esize_12w_25km", "esize_12w_50km",  
             "vcount_5km", "vcount_10km", "vcount_25km", "vcount_50km",
             "vcount_2w_5km", "vcount_2w_10km", "vcount_2w_25km", "vcount_2w_50km",
             "vcount_4w_5km", "vcount_4w_10km", "vcount_4w_25km", "vcount_4w_50km",
             "vcount_8w_5km", "vcount_8w_10km", "vcount_8w_25km", "vcount_8w_50km",
             "vcount_12w_5km", "vcount_12w_10km", "vcount_12w_25km", "vcount_12w_50km", 
             "goebbels_5km", "goebbels_10km", "goebbels_25km", "goebbels_50km",
             "goebbels_2w_5km", "goebbels_2w_10km", "goebbels_2w_25km", "goebbels_2w_50km",
             "goebbels_4w_5km", "goebbels_4w_10km", "goebbels_4w_25km", "goebbels_4w_50km",
             "goebbels_8w_5km", "goebbels_8w_10km", "goebbels_8w_25km", "goebbels_8w_50km",
             "goebbels_12w_5km", "goebbels_12w_10km", "goebbels_12w_25km", "goebbels_12w_50km",
             "goebbels_5km_nomatch", "goebbels_10km_nomatch", "goebbels_25km_nomatch", "goebbels_50km_nomatch",
             "goebbels_2w_5km_nomatch", "goebbels_2w_10km_nomatch", "goebbels_2w_25km_nomatch", "goebbels_2w_50km_nomatch",
             "goebbels_4w_5km_nomatch", "goebbels_4w_10km_nomatch", "goebbels_4w_25km_nomatch", "goebbels_4w_50km_nomatch",
             "goebbels_8w_5km_nomatch", "goebbels_8w_10km_nomatch", "goebbels_8w_25km_nomatch", "goebbels_8w_50km_nomatch",
             "goebbels_12w_5km_nomatch", "goebbels_12w_10km_nomatch", "goebbels_12w_25km_nomatch", "goebbels_12w_50km_nomatch",		
             "members")
varying_vars <- c(t(outer(v_names, 0:7, paste0)))
setdiff(varying_vars, names(elections_df)) # manually inspect which variables to add to data frame
elections_df <- mutate(elections_df,
                       visit_5km0 = NA, visit_10km0 = NA, visit_25km0 = NA, visit_50km0 = NA, 
                       visit_2w_5km0 = NA, visit_2w_10km0 = NA, visit_2w_25km0 = NA, visit_2w_50km0 = NA,
                       visit_4w_5km0 = NA, visit_4w_10km0 = NA, visit_4w_25km0 = NA, visit_4w_50km0 = NA,
                       visit_8w_5km0 = NA, visit_8w_10km0 = NA, visit_8w_25km0 = NA, visit_8w_50km0 = NA,
                       visit_12w_5km0 = NA, visit_12w_10km0 = NA, visit_12w_25km0 = NA, visit_12w_50km0 = NA,
                       visit_5km0 = NA, visit_10km0 = NA, visit_25km0 = NA, visit_50km0 = NA, 
                       visit_5km_nomatch0 = NA, visit_10km_nomatch0 = NA, visit_25km_nomatch0 = NA, visit_50km_nomatch0 = NA, 	   
                       visit_2w_5km_nomatch0 = NA, visit_2w_10km_nomatch0 = NA, visit_2w_25km_nomatch0 = NA, visit_2w_50km_nomatch0 = NA,
                       visit_4w_5km_nomatch0 = NA, visit_4w_10km_nomatch0 = NA, visit_4w_25km_nomatch0 = NA, visit_4w_50km_nomatch0 = NA,
                       visit_8w_5km_nomatch0 = NA, visit_8w_10km_nomatch0 = NA, visit_8w_25km_nomatch0 = NA, visit_8w_50km_nomatch0 = NA,
                       visit_12w_5km_nomatch0 = NA, visit_12w_10km_nomatch0 = NA, visit_12w_25km_nomatch0 = NA, visit_12w_50km_nomatch0 = NA,	   
                       esize_5km0 = NA, esize_10km0 = NA, esize_25km0 = NA, esize_50km0 = NA,
                       esize_2w_5km0 = NA,  esize_2w_10km0 = NA,  esize_2w_25km0 = NA,  esize_2w_50km0 = NA,
                       esize_4w_5km0 = NA,  esize_4w_10km0 = NA,  esize_4w_25km0 = NA,  esize_4w_50km0 = NA,
                       esize_8w_5km0 = NA,  esize_8w_10km0 = NA,  esize_8w_25km0 = NA,  esize_8w_50km0 = NA,
                       esize_12w_5km0 = NA,  esize_12w_10km0 = NA,  esize_12w_25km0 = NA,  esize_12w_50km0 = NA,
                       vcount_5km0 = NA, vcount_10km0 = NA, vcount_25km0 = NA, vcount_50km0 = NA,
                       vcount_2w_5km0 = NA, vcount_2w_10km0 = NA, vcount_2w_25km0 = NA, vcount_2w_50km0 = NA,
                       vcount_4w_5km0 = NA, vcount_4w_10km0 = NA, vcount_4w_25km0 = NA, vcount_4w_50km0 = NA,
                       vcount_8w_5km0 = NA, vcount_8w_10km0 = NA, vcount_8w_25km0 = NA, vcount_8w_50km0 = NA,
                       vcount_12w_5km0 = NA, vcount_12w_10km0 = NA, vcount_12w_25km0 = NA, vcount_12w_50km0 = NA,   
                       goebbels_5km0 = NA, goebbels_10km0 = NA, goebbels_25km0 = NA, goebbels_50km0 = NA, 
                       goebbels_2w_5km0 = NA, goebbels_2w_10km0 = NA, goebbels_2w_25km0 = NA, goebbels_2w_50km0 = NA,
                       goebbels_4w_5km0 = NA, goebbels_4w_10km0 = NA, goebbels_4w_25km0 = NA, goebbels_4w_50km0 = NA,
                       goebbels_8w_5km0 = NA, goebbels_8w_10km0 = NA, goebbels_8w_25km0 = NA, goebbels_8w_50km0 = NA,
                       goebbels_12w_5km0 = NA, goebbels_12w_10km0 = NA, goebbels_12w_25km0 = NA, goebbels_12w_50km0 = NA,
                       goebbels_5km0 = NA, goebbels_10km0 = NA, goebbels_25km0 = NA, goebbels_50km0 = NA, 
                       goebbels_5km_nomatch0 = NA, goebbels_10km_nomatch0 = NA, goebbels_25km_nomatch0 = NA, goebbels_50km_nomatch0 = NA, 	   
                       goebbels_2w_5km_nomatch0 = NA, goebbels_2w_10km_nomatch0 = NA, goebbels_2w_25km_nomatch0 = NA, goebbels_2w_50km_nomatch0 = NA,
                       goebbels_4w_5km_nomatch0 = NA, goebbels_4w_10km_nomatch0 = NA, goebbels_4w_25km_nomatch0 = NA, goebbels_4w_50km_nomatch0 = NA,
                       goebbels_8w_5km_nomatch0 = NA, goebbels_8w_10km_nomatch0 = NA, goebbels_8w_25km_nomatch0 = NA, goebbels_8w_50km_nomatch0 = NA,
                       goebbels_12w_5km_nomatch0 = NA, goebbels_12w_10km_nomatch0 = NA, goebbels_12w_25km_nomatch0 = NA, goebbels_12w_50km_nomatch0 = NA,         
                       pop6 = pop3, pop7 = pop3,
                       dnvp5 = NA, dnvp6 = NA, dnvp7 = NA,
                       dvp6 = NA, dvp7 = NA,
                       zentrum6 = NA, zentrum7 = NA,
                       ddp2 = NA, ddp3 = NA,  ddp4 = NA, ddp5 = NA, ddp6 = NA, ddp7 = NA,
                       spd6 = NA, spd7 = NA,
                       uspd1 = NA, uspd2 = NA, uspd3 = NA, uspd4 = NA, uspd5 = NA, uspd6 = NA, uspd7 = NA, 
                       kpd6 = NA, kpd7 = NA,
                       nsdap6 = NA, nsdap7 = NA,
                       dust0 = NA, dust1 = NA, dust2 = NA, dust3 = NA, dust4 = NA, dust5 = NA, dust7 = NA,
                       hind0 = NA, hind1 = NA, hind2 = NA, hind3 = NA, hind4 = NA, hind5 = NA,
                       hitl0 = NA, hitl1 = NA, hitl2 = NA, hitl3 = NA, hitl4 = NA, hitl5 = NA,
                       thae0 = NA, thae1 = NA, thae2 = NA, thae3 = NA, thae4 = NA, thae5 = NA,
                       wint0 = NA, wint1 = NA, wint2 = NA, wint3 = NA, wint4 = NA, wint5 = NA, wint7 = NA,
                       members0 = NA)
elections_df_long <- reshape(elections_df, varying = varying_vars, timevar = "election", idvar = "lfnr", times = 0:7, direction = "long", sep = "")
elections_df_long <- arrange(elections_df_long, krnr, lfnr, election)



# re-express votes as proportions of eligibles (not voters!) ---------------------------

# missingness on election variables
iffer <- str_detect(colnames(elections_df_long), "^dnvp|^dvp|^zentrum|^ddp|^spd|^uspd|^kpd|^nsdap|^dust|^hind|^hitl|^thae|^wint")
elections_df_long[,iffer] <- sapply(elections_df_long[,iffer], function(x) ifelse(is.na(x),0,x))

# proportions
elections_df_long <- mutate(elections_df_long, 
                            p_dnvp = dnvp/wb,
                            p_dvp = dvp/wb,
                            p_zentrum = zentrum/wb,
                            p_ddp = ddp/wb,
                            p_spd = spd/wb,
                            p_uspd = uspd/wb,
                            p_kpd = kpd/wb,
                            p_nsdap = nsdap/wb,
                            p_dust = dust/wb,
                            p_hind = hind/wb,
                            p_hitl = hitl/wb,
                            p_thae = thae/wb,
                            p_wint = wint/wb,
                            p_nsdap_as = nsdap/as,
                            p_kpd_as = kpd/as,
                            p_hitl_as = hitl/as,
                            p_thae_as = thae/as)
varnames_proportions <- select(elections_df_long, starts_with("p_")) %>% names()
elections_df_long$p_sum = rowSums(elections_df_long[,varnames_proportions], na.rm = TRUE)
elections_df_long$p_rest <- 1 - elections_df_long$p_sum
elections_df_long$p_turnout = elections_df_long$as / elections_df_long$wb
elections_df_long$p_members <- (elections_df_long$members / elections_df_long$wb)



# re-express outcomes as differences -------------------------
elections_df_long$d_p_turnout <- by(elections_df_long$p_turnout, as.factor(elections_df_long$krnr), diff_x) %>% unlist() %>% as.numeric()
elections_df_long$d_p_nsdap <- by(elections_df_long$p_nsdap, as.factor(elections_df_long$krnr), diff_x) %>% unlist() %>% as.numeric()
elections_df_long$d_p_kpd <- by(elections_df_long$p_kpd, as.factor(elections_df_long$krnr), diff_x) %>% unlist() %>% as.numeric()
elections_df_long$d_p_hitl <- by(elections_df_long$p_hitl, as.factor(elections_df_long$krnr), diff_x) %>% unlist() %>% as.numeric()
elections_df_long$d_p_hind <- by(elections_df_long$p_hind, as.factor(elections_df_long$krnr), diff_x) %>% unlist() %>% as.numeric()
elections_df_long$d_p_thae <- by(elections_df_long$p_thae, as.factor(elections_df_long$krnr), diff_x) %>% unlist() %>% as.numeric()
elections_df_long$d_p_members <- by(elections_df_long$p_members, as.factor(elections_df_long$krnr), diff_x) %>% unlist() %>% as.numeric()
elections_df_long$d_members <- by(elections_df_long$members, as.factor(elections_df_long$krnr), diff_x) %>% unlist() %>% as.numeric()
# also: grouping by community level identificator (lfnr) for 1930 election analysis
elections_df_long$d_p_turnout_comm <- by(elections_df_long$p_turnout, as.factor(elections_df_long$lfnr), diff_x) %>% unlist() %>% as.numeric()
elections_df_long$d_p_nsdap_comm <- by(elections_df_long$p_nsdap, as.factor(elections_df_long$lfnr), diff_x) %>% unlist() %>% as.numeric()
elections_df_long$d_p_kpd_comm <- by(elections_df_long$p_kpd, as.factor(elections_df_long$lfnr), diff_x) %>% unlist() %>% as.numeric()


# generate lag variables -------------------------------------
elections_df_long$l_p_turnout <- by(elections_df_long$p_turnout, as.factor(elections_df_long$krnr), lag) %>% unlist() %>% as.numeric()
elections_df_long$l_p_nsdap <- by(elections_df_long$p_nsdap, as.factor(elections_df_long$krnr), lag) %>% unlist() %>% as.numeric()
elections_df_long$l_p_kpd <- by(elections_df_long$p_kpd, as.factor(elections_df_long$krnr), lag) %>% unlist() %>% as.numeric()
elections_df_long$l_p_hitl <- by(elections_df_long$p_hitl, as.factor(elections_df_long$krnr), lag) %>% unlist() %>% as.numeric()
elections_df_long$l_p_hind <- by(elections_df_long$p_hind, as.factor(elections_df_long$krnr), lag) %>% unlist() %>% as.numeric()
elections_df_long$l_p_thae <- by(elections_df_long$p_thae, as.factor(elections_df_long$krnr), lag) %>% unlist() %>% as.numeric()
elections_df_long$l1_visit_5km <- by(elections_df_long$visit_5km, as.factor(elections_df_long$krnr), lag) %>% unlist() %>% as.numeric()
elections_df_long$l1_visit_10km <- by(elections_df_long$visit_10km, as.factor(elections_df_long$krnr), lag) %>% unlist() %>% as.numeric()
elections_df_long$l1_visit_25km <- by(elections_df_long$visit_25km, as.factor(elections_df_long$krnr), lag) %>% unlist() %>% as.numeric()
elections_df_long$l1_visit_50km <- by(elections_df_long$visit_50km, as.factor(elections_df_long$krnr), lag) %>% unlist() %>% as.numeric()

elections_df_long$l2_visit_5km <- by(elections_df_long$visit_5km, as.factor(elections_df_long$krnr), lag, n = 2) %>% unlist() %>% as.numeric()
elections_df_long$l2_visit_10km <- by(elections_df_long$visit_10km, as.factor(elections_df_long$krnr), lag, n = 2) %>% unlist() %>% as.numeric()
elections_df_long$l2_visit_25km <- by(elections_df_long$visit_25km, as.factor(elections_df_long$krnr), lag, n = 2) %>% unlist() %>% as.numeric()
elections_df_long$l2_visit_50km <- by(elections_df_long$visit_50km, as.factor(elections_df_long$krnr), lag, n = 2) %>% unlist() %>% as.numeric()

elections_df_long$l3_visit_5km <- by(elections_df_long$visit_5km, as.factor(elections_df_long$krnr), lag, n = 3) %>% unlist() %>% as.numeric()
elections_df_long$l3_visit_10km <- by(elections_df_long$visit_10km, as.factor(elections_df_long$krnr), lag, n = 3) %>% unlist() %>% as.numeric()
elections_df_long$l3_visit_25km <- by(elections_df_long$visit_25km, as.factor(elections_df_long$krnr), lag, n = 3) %>% unlist() %>% as.numeric()
elections_df_long$l3_visit_50km <- by(elections_df_long$visit_50km, as.factor(elections_df_long$krnr), lag, n = 3) %>% unlist() %>% as.numeric()

elections_df_long$l4_visit_5km <- by(elections_df_long$visit_5km, as.factor(elections_df_long$krnr), lag, n = 4) %>% unlist() %>% as.numeric()
elections_df_long$l4_visit_10km <- by(elections_df_long$visit_10km, as.factor(elections_df_long$krnr), lag, n = 4) %>% unlist() %>% as.numeric()
elections_df_long$l4_visit_25km <- by(elections_df_long$visit_25km, as.factor(elections_df_long$krnr), lag, n = 4) %>% unlist() %>% as.numeric()
elections_df_long$l4_visit_50km <- by(elections_df_long$visit_50km, as.factor(elections_df_long$krnr), lag, n = 4) %>% unlist() %>% as.numeric()

elections_df_long$l_visit_5km <- apply(elections_df_long[,c("l1_visit_5km", "l2_visit_5km", "l3_visit_5km", "l4_visit_5km")], 1, max, na.rm = TRUE)
elections_df_long$l_visit_5km[is.infinite(elections_df_long$l_visit_5km)] <- NA
elections_df_long$l_visit_10km <- apply(elections_df_long[,c("l1_visit_10km", "l2_visit_10km", "l3_visit_10km", "l4_visit_10km")], 1, max, na.rm = TRUE)
elections_df_long$l_visit_10km[is.infinite(elections_df_long$l_visit_10km)] <- NA
elections_df_long$l_visit_25km <- apply(elections_df_long[,c("l1_visit_25km", "l2_visit_25km", "l3_visit_25km", "l4_visit_25km")], 1, max, na.rm = TRUE)
elections_df_long$l_visit_25km[is.infinite(elections_df_long$l_visit_25km)] <- NA
elections_df_long$l_visit_50km <- apply(elections_df_long[,c("l1_visit_50km", "l2_visit_50km", "l3_visit_50km", "l4_visit_50km")], 1, max, na.rm = TRUE)
elections_df_long$l_visit_50km[is.infinite(elections_df_long$l_visit_50km)] <- NA

# also: grouping by community level identificator (lfnr) for 1930 election analysis
elections_df_long$l_p_turnout_comm <- by(elections_df_long$p_turnout, as.factor(elections_df_long$lfnr), lag) %>% unlist() %>% as.numeric()
elections_df_long$l_p_nsdap_comm <- by(elections_df_long$p_nsdap, as.factor(elections_df_long$lfnr), lag) %>% unlist() %>% as.numeric()
elections_df_long$l_p_kpd_comm <- by(elections_df_long$p_kpd, as.factor(elections_df_long$lfnr), lag) %>% unlist() %>% as.numeric()
elections_df_long$l1_visit_5km_comm <- by(elections_df_long$visit_5km, as.factor(elections_df_long$lfnr), lag) %>% unlist() %>% as.numeric()
elections_df_long$l1_visit_10km_comm <- by(elections_df_long$visit_10km, as.factor(elections_df_long$lfnr), lag) %>% unlist() %>% as.numeric()
elections_df_long$l1_visit_25km_comm <- by(elections_df_long$visit_25km, as.factor(elections_df_long$lfnr), lag) %>% unlist() %>% as.numeric()
elections_df_long$l1_visit_50km_comm <- by(elections_df_long$visit_50km, as.factor(elections_df_long$lfnr), lag) %>% unlist() %>% as.numeric()
elections_df_long$l2_visit_5km_comm <- by(elections_df_long$visit_5km, as.factor(elections_df_long$lfnr), lag, n = 2) %>% unlist() %>% as.numeric()
elections_df_long$l2_visit_10km_comm <- by(elections_df_long$visit_10km, as.factor(elections_df_long$lfnr), lag, n = 2) %>% unlist() %>% as.numeric()
elections_df_long$l2_visit_25km_comm <- by(elections_df_long$visit_25km, as.factor(elections_df_long$lfnr), lag, n = 2) %>% unlist() %>% as.numeric()
elections_df_long$l2_visit_50km_comm <- by(elections_df_long$visit_50km, as.factor(elections_df_long$lfnr), lag, n = 2) %>% unlist() %>% as.numeric()
elections_df_long$l3_visit_5km_comm <- by(elections_df_long$visit_5km, as.factor(elections_df_long$lfnr), lag, n = 3) %>% unlist() %>% as.numeric()
elections_df_long$l3_visit_10km_comm <- by(elections_df_long$visit_10km, as.factor(elections_df_long$lfnr), lag, n = 3) %>% unlist() %>% as.numeric()
elections_df_long$l3_visit_25km_comm <- by(elections_df_long$visit_25km, as.factor(elections_df_long$lfnr), lag, n = 3) %>% unlist() %>% as.numeric()
elections_df_long$l3_visit_50km_comm <- by(elections_df_long$visit_50km, as.factor(elections_df_long$lfnr), lag, n = 3) %>% unlist() %>% as.numeric()
elections_df_long$l4_visit_5km_comm <- by(elections_df_long$visit_5km, as.factor(elections_df_long$lfnr), lag, n = 4) %>% unlist() %>% as.numeric()
elections_df_long$l4_visit_10km_comm <- by(elections_df_long$visit_10km, as.factor(elections_df_long$lfnr), lag, n = 4) %>% unlist() %>% as.numeric()
elections_df_long$l4_visit_25km_comm <- by(elections_df_long$visit_25km, as.factor(elections_df_long$lfnr), lag, n = 4) %>% unlist() %>% as.numeric()
elections_df_long$l4_visit_50km_comm <- by(elections_df_long$visit_50km, as.factor(elections_df_long$lfnr), lag, n = 4) %>% unlist() %>% as.numeric()

elections_df_long$l_visit_5km_comm <- apply(elections_df_long[,c("l1_visit_5km_comm", "l2_visit_5km_comm", "l3_visit_5km_comm", "l4_visit_5km_comm")], 1, max, na.rm = TRUE)
elections_df_long$l_visit_5km_comm[is.infinite(elections_df_long$l_visit_5km_comm)] <- NA
elections_df_long$l_visit_10km_comm <- apply(elections_df_long[,c("l1_visit_10km_comm", "l2_visit_10km_comm", "l3_visit_10km_comm", "l4_visit_10km_comm")], 1, max, na.rm = TRUE)
elections_df_long$l_visit_10km_comm[is.infinite(elections_df_long$l_visit_10km_comm)] <- NA
elections_df_long$l_visit_25km_comm <- apply(elections_df_long[,c("l1_visit_25km_comm", "l2_visit_25km_comm", "l3_visit_25km_comm", "l4_visit_25km_comm")], 1, max, na.rm = TRUE)
elections_df_long$l_visit_25km_comm[is.infinite(elections_df_long$l_visit_25km_comm)] <- NA
elections_df_long$l_visit_50km_comm <- apply(elections_df_long[,c("l1_visit_50km_comm", "l2_visit_50km_comm", "l3_visit_50km_comm", "l4_visit_50km_comm")], 1, max, na.rm = TRUE)
elections_df_long$l_visit_50km_comm[is.infinite(elections_df_long$l_visit_50km_comm)] <- NA



# generate volatility variable ---------------------------------
elections_df_long$d1_p_dnvp <- func_volatility("p_dnvp")
elections_df_long$d1_p_dvp <- func_volatility("p_dvp")
elections_df_long$d1_p_zentrum <- func_volatility("p_zentrum")
elections_df_long$d1_p_ddp <- func_volatility("p_ddp")
elections_df_long$d1_p_spd <- func_volatility("p_spd")
elections_df_long$d1_p_uspd <- func_volatility("p_uspd")
elections_df_long$d1_p_kpd <- func_volatility("p_kpd")
elections_df_long$d1_p_nsdap <- func_volatility("p_nsdap")
elections_df_long$d1_sum <- rowSums(elections_df_long[,c("d1_p_dnvp", "d1_p_dvp", "d1_p_zentrum", "d1_p_ddp", "d1_p_spd", "d1_p_uspd", "d1_p_kpd", "d1_p_nsdap")], na.rm = TRUE)
elections_df_long$volatility <- elections_df_long$d1_sum/2
elections_df_long <- elections_df_long[,!(names(elections_df_long) %in% c("d1_p_dnvp", "d1_p_dvp", "d1_p_zentrum", "d1_p_ddp", "d1_p_spd", "d1_p_uspd", "d1_p_kpd", "d1_p_nsdap", "d1_sum"))]


# generate competitiveness of previous election variables -------------

# primary districts
elections_df_long$l_nsdap <- by(elections_df_long$nsdap, as.factor(elections_df_long$krnr), lag) %>% unlist() %>% as.numeric()
l_nsdap_df <- aggregate(l_nsdap~wkr+election, data=elections_df_long, sum, na.rm=TRUE)
l_nsdap_df <- rename.vars(l_nsdap_df, "l_nsdap", "wkr_nstotal0")
elections_df_long <- merge(elections_df_long, l_nsdap_df, by = c("wkr", "election"), all.x = TRUE)
elections_df_long$wkr_nsseats0 <- floor(elections_df_long$wkr_nstotal0/60000)
elections_df_long$wkr_nsrest0 <- elections_df_long$wkr_nstotal0 - elections_df_long$wkr_nsseats0*60000

elections_df_long$wkr_nsnext0 <- elections_df_long$wkr_nsrest0/60000 
elections_df_long$wkr_nslast0 <- (60000-elections_df_long$wkr_nsrest0)/60000
elections_df_long$wkr_nslast0[elections_df_long$wkr_nsseats0==0] <- NA
elections_df_long$wkr_comp_last <-  apply(elections_df_long[,c("wkr_nsnext0", "wkr_nslast0")], 1, max, na.rm = TRUE)
elections_df_long$wkr_comp_last[is.infinite(elections_df_long$wkr_comp_last)] <- NA

# Cox' (1999) interaction
elections_df_long$l_p_nsdapXwkr_comp <- elections_df_long$l_p_nsdap * elections_df_long$wkr_comp_last

# secondary districts (Wahlkreisverbaende)
elections_df_long$wkv <- elections_df_long$wkr %>% recode("1=1;c(2,3)=2;c(4,5)=3;c(6,35)=4;c(7,8,9)=5;c(10,11,12)=6;c(13,34)=7;c(14,15,16)=8;c(17,18)=9;c(19,33)=10;c(20,21)=11;c(22,23)=12;c(24,25)=13;c(26,27)=14;c(28,29,30)=15;c(31,32)=16")

wkv_nsrest0_df <- aggregate(wkr_nsrest0~wkv+election, data=elections_df_long, sum, na.rm=TRUE)
wkv_nsrest0_df <- rename.vars(wkv_nsrest0_df, "wkr_nsrest0", "wkv_nsrest0")
elections_df_long <- merge(elections_df_long, wkv_nsrest0_df, by = c("wkv", "election"), all.x = TRUE)
elections_df_long$wkv_nsseats0 <- floor(elections_df_long$wkv_nsrest0/60000)
elections_df_long$wkv_comp_last <- (60000 - (elections_df_long$wkv_nsrest0 - elections_df_long$wkv_nsseats0*60000))/60000


# generate competitiveness of current election variables -------------

# primary districts
nsdap_df <- aggregate(nsdap~wkr+election, data=elections_df_long, sum, na.rm=TRUE)
nsdap_df <- rename.vars(nsdap_df, "nsdap", "wkr_nstotal")
elections_df_long <- merge(elections_df_long, nsdap_df, by = c("wkr", "election"), all.x = TRUE)

elections_df_long$wkr_nsseats <- floor(elections_df_long$wkr_nstotal/60000)
elections_df_long$wkr_nsrest <- elections_df_long$wkr_nstotal - elections_df_long$wkr_nsseats*60000

elections_df_long$wkr_nsnext <- elections_df_long$wkr_nsrest/60000
elections_df_long$wkr_nslast <- (60000 - elections_df_long$wkr_nsrest)/60000
elections_df_long$wkr_nslast[elections_df_long$wkr_nsseats==0] <- NA

elections_df_long$wkr_comp_curr <-  apply(elections_df_long[,c("wkr_nsnext", "wkr_nslast")], 1, max, na.rm = TRUE)
elections_df_long$wkr_comp_curr[is.infinite(elections_df_long$wkr_comp_curr)] <- NA

# secondary districts (Wahlkreisverbaende)

wkv_nsrest_df <- aggregate(wkr_nsrest~wkv+election, data=elections_df_long, sum, na.rm=TRUE)
wkv_nsrest_df <- rename.vars(wkv_nsrest_df, "wkr_nsrest", "wkv_nsrest")
elections_df_long <- merge(elections_df_long, wkv_nsrest_df, by = c("wkv", "election"), all.x = TRUE)

elections_df_long$wkv_nsseats <- floor(elections_df_long$wkv_nsrest/60000)
elections_df_long$wkv_comp_curr <- (60000 - (elections_df_long$wkv_nsrest - elections_df_long$wkv_nsseats*60000))/60000



# generate other variables --------------------------------------

# local NSDAP members in 1,000
elections_df_long$memberst <- elections_df_long$members/1000

# number of eligibles (in 1,000), also polynomials
elections_df_long$wbht <- elections_df_long$wb/1E5 # eligibles in 100,000
elections_df_long$wbht2 <- elections_df_long$wbht^2 # eligibles in 100,000 square
elections_df_long$wbht3 <- elections_df_long$wbht^3 # eligibles in 100,000 cube
elections_df_long$lwb <- log(elections_df_long$wb) # log eligibles
elections_df_long$airfields_dist_min100km <- elections_df_long$airfields_dist_min/100 # distance to nearest airfield in 100km
elections_df_long$members_perc <- (elections_df_long$members / elections_df_long$wb) * 100 # members/eligibles

# identity var
elections_df_long$identity <- 1 # used for equal weighting



# deal with item missingness on visit variables ---------------------------------

# missingness on treatment variables: set to zero (= no visit)
iffer <- str_detect(colnames(elections_df_long), "^visit_|^esize_|^vcount_")
elections_df_long[,iffer] <- sapply(elections_df_long[,iffer], function(x) ifelse(is.na(x),0,x))


# drop unneeded variables ---------------------------------
iffer <- !str_detect(colnames(elections_df_long), "0$|^l[[:digit:]]")
elections_df_long <- elections_df_long[,iffer]

iffer <- !str_detect(colnames(elections_df_long), "wp1|dbpx1|dlvx1|vrp1|x1|dstp2|wp2|dlvx2|kvp2|csvd2|x2|dlvx3|dstp3|wp3|csvd3|x3|dstp4|csvd4|wp4|vb4|x4|kf5|csvd5|dbpx5|dhp5|dstp5|x5|x6|x7|agg2024|agg2424|agg2428|agg3033|agg2833|agg3032|agg3232|agg3233")
elections_df_long <- elections_df_long[,iffer]


# drop cases from 1924 election ----------------------------
elections_df_long <- elections_df_long[elections_df_long$election != 0,]




# create different data frames for analysis ----------------

# inspect population size at various aggregation levels
sum(elections_df_long$pop[elections_df_long$election == 2 & elections_df_long$agglvl != "KREISE M.GEMEINDEN >"], na.rm = TRUE) # total eligible population
sum(elections_df_long$pop[elections_df_long$election == 2 & elections_df_long$agglvl == "stadtkreise"], na.rm = TRUE)
sum(elections_df_long$pop[elections_df_long$election == 2 & elections_df_long$agglvl == "KREISE M.GEMEINDEN >"], na.rm = TRUE)
sum(elections_df_long$pop[elections_df_long$election == 2 & elections_df_long$agglvl == "KREISE O.GEMEINDEN >"], na.rm = TRUE)
sum(elections_df_long$pop[elections_df_long$election == 2 & elections_df_long$agglvl == "GEMEINDEN AB 2000 E."], na.rm = TRUE)
sum(elections_df_long$pop[elections_df_long$election == 2 & elections_df_long$agglvl == "RESTKREISE (GEM.< 20"], na.rm = TRUE)

# community-level data frame for May 1928 and Sep 1930 election
# drop "KREISE M.GEMEINDEN", as they merely report the sum of "GEMEINDEN AB 2000 E." and "RESTKREISE (GEM. < 2000 E.)"
community_df_long <- filter(elections_df_long, str_detect(agglvl, "stadtkreise|GEMEINDEN AB 2000 E\\.|KREISE O\\.GEMEINDEN|RESTKREISE \\(GEM\\.< 20")) 
community_df_long <- filter(community_df_long, election == 1 | election == 2)
sum(community_df_long$pop[community_df_long$election == 2], na.rm = TRUE)

# Kreis-level data frame for May 1928, Sep 1930, Jul 1932, Nov 1932, Mar 1933 elections
# drop "GEMEINDEN AB 2000 E" and "RESTKREISE \\(GEM\\.< 20" because election results for 1932 elections are unavailable for these units
county_df_long <- filter(elections_df_long, str_detect(agglvl, "stadtkreise|KREISE M.GEMEINDEN >|KREISE O\\.GEMEINDEN")) 
county_df_long <- filter(county_df_long, election >= 1 &  election <= 5)
sum(county_df_long$pop[county_df_long$election == 2], na.rm = TRUE)

# Kreis-level data frame for Mar and Apr 1932 Presidential election
county_pres_df_long <- filter(elections_df_long, str_detect(agglvl, "stadtkreise|KREISE M.GEMEINDEN >|KREISE O\\.GEMEINDEN"))
county_pres_df_long <- filter(county_pres_df_long, election == 6 | election == 7)
sum(county_pres_df_long$pop[county_pres_df_long$election == 7], na.rm = TRUE)
county_pres_df_long$wkr_nslast <- NULL # delete Competitiveness variable; causes missingness problems and is not necessary in this model


# drop election-wise incomplete cases --------------
community_df_long_complete_list <- list()
for (i in 1:2) {
	dat_filter <- filter(community_df_long, election == i)
	if(i == 2) {community_df_long_complete_list[[i]] <- dat_filter[complete.cases(dat_filter),] # only elections where the outcome is considered
	}else{
	  community_df_long_complete_list[[i]] <- dat_filter 
	}
	
}
community_df_long <- do.call(rbind.fill, community_df_long_complete_list)
dim(community_df_long)
table(community_df_long$election)

county_df_long_complete_list <- list()
for (i in 1:5) {
	dat_filter <- filter(county_df_long, election == i)
	if (i > 1) { county_df_long_complete_list[[i]] <- dat_filter[complete.cases(dat_filter),]
	}else{
	  county_df_long_complete_list[[i]] <- dat_filter 
	}
}
county_df_long <- do.call(rbind.fill, county_df_long_complete_list)
dim(county_df_long)
table(county_df_long$election)


county_pres_df_long_complete_list <- list()
for (i in 6:7) {
	dat_filter <- filter(county_pres_df_long, election == i)
	if (i == 7) { county_pres_df_long_complete_list[[i]] <- dat_filter[complete.cases(dat_filter),]
	}else{
	  county_pres_df_long_complete_list[[i]] <- dat_filter
	}
}
county_pres_df_long <- do.call(rbind.fill, county_pres_df_long_complete_list)
dim(county_pres_df_long)
table(county_pres_df_long$election)


# export data frames ------------------------------

community_df_long <- arrange(community_df_long, krnr, lfnr, election)
county_df_long <- arrange(county_df_long, krnr, lfnr, election)
county_pres_df_long <- arrange(county_pres_df_long, krnr, lfnr, election)


# RData export
save(community_df_long, file = "community_df_pointdata_long.RData")
save(county_df_long, file = "county_df_pointdata_long.RData")
save(county_pres_df_long, file = "county_pres_df_pointdata_long.RData")


# Stata export
write.dta(community_df_long, file = "community_df_pointdata_long.dta", version = 10)
write.dta(county_df_long, file = "county_df_pointdata_long.dta", version = 10)
write.dta(county_df_long, file = "county_pres_df_pointdata_long.dta", version = 10)

# export full data frame
elections_df_long <- arrange(elections_df_long, krnr, lfnr, election)
table(elections_df_long$election)
save(elections_df_long, file = "elections_all_df_pointdata_long.RData")

