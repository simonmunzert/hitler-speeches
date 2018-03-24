#----------------------------------------------------------------
# Examining a Most Likely Case for Strong Campaign Effects: 
# Hitler's Speeches and the Rise of the Nazi Party, 1927--1933
# Peter Selb (University of Konstanz)
# Simon Munzert (MZES, University of Mannheim)
# 2016
#----------------------------------------------------------------


# function to generalize matching procedure given treatment variable, covariate names, and original data frame -- useful for sensitivity analysis 
performMatch <- function(treat.var, dat.raw, electiontype = c("county", "pres", "comm"), goebbels.var = TRUE, goebbels.adapt = TRUE, exactmatch = NULL, treat.var.red = TRUE) {
  set.seed(42)
  dat_ps <- get(dat.raw)
  covar.names <- c("memberst", "wbht", "airfields_dist_min100km")
  if(electiontype == "county") {
    addvars <- c("l_p_nsdap", "wkr_comp_last", "wkv_comp_last")
  }else if(electiontype == "pres") {
    addvars <-   "l_p_hitl"
  }else{
    addvars <- c("l_p_nsdap_comm", "wkr_comp_last", "wkv_comp_last")
  }
  if (goebbels.var == TRUE & goebbels.adapt == TRUE) {
    goebbels_var <- paste0("goebbels", str_extract(treat.var, "_[[:digit:]].+$"))
  }else if(goebbels.var == TRUE){
    goebbels_var <- "goebbels_10km"
  }else{
    goebbels_var <- NULL
  }
  if (treat.var.red == TRUE) {
   treat.var.red <- str_replace(treat.var, "_[[:digit:]]{1,2}w", "")
  }else{
    treat.var.red <- "visit_10km"
  }
  covar_names <- c(covar.names, addvars, goebbels_var, paste0("l_", treat.var.red))
  model.formula <- paste(treat.var, " ~ ", paste(covar_names, collapse=" + "),sep = "") %>% as.formula
  rownames(dat_ps) <- 1:nrow(dat_ps)
  model.out <- matchit(formula = model.formula, 
                       data = dat_ps,
                       method = "nearest", 
                       m.order = "random",
                       ratio = 1,
                       caliper = .25,
                       replace = FALSE,
                       discard = "none",
                       exact = c(paste0(treat.var, "_nomatch"), exactmatch),
                       distance = "probit") 
  match.data.out <- match.data(model.out)
  return(match.data.out)
}


# function to create matching data frame
matchDatFunMatrix <- function(i, dat) {
  if (dat == "county_pres_df_long") {
    dat <- get(dat)
    dat <- filter(dat, election == 7)
    dat$election <- 6
    rownames(dat) <- 1:nrow(dat)
  }else if (dat == "community_df_long"){
    dat <- get(dat)
    dat <- filter(dat, election == 2)
    dat$election <- 7
    rownames(dat) <- 1:nrow(dat)
  } else {
    dat <- get(dat)
    dat <- filter(dat, election == i+1)
    rownames(dat) <- 1:nrow(dat)    
  }
  match_dat <- data.frame(id_var = rownames(dat[dat$election == i+1,]), treated = model.out.list[[i]]$treat, discarded = model.out.list[[i]]$discarded, distance = model.out.list[[i]]$distance)
  # create buffer variable
  treat_var <- model.out.list[[i]]$formula %>% char() %>% .[2]
  buffer_var <- paste0(treat_var, "_nomatch")
  match_dat$buffer <- dat[dat$election == i+1,][,buffer_var]  
  # create match matrix
  match.matrix.full <- data.frame(id_var = c(rownames(model.out.list[[i]]$match.matrix), model.out.list[[i]]$match.matrix), id_var_match = c(model.out.list[[i]]$match.matrix, rownames(model.out.list[[i]]$match.matrix)))
  # create id variable from matched observation
  match_dat <- merge(match_dat, match.matrix.full, by = "id_var", all.x = TRUE)
  match_dat$id_var <- num(char(match_dat$id_var))
  match_dat <- arrange(match_dat, id_var)
  # create distance variable of match
  match_dat$distance_match <- NA
  for (j in 1:nrow(match_dat)) {
    id_match <- match_dat$id_var_match[j] %>% char() %>% num()
    match_dat$distance_match[j] <- ifelse(id_match != -1, match_dat$distance[match_dat$id_var == id_match], NA)
  }
  # create matched variable
  match_dat$matched <- !is.na(match_dat$distance_match)    
  # return df
  return(match_dat)
}


# function to get full sample
getDatFull <- function(dat, election, treatment){
  dat_full <- get(dat)
  dat_full <- dat_full[dat_full$election == election - 1 | dat_full$election == election,]
  obs <- dat_full$lfnr[dat_full$election == election]
  dat_full <- dat_full[dat_full$lfnr %in% obs,]
  dat_full$time <- ifelse(dat_full$election == election, 1, 0)
  dat_full$exposure <- ifelse(dat_full$lfnr %in% dat_full$lfnr[dat_full[,treatment] & dat_full$time ==1], 1, 0)
  dat_full$timeXexposure <- dat_full$time * dat_full$exposure
  return(dat_full)
}

# function to get matched sample
getDatMatch <- function(dat.match, dat.orig, election, treatment){
  dat_match <- get(dat.match)
  dat_orig <- get(dat.orig)
  dat_match_0 <- dat_orig[dat_orig$lfnr %in% dat_match$lfnr & dat_orig$election == election-1,]
  dat_match_0$distance <- NA
  dat_match_0$weights <- NA
  dat_match_full <- rbind.fill(dat_match, dat_match_0)
  single_obs <- dat_match_full$lfnr[table(dat_match_full$lfnr) == 1]
  dat_match_full <- filter(dat_match_full, !(lfnr %in% single_obs)) # drop single case observations (i.e.)
  dat_match_full$weights <- rep(dat_match_full$weights[!is.na(dat_match_full$weights)], 2)
  dat_match_full$time <- ifelse(dat_match_full$election == election, 1, 0)
  dat_match_full$exposure <- ifelse(dat_match_full$lfnr %in% dat_match_full$lfnr[dat_match_full[,treatment] & dat_match_full$time ==1], 1, 0)
  dat_match_full$timeXexposure <- dat_match_full$time * dat_match_full$exposure
  return(dat_match_full)
}


getCoef <- function(model) {
  model <- get(model)
  model.df <- summary(model)$df[2]
  coef_point <- coef(model)['timeXexposure']
  coef_se <- model$clusterse['timeXexposure',]["Std. Error"]
  coef_weaklo <- coef_point - qt(.10, model.df, lower.tail = FALSE) * coef_se 
  coef_weakhi <- coef_point + qt(.10, model.df, lower.tail = FALSE) * coef_se 
  coef_hardlo <- coef_point - qt(.025, model.df, lower.tail = FALSE) * coef_se 
  coef_hardhi <- coef_point + qt(.025, model.df, lower.tail = FALSE) * coef_se 
  df.out <- data.frame(coef_point = coef_point, coef_weaklo = coef_weaklo, coef_weakhi = coef_weakhi, coef_hardlo = coef_hardlo, coef_hardhi = coef_hardhi)
  return(df.out)
}