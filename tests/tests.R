#### libraries ####

library(readr)
library(dplyr)

#### Save CSV file to RDA file ####

update_inf_dt <- function(fp){

  #save a date stamped copy of current inf_dt

  load("/opt/R/R_project/pperrin/paulverse/data/inf_dt.rda")
  save(inf_dt, file=paste0("/opt/R/R_project/pperrin/paulverse/MISC/inf_dt_", Sys.time(), ".rda"))

}

update_inf_dt()

#### Load your new inf_dt to test ####

#load("/opt/R/R_project/pperrin/paulverse/data/inf_dt.rda")

load("/opt/R/R_project/pperrin/inf_dt_2025_08_13.rda")

inf_dt_old <- inf_dt

inf_dt <- read_csv("/opt/R/R_project/pperrin/inf_dt_thru_2100.csv")

#### add missing var ####

inf_dt_tmp <- inf_dt %>%
  arrange(CLDR_DT) %>%
  select(FCL_WK_BGN_DT, FCL_YR_PER_C) %>%
  group_by(FCL_YR_PER_C, FCL_WK_BGN_DT) %>%
  summarise(n = n())  %>%
  select(-n) %>%
  mutate(FCL_WK_OF_PER_C = as.character(row_number())) %>%
  ungroup()

inf_dt <- inf_dt %>%
  left_join(inf_dt_tmp, by = join_by(FCL_WK_BGN_DT, FCL_YR_PER_C))

#### Manipulate new inf_dt so that it passes tests ####

inf_dt$FCL_YR_PER_C <- substr(as.character(inf_dt$FCL_YR_PER_C),1,7)

inf_dt$FCL_WK_OF_PER_N <- inf_dt$FCL_WK_OF_PER_C

inf_dt$FCL_PER_OF_YR_N <- as.integer(inf_dt$FCL_PER_OF_YR_C)

inf_dt$FCL_PER_OF_YR_C <- as.character(inf_dt$FCL_PER_OF_YR_C)

inf_dt$FCL_PER_OF_YR_C <- ifelse(nchar(inf_dt$FCL_PER_OF_YR_C)==1,paste0("0",inf_dt$FCL_PER_OF_YR_C),inf_dt$FCL_PER_OF_YR_C)

inf_dt$FCL_WK_OF_PER_C <- as.character(inf_dt$FCL_WK_OF_PER_C)

#### Test functions locally ####

source("/opt/R/R_project/pperrin/paulverse/R/Production_Ready_Functions.R")

cal_to_fisc_per()

cal_to_fisc_wk()

get_ver(1)

get_fisc_wk()

cal_to_fisc_wk_bgn_dt()

cal_to_fisc_wk_end_dt()

#### OLD functions ####

cal_to_fisc_per_OLD <- function(){

  current_day <- as.data.frame(subset(inf_dt_old, as.Date(inf_dt_old$CLDR_DT) == Sys.Date()))

  return(gsub("-", "", current_day$FCL_YR_PER_C))

}

cal_to_fisc_per_OLD()

#

cal_to_fisc_wk_OLD <- function(){

  current_day <- as.data.frame(subset(inf_dt_old, as.Date(inf_dt_old$CLDR_DT) == Sys.Date()))

  return(current_day$FCL_WK_BGN_DT)
}

cal_to_fisc_wk_OLD()

#

get_ver_OLD <- function(week){

  current_day <- as.data.frame(subset(inf_dt_old, as.Date(inf_dt_old$CLDR_DT) == Sys.Date()))

  if (current_day$FCL_WK_OF_PER_N == week){

    ver = 1

  } else{

    ver = 0

  }

  return(ver)

}

get_ver_OLD(1)

#

get_fisc_wk_OLD <- function(){

  current_day <- as.data.frame(subset(inf_dt_old, as.Date(inf_dt_old$CLDR_DT) == Sys.Date()))

  return(as.integer(current_day$FCL_WK_OF_PER_N))

}

get_fisc_wk_OLD()

#

cal_to_fisc_wk_bgn_dt_OLD <- function ()
{
  current_day <- as.data.frame(subset(inf_dt_old, as.Date(inf_dt_old$CLDR_DT) ==
                                        Sys.Date()))
  return(gsub("-", "", current_day$FCL_WK_BGN_DT))
}

cal_to_fisc_wk_bgn_dt_OLD()

#

cal_to_fisc_wk_end_dt_OLD <- function ()
{
  current_day <- as.data.frame(subset(inf_dt_old, as.Date(inf_dt_old$CLDR_DT) ==
                                        Sys.Date() + 7))
  return(gsub("-", "", current_day$FCL_WK_BGN_DT))
}

cal_to_fisc_wk_end_dt_OLD()

##### Run Tests ####

cal_to_fisc_per()==cal_to_fisc_per_OLD()

cal_to_fisc_wk()==cal_to_fisc_wk_OLD()

for(i in 1:5){
  print(get_ver(i)==get_ver_OLD(i))

}

get_fisc_wk() == get_fisc_wk_OLD()

cal_to_fisc_wk_bgn_dt()==cal_to_fisc_wk_bgn_dt_OLD()

cal_to_fisc_wk_end_dt()==cal_to_fisc_wk_end_dt_OLD()

sort(unique(inf_dt$FCL_PER_OF_YR_N)) == sort(unique(inf_dt_old$FCL_PER_OF_YR_N))

sort(unique(inf_dt$FCL_PER_OF_YR_C)) == sort(unique(inf_dt_old$FCL_PER_OF_YR_C))

#### sumaries ####

summary(inf_dt$CLDR_DT)
summary(inf_dt_old$CLDR_DT)

summary(inf_dt$FCL_PER_OF_YR_N)
summary(inf_dt_old$FCL_PER_OF_YR_N)

#### Once they all pass, save new inf_dt ####

#save old copy

save(inf_dt_old,file = paste0("/opt/R/R_project/pperrin/", "inf_dt_backup_", Sys.Date(), "rds"))

#save new inf_dt

save(inf_dt, file="/opt/R/R_project/pperrin/paulverse/data/inf_dt.rda")
