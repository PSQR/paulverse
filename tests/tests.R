#### libraries ####

library(readr)
library(dplyr)

#### Load your new inf_dt to test ####

#load("/opt/R/R_project/pperrin/paulverse/data/inf_dt.rda")

load("/opt/R/R_project/pperrin/inf_dt.rda")

inf_dt_old <- inf_dt

inf_dt <- read_csv("/opt/R/R_project/pperrin/inf_dt_test_2024_07_29.csv")

#### Manipulate new inf_dt so that it passes tests ####

inf_dt$FCL_YR_PER_C <- substr(as.character(inf_dt$FCL_YR_PER_C),1,7)

inf_dt$FCL_WK_OF_PER_N <- inf_dt$FCL_WK_OF_PER_C

inf_dt$FCL_PER_OF_YR_N <- as.integer(inf_dt$FCL_PER_OF_YR_C)

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

#### sumaries ####

summary(inf_dt$CLDR_DT)
summary(inf_dt_old$CLDR_DT)

summary(inf_dt$FCL_PER_OF_YR_N)
summary(inf_dt_old$FCL_PER_OF_YR_N)

#### Once they all pass, save new inf_dt ####

#save old copy

save(inf_dt_old,file = paste0("/opt/R/R_project/pperrin/", "inf_dt_backup_", Sys.Date(), "rda"), compress = T)

#save new inf_dt

save(inf_dt, file="/opt/R/R_project/pperrin/paulverse/data/inf_dt.rda")
