#' rs_by_var 
#' This function takes a random sample of your data based on inputted by variables.  Requires SQLDF

#' @param df the name of your dataframe
#' @param byvars the by variables you wish to sample by
#' @param num the number of samples to return
#' @return subsetted dataframe 
#' @export
rs_by_var <- function(df, byvars, num){

  fun_df <- df
  
  temp <- sqldf(paste0("
                       
                       SELECT DISTINCT
                       
                       ", byvars, " 
                       
                       FROM fun_df
                       
                       "))
  
  temp_2 <- temp[sample(nrow(temp), num), ]
  
  temp_3 <- sqldf(paste0("SELECT * FROM fun_df JOIN temp_2 USING(", byvars, ")"))
  
  return(temp_3)
  
}

#' cal_to_fisc_per

#' @export

cal_to_fisc_per <- function(){
  
  load(file = "/opt/R/R_project/pperrin/2020/SOP_Rebuild_w_Danny/Input_Data/inf_dt.Rda")
  
  current_day <- as.data.frame(subset(inf_dt, as.Date(inf_dt$CLDR_DT) == Sys.Date()))
  
  return(gsub("-", "", current_day$FCL_YR_PER_C))
  
}

#' cal.to.fisc.wk

#' @export

cal_to_fisc_wk <- function(){
  
  load(file = "/opt/R/R_project/pperrin/2020/SOP_Rebuild_w_Danny/Input_Data/inf_dt.Rda")
  
  current_day <- as.data.frame(subset(inf_dt, as.Date(inf_dt$CLDR_DT) == Sys.Date()))
  
  return(current_day$FCL_WK_BGN_DT)
  
}

#' save.time.stamped

#' @export

save_time_stamped <- function(df, the_file_path, the_time_stamp){
  
  save(df, file = the_file_path)
  
  the_list <- unlist(strsplit(the_file_path, "\\."))
  
  directory <- the_list[1]
  
  extension <- the_list[2]
  
  save(df, file = paste0(directory, "_", the_time_stamp, "." , extension))
  
}