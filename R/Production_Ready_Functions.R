#' rs_by_var 
#'
#' This function takes a random sample of your data based on inputted by variables.  Requires SQLDF
#'
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
#'
#'This function returns current Steelcase fiscal period
#'
#'@return "YYYYMM"
#'@export

cal_to_fisc_per <- function(){
  
  current_day <- as.data.frame(subset(inf_dt, as.Date(inf_dt$CLDR_DT) == Sys.Date()))
  
  return(gsub("-", "", current_day$FCL_YR_PER_C))
  
}

#' cal_to_fisc_wk
#'
#'This function returns the beginning of the current Steelcase fiscal week
#'
#'@return "YYYY-mm-dd UTC"
#'@export

cal_to_fisc_wk <- function(){

  current_day <- as.data.frame(subset(inf_dt, as.Date(inf_dt$CLDR_DT) == Sys.Date()))
  
  return(current_day$FCL_WK_BGN_DT)
  
}

#' save_time_stamped
#'
#' This function saves a time stamped version of your output data set, in addtion to the data set itself
#'
#' @param df the name of your dataframe
#' @param the_file_path out file path
#' @param the_time_stamp value to timestamp your output with
#' @export

save_time_stamped <- function(df, the_file_path, the_time_stamp){
  
  the_list <- unlist(strsplit(the_file_path, "\\."))
  
  directory <- the_list[1]
  
  extension <- the_list[2]
  
  if (toupper(extension) == "RDS"){
    
    saveRDS(df, file = the_file_path)
    
    saveRDS(df, file = paste0(directory, "_", the_time_stamp, "." , extension))
    
  } else if(toupper(extension) == "RDA"){
  
  save(df, file = the_file_path)
  
  save(df, file = paste0(directory, "_", the_time_stamp, "." , extension))
  
  } else if(toupper(extension) == "CSV"){
    
    write.csv(df, file = the_file_path)
    
    write.csv(df, file = paste0(directory, "_", the_time_stamp, "." , extension))
    
  }
  
}
