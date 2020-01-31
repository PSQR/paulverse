#' rs_by_var 
#'
#' This function takes a random sample of your data based on inputted by variables.  Requires SQLDF
#'
#' @param df the name of your dataframe
#' @param byvars the by variables you wish to sample by
#' @param num the number of samples to return
#' @return subsetted dataframe
#' rs_by_var(df = mtcars, byvars = "cyl, carb", num = 2)
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

#' rs_by_var_2 
#'
#' This function takes a random sample of your data based on inputted by variables.  Requires SQLDF
#'
#' @param df the name of your dataframe
#' @param byvars the by variables you wish to sample by
#' @param num the number of samples to return
#' @return subsetted dataframe 
#' @examples
#' rs_by_var_2(df = mtcars, byvars = c("cyl", "carb"), num = 2)
#' @export
rs_by_var_2 <- function(df, byvars, num){
  
  df_2 <- df[,byvars]
  
  unique(df_2)
  
  df_3 <- df_2[sample(nrow(df_2), num), ]
  
  df_4 <- inner_join(df, df_3)
  
  return(df_4)
  
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

#' get_ver
#'
#' This function returns whether or not it is the specified week of the fiscal period
#'
#' @param week week of fiscal period (1, 2,3, or 4)
#' @return 0 or 1
#' @export

get_ver <- function(week){
  
  current_day <- as.data.frame(subset(inf_dt, as.Date(inf_dt$CLDR_DT) == Sys.Date()))
  
  if (current_day$FCL_WK_OF_PER_N == week){
    
    ver = 1
    
  } else{
    
    ver = 0
    
  }
  
  return(ver)
  
}

#' save_zip
#'
#' This function saves your output file as a zip file
#'
#' @param the_file output file you want to zip
#' @export

save_zip <- function(the_file){
    
    #print(the_file)
    
    the_list <- unlist(strsplit(the_file, "\\."))
    
    directory <- the_list[1]
    
    file <- the_list[2]
    
    system(paste0("zip ", directory, ".zip ", the_file))
    
    system(paste0("rm ", the_file))
    
  }
  
#' load_zip
#'
#' This function loads your output from a zip file
#'
#' @param the_file zipped output file you want to load
#' @param extension the extension of the file once unzipped
#' @export

load_zip <- function(the_file, extension){
    
    the_list <- unlist(strsplit(the_file, "\\."))
    
    directory <- the_list[1]
    
    file <- the_list[2]
    
    the_list <- unlist(strsplit(directory, "/"))
    
    list_length <- length(the_list)
    
    directory_2 <- paste0("/",the_list[2],"/")
    
    for (i in 3:(list_length-1)){
      
      #print(i)
      
      directory_2 <- paste0(directory_2, the_list[i], "/")
      
    }
    
    system(paste0("unzip -j ", the_file, " -d ", directory_2))
    
    #load(paste0(directory, extension))
    
    #Sleeping for 1 second seems to work.  Using 5 just to be safe.
    
    system(paste0("(sleep 5; rm ", directory, extension, ")"), intern = FALSE, wait = FALSE)
    
    #print("Hello, World!")
    
    return(paste0(directory, extension))
    
  }
