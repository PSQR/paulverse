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
