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
#' This function takes a random sample of your data based on inputted by variables.  Requires dplyr
#'
#' @param df the name of your dataframe
#' @param byvars the by variables you wish to sample by
#' @param num the number of samples to return
#' @return subsetted dataframe
#' @examples
#' rs_by_var_2(df = mtcars, byvars = c("cyl", "carb"), num = 2)
#' @export
rs_by_var_2 <- function(df, byvars, num){

  df_2 <- as.data.frame(df[,byvars])

    if(length(byvars) < 2){

      names(df_2) <- byvars

    }

    df_3 <- unique(df_2)

    df_4 <- dplyr::sample_n(df_3, num)

    df_5 <- inner_join(df, df_4)

    return(df_5)

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

save_time_stamped <- function(df, the_file_path, the_time_stamp, row.names = F){

  the_list <- unlist(strsplit(the_file_path, "\\."))

  directory <- the_list[1]

  extension <- the_list[2]

  if (toupper(extension) == "RDS"){

    saveRDS(df, file = the_file_path)

    saveRDS(df, file = paste0(directory, "_", the_time_stamp, "." , tolower(extension)))

  } else if(toupper(extension) == "RDA"){

    save(df, file = the_file_path)

    save(df, file = paste0(directory, "_", the_time_stamp, "." , extension))

  } else if(toupper(extension) == "CSV"){

    write.csv(df, file = the_file_path, row.names = row.names)

    write.csv(df, file = paste0(directory, "_", the_time_stamp, "." , tolower(extension)), row.names = row.names)

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

#' get_fisc_wk
#'
#' This function returns current week of the fiscal period
#'
#' @return integer 1-5 current fiscal week of period
#' @export

get_fisc_wk <- function(){

  current_day <- as.data.frame(subset(inf_dt, as.Date(inf_dt$CLDR_DT) == Sys.Date()))

  return(as.integer(current_day$FCL_WK_OF_PER_N))

}

#' analyze_logs
#'
#' This function reads every txt of log file in directory and returns when the string "ERROR" was found
#'
#' @param the name of your directory
#' @return dataframe
#' @examples
#' tmp_df <- analyze_logs(directory = "/opt/R/R_project/pperrin/2020/SOE_Rebuild/Logs/202110_1/stderr/"))
#' @export

analyze_logs <- function(directory){

  processFile = function(filepath) {

    error_flag = 0

    con = file(filepath, "r")
    while ( TRUE ) {
      line = readLines(con, n = 1)
      if ( length(line) == 0 ) {
        break
      }

      #print(toupper(toString(line)))

      #print((grepl(toupper(toString(line)), "ERROR", fixed = TRUE) == T))

      if((grepl(pattern = "ERROR", x = toupper(toString(line)), fixed = TRUE) == T) & (toString(nchar(line)) > 1)){

        #print(paste0(file_list[i], " contains an error"))

        error_flag = 1

      }

    }

    close(con)

    return(data.frame("Program" = file_list[i], "Error_Flag" = error_flag))

  }

  #processFile(filepath = 'D:\\temp\\temp_logs\\1_hst_style_week_stderr_2020_11_09_00_01.log')

  #### Loop Through Logs ####

  the_directory = directory

  file_list = list.files(the_directory)

  for (i in 1:length(file_list)){

    #print(i)

    #print(file_list[i])

    #paste0(the_directory, file_list[i])

    if(i == 1){

      out_df <- processFile(filepath = paste0(the_directory, file_list[i]))

    } else{

      out_df <- bind_rows(out_df, processFile(filepath = paste0(the_directory, file_list[i])))

    }

    #processFile(filepath = paste0(the_directory, file_list[i]))

  }

  return(out_df)

}

#' per_error
#'
#' Computes percent error
#'
#' @param df the name of your df
#' @param predict_var the name of your prediction
#' @param actual_var the name of your actual
#' @return dataframe
#' @examples
#' bckt_accrcy_02 <- per_error(df = bckt_accrcy_01, predict_var = predict, actual_var = SL_QT)
#' @export

per_error <- function(df, predict_var, actual_var){

  df$predict_temp <- eval(substitute(predict_var), df)

  df$actual_temp <- eval(substitute(actual_var), df)

  temp_df <- df

  temp_df$ERROR <- temp_df$predict_temp - temp_df$actual_temp

  temp_df$ERROR_PCT <- temp_df$ERROR / temp_df$actual_temp

  temp_df$ERROR_PCT <- ifelse(temp_df$actual_temp == 0 & abs(round(temp_df$predict_temp)) > 0, 1, temp_df$ERROR_PCT)

  temp_df$ERROR_PCT <- ifelse(temp_df$actual_temp == 0 & abs(round(temp_df$predict_temp)) == 0, 0, temp_df$ERROR_PCT)

  temp_df$predict_temp <- NULL

  temp_df$actual_temp <- NULL

  #Capping individual error at 100%

  temp_df$ERROR_PCT <- ifelse(temp_df$ERROR_PCT > 1, 1, temp_df$ERROR_PCT)

  return(temp_df)

}

#' cal_to_fisc_wk_bgn_dt
#'
#' returns begin date of current fiscal week
#'
#' @return string
#' @examples
#' cal_to_fisc_wk_bgn_dt()
#' @export

cal_to_fisc_wk_bgn_dt <- function ()
{
  current_day <- as.data.frame(subset(inf_dt, as.Date(inf_dt$CLDR_DT) ==
                                        Sys.Date()))
  return(gsub("-", "", current_day$FCL_WK_BGN_DT))
}

#' cal_to_fisc_wk_end_dt
#'
#' returns begin date of current fiscal week
#'
#' @return string
#' @examples
#' cal_to_fisc_wk_end_dt()
#' @export

cal_to_fisc_wk_end_dt <- function ()
{
  current_day <- as.data.frame(subset(inf_dt, as.Date(inf_dt$CLDR_DT) ==
                                        Sys.Date() + 7))
  return(gsub("-", "", current_day$FCL_WK_BGN_DT))
}

#' Chrctr_Crrnt_Dttme
#'
#' returns date time in a string seperated by underscores
#'
#' @return string
#' @examples
#' Chrctr_Crrnt_Dttme()
#' @export

Chrctr_Crrnt_Dttme <- function(){

  date_time_str <- gsub('-','_',Sys.time())

  date_time_str <- gsub(' ','_', date_time_str)

  date_time_str <- gsub(':','_', date_time_str)

  dt_format_NUM <- function(start_pos, stop_pos){

    as.numeric(substr(date_time_str, start_pos, stop_pos))

  }

  dt_format_CHAR <- function(start_pos, stop_pos){

    substr(date_time_str, start_pos, stop_pos)

  }

  return(paste0(dt_format_NUM(start_pos = 6, stop_pos = 7), "/",
                dt_format_NUM(start_pos = 9, stop_pos = 10), "/",
                dt_format_NUM(start_pos = 1, stop_pos = 4), " ",
                dt_format_CHAR(start_pos = 12, stop_pos = 13), ":",
                dt_format_CHAR(start_pos = 15, stop_pos = 16)))

}

#' arrange.vars
#'
#' returns date time in a string seperated by underscores
#' @param data the name of your df
#' @param vars the names of your variables
#' @return df
#' @examples
#' bckt_intl_bckt_frcsts <- arrange.vars(data = forecasts_2, vars = c("RGN" = 1,"P_STYL_CD" = 2,"PLNT_CD" = 3))
#' @export
#'

arrange_vars <- function(data, vars){
  ##stop if not a data.frame (but should work for matrices as well)
  stopifnot(is.data.frame(data))

  ##sort out inputs
  data.nms <- names(data)
  var.nr <- length(data.nms)
  var.nms <- names(vars)
  var.pos <- vars
  ##sanity checks
  stopifnot( !any(duplicated(var.nms)),
             !any(duplicated(var.pos)) )
  stopifnot( is.character(var.nms),
             is.numeric(var.pos) )
  stopifnot( all(var.nms %in% data.nms) )
  stopifnot( all(var.pos > 0),
             all(var.pos <= var.nr) )

  ##prepare output
  out.vec <- character(var.nr)
  out.vec[var.pos] <- var.nms
  out.vec[-var.pos] <- data.nms[ !(data.nms %in% var.nms) ]
  stopifnot( length(out.vec)==var.nr )

  ##re-arrange vars by position
  data <- data[ , out.vec]
  return(data)
}

#' process_CSP_Override
#'
#' This code is used in both the components forecasting and S&OE AMER.
#' @param forecast_type either COMP or CDW
#' @return df
#' @examples
#' csp_final <- process_CSP_Override(forecast_type = "COMP")
#' @export
#'

process_CSP_Override <- function(forecast_type) {

  if (nrow(CSP_OVERRIDE) > 0) {
    out_df <- left_join(csp_init_01, CSP_OVERRIDE, by = c("material_code",
                                                          "Fiscal_Period"))
    names(out_df)[names(out_df) == "Override / Lift / Percentage"] <- "Override_Lift_Percentage"
    names(out_df)[names(out_df) == "Comp / CDW / Both"] <- "Comp_CDW_Both"
    names(out_df)[names(out_df) == "FINAL_QTY"] <- "Final_Qty"
    out_df$Override_Lift_Percentage <- ifelse(is.na(out_df$Override_Lift_Percentage),
                                              "DO_NOTHING", out_df$Override_Lift_Percentage)
    out_df$Final_Qty <- ifelse(toupper(out_df$Override_Lift_Percentage) ==
                                 "LIFT" & (toupper(out_df$Comp_CDW_Both) == toupper(forecast_type) | toupper(out_df$Comp_CDW_Both) == "BOTH"),
                               out_df$Final_Qty + out_df$`CSP Current Plan`, out_df$Final_Qty)
    out_df$Final_Qty <- ifelse(toupper(out_df$Override_Lift_Percentage) ==
                                 "OVERRIDE" & (toupper(out_df$Comp_CDW_Both) == toupper(forecast_type) | toupper(out_df$Comp_CDW_Both) == "BOTH"),
                               out_df$`CSP Current Plan`, out_df$Final_Qty)
    out_df$Final_Qty <- ifelse(toupper(out_df$Override_Lift_Percentage) ==
                                 "PERCENTAGE" & (toupper(out_df$Comp_CDW_Both) == toupper(forecast_type) | toupper(out_df$Comp_CDW_Both) == "BOTH"),
                               (out_df$`CSP Current Plan` * out_df$Final_Qty) +
                                 (out_df$Final_Qty), out_df$Final_Qty)
    write_csv(out_df, "/opt/sasshare/Projects_Folders/SOP_Files/SP_Data/Manual_CSP_adjustments_PROCESSED.csv")
    out_df$ROW_COUNT <- NULL
    out_df$`CSP Current Plan` <- NULL
  }
  else {
    print("No data in CSP_OVERRIDE file")
    out_df <- csp_init_01
  }
  return(out_df)

}

#' better_summary
#'
#' It's just a better summary
#' @param df your dataframe
#' @examples
#' mtcars %>% better_summary()
#' @export
#'

better_summary <- function(df){
  
  temp_df <- df
  
  char_cols <- sapply(df, is.character)
  
  temp_df[char_cols] <- lapply(temp_df[char_cols], as.factor)
  
  print(paste("nrow", temp_df %>% nrow()))
  
  return(summary(temp_df))
  
}