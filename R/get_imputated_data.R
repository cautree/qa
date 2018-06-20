#' A get_imputated_data function
#'
#' This function allows you to imputate the data, get rid of NA
#' @param input_df, input data frame.
#' @return a r dataframe without NA values
#' @export
#'

get_imputated_data = function(  input_df) {



  df_info = input_df[, c(1:3)]
  rownames(df_info ) = df_info $plate_well
  df_info $plate_well = NULL



  df_data = input_df[, c(1, 4:dim(input_df)[2])]
  rownames(df_data ) = df_data $plate_well
  df_data $plate_well = NULL

  # replace NA with 0.25 of min
  NA2mean <- function(x) replace(x, is.na(x), min(x, na.rm = TRUE)*0.25)
  df_data_new<- lapply(df_data, NA2mean)

  df_data_new =data.frame(Reduce(cbind, df_data_new))

  colnames(df_data_new) =colnames(df_data)

  res_df = merge(df_info, df_data, by=0)

  res_df$plate_well = res_df$Row.names

  res_df$Row.names = NULL

  res_df = res_df %>%
    dplyr::select(plate_well, subjectId, year, everything())

  return(res_df)


}
