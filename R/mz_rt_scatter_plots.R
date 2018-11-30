
get_mz_ = function(s){

  b=tidyr::extract_numeric(s)
  b
  b= a[[1]][3]
  return(b)
}

get_rt_ = function(s){
  a=strsplit(s,"_")
  b= a[[1]][4]
  return(b)
}


get_group_ = function(s){
  a=strsplit(s,"_")
  b= a[[1]][1]
  return(b)
}

#' create mz_rt scatter plot
#'
#' This function loads a file as a dataframe.
#' @param input_df, input data frame.
#' @param infile Path to the input file
#' @param df_name, name for the input data, default is "Vital"
#' @return a ggplot scatter plot
#' @export
#'

create_mz_rt_scatter_plots = function(input_df, df_name="Vital") {



  df_data = input_df[,grep("mzid",colnames(input_df))]


  df_mzrt = as.data.frame(colnames(df_data))


  df_mzrt = df_mzrt %>%
    dplyr::mutate(mz=sapply(as.character(df_mzrt$`colnames(df_data)`), get_mz_) ) %>%
    dplyr::mutate(rt = sapply(as.character(df_mzrt$`colnames(df_data)`), get_rt_) ) %>%
    dplyr::mutate(group=sapply(as.character(df_mzrt$`colnames(df_data)`), get_group_) ) %>%
    dplyr::mutate(mz= as.numeric(mz)) %>%
    dplyr::mutate(rt = as.numeric(rt))


  df_mzrt %>%
    ggplot2::ggplot(aes(mz, rt, color = group))  +
    ggplot2::geom_point()+
    ggplot2::ggtitle("scatter plot of mz and rt")

  ggplot2::ggsave( paste(df_name, "mz_rt_scatter_plot.pdf", sep=" ") )
}
