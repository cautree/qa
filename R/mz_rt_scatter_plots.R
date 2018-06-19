
get_mz_ = function(s){
  a=strsplit(s,"_")
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

#' Load a df
#'
#' This function loads a file as a matrix. It assumes that the first column
#' contains the rownames and the subsequent columns are the sample identifiers.
#' Any rows with duplicated row names will be dropped with the first one being
#' kepted.
#'
#' @param infile Path to the input file
#' @return A matrix of the infile
#' @export

create_mz_rt_scatter_plots = function(input_df) {


  df_data = input_df[,grep("mzid",colnames(input_df))]


  df_mzrt = as.data.frame(colnames(df_data))


  df_mzrt = df_mzrt %>%
    mutate(mz=sapply(as.character(df_mzrt$`colnames(df_sample)`), get_mz_) ) %>%
    mutate(rt = sapply(as.character(df_mzrt$`colnames(df_sample)`), get_rt_) ) %>%
    mutate(group=sapply(as.character(df_mzrt$`colnames(df_sample)`), get_group_) ) %>%
    mutate(mz= as.numeric(mz)) %>%
    mutate(rt = as.numeric(rt))


  df_mzrt %>%
    ggplot(aes(mz, rt, color = group))  +
    geom_point()+
    ggtitle("scatter plot of mz and rt")

  df_mzrt
}
