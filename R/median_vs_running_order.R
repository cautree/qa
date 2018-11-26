#' A median_vs_running_order function
#'
#' This function allows you to get median vs running order
#' @param input_df, input data frame.
#' @param is_sample, boolean
#' @param df_name, name for the input data
#' @return a ggplot scatter graph
#' @export

get_median_vs_running_order = function(df_input,is_sample=TRUE, df_name="Vital") {

  df_sample <- df_input[,colSums(is.na(df_input))<nrow(df_input)]

  rownames(df_sample) =df_sample$plate_well
  df_sample$plate_well= NULL



  df_sample = as.data.frame(t(df_sample))

  row_median = as.data.frame( sapply(df_sample, median, na.rm = TRUE))

  names(row_median) ="row_median"

 median_df = row_median %>%
    dplyr::mutate(plate_well = rownames(.)) %>%
    tidyr::separate(plate_well, c("plate", "well"), convert=TRUE) %>%
    dplyr::mutate(order =(plate-1)*96+well )%>%
    dplyr::mutate(plate = as.factor(plate))

  median_df %>%
    ggplot2::ggplot(aes(order, row_median, color = plate)) +
    ggplot2::geom_point()+
    ggplot2::ylab("metabolite_median") +
    ggplot2::ggtitle("metabolite_median vs analysis order")+
    ggplot2::theme(plot.title = element_text(hjust = 0.5))


  if( is_sample){

  ggsave(paste(df_name, "sample_median_vs_running_order.pdf", sep=" ") )

  }else{
    ggsave(paste(df_name, "pp_median_vs_running_order.pdf", sep=" ") )

  }

}
