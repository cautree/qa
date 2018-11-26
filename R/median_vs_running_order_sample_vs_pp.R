#' A get_median_vs_running_order_comparision  function
#'
#' This function allows you to get median vs running order comparision between sample and pp
#' @param sample_meta_data, input data frame.
#' @param pp_meta_data, input data frame.
#' @param df_name, string for data set
#' @return a ggplot scatter graph
#' @export

get_median_vs_running_order_comparision = function(sample_meta_data, pp_meta_data, df_name="Vital") {


  rownames(sample_df) =sample_df$plate_well
  sample_df$plate_well= NULL

  df_sample <- df_sample[,colSums(is.na(df_sample))<nrow(df_sample)]

  row_median_s = as.data.frame( apply(df_sample, 1, median, na.rm = TRUE))

  names(row_median_s) ="row_median"

  row_median_df_s = row_median_s %>%
    dplyr::mutate(plate_well = rownames(.)) %>%
    tidyr::separate(plate_well, c("plate", "well")) %>%
    dplyr::mutate(order =1:dim(.)[1] )%>%
    dplyr::mutate(group = "sample")

  rownames(pp_df) = pp_df$plate_well
  pp_df$plate_well= NULL


  df_pp <- pp_df[,colSums(is.na(pp_df))<nrow(pp_df)]

  row_median_p = as.data.frame( apply(df_pp, 1, median, na.rm = TRUE))

  names(row_median_p) ="row_median"

  row_median_df_p = row_median_p %>%
    dplyr::mutate(plate_well = rownames(.)) %>%
    tidyr::separate(plate_well, c("plate", "well")) %>%
    dplyr::mutate(order =1:dim(.)[1] )%>%
    dplyr::mutate(group = "pp")


  median_df = rbind(row_median_df_s, row_median_df_p)


  median_df %>%
    ggplot2::ggplot(aes(order, row_median, color = plate, shape = group)) +
    ggplot2::geom_point()+
    ggplot2::ylab("metabolite_median") +
    ggplot2::ggtitle("metabolite_median vs analysis order")+
    ggplot2::theme(plot.title = element_text(hjust = 0.5))




  ggsave(paste(df_name, "sample_median_running_order_pp_median_running_order_comparision.pdf", sep=" ") )



}
